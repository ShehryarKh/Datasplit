/*
 * Project 1 
 * 
 * authors: Yan Zhen Lin, Shehryar Khan
 * 
 */

package DataSplit

import scala.actors.Actor
import scala.actors.Actor._
import scala.io.Source
import java.io.FileNotFoundException
import scala.collection.mutable._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

//the controller takes in pass in values from FileHandle, specifically the filename, the top k value and the processor value
class Controller(K:Int, Processors:Int, FileName:String ){
  val caller = self //self thread
  val k = K
  val processorNum = Processors
  val fileName = FileName
  var pt = new Hypogeometric() 
  var pb = new MergeSort()
  var N =0
  var kList = new Array[String](this.k) //this array will represent the top k points
  var uniqueMap = Map[String, Int]() //this map represents a unique set of category and the number of counts
  var percentageMap = ListBuffer[Task]() //this map will hold the hypergeometric calculations will hold
  var doubleList = ListBuffer[Double]() //this is a sorted list keeping values from the merge sort 
  var finalResult = ListBuffer[Task]() //the final result container
  
  //create a set of actors base on the number of processors 
  val readers = (0 to processorNum-1 ) map (new Reader(_, k))
  for (reader <- readers){
    reader.start //start the actors
  }
  
  var currentID = 0
  //read the file using source
  try{
    val file = Source.fromFile(FileName)
    println("file:"+FileName+"found")
    //read the line one by one
    for(line <- file.getLines() ){
      //get the numeric value
      val priority = this.getPriority(line)
      //pass in to the actors that we started before alternate lines
      currentID = N%readers.length //using modulo base off modulo of processor number
      readers(currentID)!(priority, line)
      N+=1
    }
  }catch{
    case ex: FileNotFoundException =>{
      println("Missing file exception")
    }
  }
  
  //retrieve the computed data from the actors
  for(reader <- readers){
    //get back the unique map from each of the actors
    reader!"getMap" 
    receive{
      //case x:Map[String, Int] => this.printMap(x)
      case x:Map[String, Int] => this.setMap(x)
    }
    //get back the largest k from each of the actors
    reader!"getData"
    receive{
      case y:Array[String] => this.setKList(y)
      //case y:Array[String] => this.printHeap(y)
    }
    reader!"exit"
  }
  
  //finding unique map category as well as the geometric values
  this.categoryMap()
  
  var doubleL = this.doubleList.toList
  //println("validating:"+doubleL.length)
  var mergeList = pb.mergeSort(doubleL)
  
  //merge sort process
  val mergeIt = mergeList.iterator
  while(mergeIt.hasNext){
    val mer = mergeIt.next()
    val targetCategory = this.find(mer)
    this.finalResult+=Task(mer,targetCategory)
  }
  
  //print the final result
  var finalIt = this.finalResult.reverseIterator
  println("Final Result")
  while(finalIt.hasNext){
    val finalItem = finalIt.next()
    println(finalItem.prio+" "+finalItem.data);
  }
  
  //find the matching target category for the computed geometric value
  def find(target:Double):String={
    val pIt = percentageMap.iterator
    var ret = ""
    while(pIt.hasNext){
      val Pitem = pIt.next()
      if(Pitem.priority == target){
        ret = Pitem.data
        pIt.drop(pIt.length+1)
      }
    }
    return this.getCategory(ret)
  }
  
  
  //println( " p = ", pt.upperbound(11, 8, 7, 7)/pt.lowerbound(11, 7))
  
  def categoryMap(){
    var it = this.uniqueMap.iterator
    while(it.hasNext){
      var newCategory = it.next()
      var currentcat = newCategory._1
      var BigK = newCategory._2
      //println("current category = "+currentcat+" big K = "+BigK );
      var little_k = this.findSuccessInTopK( currentcat)
      //N = this.N
      //K = BigK
      //n = this.k
      //k = little_k
      val P_value = pt.upperbound(this.N, BigK, this.k, little_k)/pt.lowerbound(this.N, this.k)
      this.doubleList+= P_value
      this.percentageMap+=Task(P_value, currentcat)
    }
  }
  
  //find number of successful hits within the top k list
  def findSuccessInTopK(currentCat:String ):Int={
    var sum = 0;
    //var kList = new Array[String](this.k)
    for(i <- 0 to this.k-1){
      if( this.getCategory(this.kList(i)) == currentCat ){
        sum += 1
      }
    }
    return sum
  }
  
  def printMap(){
    var it = this.uniqueMap.iterator
    println("printing map")
    while(it.hasNext){
      var cur = it.next()
      println(cur._1+" "+cur._2);
    }
  }
  
  //set the category and its number of total occurence within the N data set
  def setMap(newMap:Map[String, Int]){
    var mapIterator = newMap.iterator
    while(mapIterator.hasNext){
      var cur = mapIterator.next()
      if( this.uniqueMap.contains(cur._1) ){ //if it contains
        this.uniqueMap(cur._1) += cur._2.toInt
      } else{
        this.uniqueMap+=(cur._1 -> cur._2 )
      }
    }
  }
  
  //we are comparing two array list, and taking the largest out of them, comparing one index at a time
  //the loop will be a decrement loop
  def setKList(heap:Array[String]){
    var tempList = new Array[String](this.k) //
    var heapIndex = this.k-1;  
    var kListIndex = this.k-1;  
    for(i <-this.k-1 to 0 by -1){ //working backwards
      if( this.kList(kListIndex) == null ){ //if the kList is empty
        tempList(i) = heap(heapIndex) //set to temporary list 
        heapIndex -= 1
        kListIndex -= 1
      }else{ //compare the current heap list and the pass in array
        if(this.getPriority(this.kList(kListIndex)) >= this.getPriority(heap(heapIndex))){
          tempList(i) = this.kList(kListIndex)
          kListIndex -= 1 
        }else{
          tempList(i) = heap(heapIndex) // set onto the temporary list
          heapIndex -= 1
        }
      }
    }
    //copy from the temporary list onto the kList
    for(j <- 0 to this.k-1){
      this.kList(j) = tempList(j)
    }
  }
  
  //returns the category value of a data string
  def getCategory(line:String):String={
    var response = ""
    var stop = false
    var firstLetterFound = false 
    var cur = ' ';
    for(i <- 0 to line.length-1 if stop == false){
      cur = line.charAt(i)
      if(cur.isLetter){ //the first numeric has not been found
        response += cur
        firstLetterFound = true
      }else{ 
        if(firstLetterFound == true){
          stop = true
        }
      }
    }
    return response
  }
  
  //this function parses the floating value from each line
  def getPriority(line:String):Double={
    var response = 0.0
    var stop = false
    var firstNumericFound = false
    var dotFound = false
    
    var resultString = ""
    var cur = ' ';
    for(i <- 0 to line.length-1 if stop == false){
      cur = line.charAt(i)
      if(!firstNumericFound){ //the first numeric has not been found
        if(cur.isDigit){ //
          resultString += cur
          firstNumericFound = true
        }
      }else{ 
        if(cur.isDigit || cur == '.'){
          if(cur == '.'){
            if(dotFound == false) {
              resultString += cur
              dotFound = true
            }else{
              stop = true
            }
          }else{ //digit
            resultString += cur  
          }
        }else{
          stop = true
        }
      }
    }
    //println("result string = "+resultString)
    response = resultString.toDouble
    return response
  }
  
}