package DataSplit

import scala.actors.Actor
import scala.actors.Actor._
import scala.util.control.Breaks._
import scala.collection.mutable._
import scala.collection.mutable.Map

/*a concurrent actor that will insert the data into a priorityQueue and 
 * return back the largest k items as well as the unique categories it has found*/
class Reader(Id:Int, ksize:Int) extends Actor{
  var id = Id;
  var k = ksize;
  var counter = 0;
  
  var heap = new PriorityHeap() //this is our max heap
  var myMap = Map[String, Int]()
  var kList = new Array[String](this.k)
  
  //listen for messages
  def act = loop{
    receive{
      case (x:Double, y:String) => dataHandle(x, y);
      case "exit" =>this.escape()
      case "getData" =>sender!this.returnData() //return the k largest data values
      case "getMap" =>sender!this.returnMap() // return the unique category and their counters
      case _=> println("nothing recieved");
    }
  }
  
  //stop the actor
  def escape(){
    println("exit signal recieved");
    exit();
  }
  
  def setkLargest(){
    var diff = this.heap.size - this.k
    while(diff > 0){
      this.heap.dequeue
      diff -= 1
    }
    
    for(i <- 0 to this.k-1){
      val newMax = this.heap.dequeue
      this.kList(i) = newMax.data
    }
  }
  
  /*
  def returnData():PriorityHeap={
    return this.heap
  }*/
  
  def returnData():Array[String]={
    this.setkLargest()
    return this.kList
  }
  
  /*def returnMap():Iterator[(String,Int)]={
    return this.myMap.iterator
  }*/
  
  def returnMap():Map[String,Int]={
    return this.myMap
  }
  
  //this function handles the data passin from the sender
  def dataHandle(x:Double, y:String){
    //step 1, put everything into the heap
    this.heap.+=(Task(x,y))
    //step 2, get back k elements
    var category = this.getCategory(y)
    this.update(category) //update the map
  }
  
  //update the category list that has been found in the data passed in to this actor
  def update(category:String ){
    if(this.myMap.isEmpty){ //if the length of the unique list is empty
        //(category, 1)::this.list
        this.myMap+=(category -> 1)
    }else{
      var found = false
      //check if the category is in the map
      if(this.myMap.contains(category)){
        this.myMap(category) += 1
      }else{ //if not found append new map category
        this.myMap+=(category -> 1)
      }
    }
  }
  
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
    //println("category response ="+response)
    return response
  }
  
}