/*
 * Project 1 
 * authors: Yan Zhen Lin, Shehryar Khan
 */

package DataSplit

/*main file for taking in a command line argument of the test 
 * file as well as a standard inupt retrieval of the k value. 
 * We also define the number of processors in the main function as well 
 * All of these parameters are passed to the Controller which will control
 *  the main process flow
 * */

object FileHandle{
  
  def main( args: Array[String]){
    
    println("Please enter the size of k, representing the top k numbers");
    var k_str = Console.readLine //representing the top k points
    val k = k_str.toInt
    
    val processors = 2
    
    println("validation")
    val controller = new Controller(k, processors, args(0));
    
  }
  
}