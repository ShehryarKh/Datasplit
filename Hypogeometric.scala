package DataSplit
//package com.alvinalexander.breakandcontinue
import util.control.Breaks._

class Hypogeometric {
      implicit def IntIntLessThan(x: Int, y: Int) = x < y
 
     // biconditional function tail recursively returns n! 
    def biconditional(n: Double, k: Double): Int = (n, k) match {
    case (n, 0) => 1
    case (0, k) => 0
    case (n, k) => biconditional(n - 1, k - 1) + biconditional(n - 1, k)
  }
    
      // calculates the "biconditional of the upper half of the hypergeometric equation
     def upperbound ( NumPopulation: Double, NumOfStates: Double, NumOfDraws: Double, NumOfSuccesses: Long): Double = {
      val A = biconditional(NumOfStates, NumOfSuccesses)
    val B = biconditional((NumPopulation-NumOfStates),(NumOfDraws-NumOfSuccesses))
    return A*B
     } 
      // calculates the "biconditional of the lower half of the hypergeometric equation
     def lowerbound(NumPopulation: Long, NumOfSuccesses: Long): Double={
       return biconditional(NumPopulation,NumOfSuccesses)
     }
    
   // takes in array of String (given small k) and figures out how many times char occur   
  def stringarray(){
    
   val pair = ("y", 42)
   println(pair._1)
   
   
  }
}
