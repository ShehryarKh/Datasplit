package DataSplit

//this class behaves as a tuple class that compares an internal double variable
case class Task(prio:Double, text:String) extends Ordered[Task] {
    val priority = prio
    val data = text
    def compare(that: Task)={
      that.priority compare this.priority
     }
}