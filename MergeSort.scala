package DataSplit
// merge sort  merges a list of numbers
class MergeSort {
def mergeSort(xs: List[Double]): List[Double] = {
  val n = xs.length / 2
  //println("n value = "+n)
  if (n == 0) xs
  else {
    def merge(xs: List[Double], ys: List[Double], accu: List[Double]): List[Double] =
      (xs, ys) match {
        case (Nil, Nil) => accu.reverse
        case (Nil, y :: ys1) => merge(xs, ys1, y :: accu)
        case (x :: xs1, Nil) => merge(xs1, ys, x :: accu)
        case (x :: xs1, y :: ys1) =>
          if (x < y) merge(xs1, ys, x :: accu)
          else merge(xs, ys1, y :: accu)
      }
    val (left, right) = xs splitAt (n)
    merge(mergeSort(left), mergeSort(right), List())
  }
}
}