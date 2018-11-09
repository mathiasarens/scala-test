import scala.collection.mutable

object LilysHomework {
  // Complete the lilysHomework function below.
  def lilysHomework(arr: Array[Int]): Int = {
    // values -> index
    val valueToIndexMap = new scala.collection.mutable.HashMap[Int, Int]()
    for (i <- 0 until arr.length) {
      valueToIndexMap.put(arr(i), i)
    }

    val sortedArray = arr.sorted
    val ascendingSwaps = calculateNumberOfSteps(arr.clone(), sortedArray, valueToIndexMap.clone())
    val descendingSwaps = calculateNumberOfSteps(arr.clone(), sortedArray.reverse, valueToIndexMap.clone())
    scala.math.min(ascendingSwaps, descendingSwaps)
  }

  def calculateNumberOfSteps(arr: Array[Int], sortedArray: Array[Int], valueToIndexMap: scala.collection.mutable.HashMap[Int, Int]): Int = {
    var swaps = 0
    for (i <- 0 until arr.length) {
      if (arr(i) != sortedArray(i)) {
        swaps += 1
        valueToIndexMap(arr(i)) = valueToIndexMap(sortedArray(i))
        arr(valueToIndexMap(arr(i))) = arr(i)
      }
    }
    swaps
  }

  def main(args: Array[String]): Unit = {
    println(lilysHomework(Array(7, 15, 12, 3)))
    println(lilysHomework(Array(15, 10, 1, 3)))
    println(lilysHomework(Array(1)))
    println(lilysHomework(Array(1,2)))
    println(lilysHomework(Array(2,1)))
  }
}
