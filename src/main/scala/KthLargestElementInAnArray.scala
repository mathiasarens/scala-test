
object KthLargestElementInAnArray {
  def findKthLargest(nums: Array[Int], k: Int): Int = {
    val sortedList: List[Int] = nums.toList.sorted.reverse
    return sortedList(k-1)
  }

  def main(args: Array[String]): Unit = {
    println(findKthLargest(Array(3,2,1,5,6,4), 2))
  }
}