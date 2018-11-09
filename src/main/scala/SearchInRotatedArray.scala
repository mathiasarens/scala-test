object SearchInRotatedArray {

  def search(nums: Array[Int], target: Int): Int = {
    if (nums.length > 0) {
      searchRecursive(nums, target, 0, nums.length)
    } else {
      -1
    }
  }

  def searchRecursive(nums: Array[Int], target: Int, lowerBound: Int, upperBound: Int): Int = {
    if (lowerBound >= upperBound - 1) {
      if (nums(lowerBound) == target) {
        lowerBound
      } else {
        -1
      }
    } else {
      val middleIndex = lowerBound + (upperBound - lowerBound) / 2
      if (nums(middleIndex) == target) {
        middleIndex
      } else if ((target >= nums(0) && nums(middleIndex) >= nums(0)) || (target < nums(0) && nums(middleIndex) < nums(0))) {
        if (nums(middleIndex) < target) {
          searchRecursive(nums, target, middleIndex, upperBound)
        } else {
          searchRecursive(nums, target, lowerBound, middleIndex)
        }
      } else if (target >= nums(0) && nums(middleIndex) < nums(0)) {
        searchRecursive(nums, target, lowerBound, middleIndex)
      } else if (target < nums(0) && nums(middleIndex) >= nums(0)) {
        searchRecursive(nums, target, middleIndex, upperBound)
      } else {
        throw new RuntimeException()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(search(Array(4, 5, 6, 7, 0, 1, 2), 0))
    println(search(Array(4, 5, 6, 7, 0, 1, 2), 7))
    println(search(Array(4, 5, 6, 7, 0, 1, 2), 4))
    println(search(Array(4, 5, 6, 7, 0, 1, 2), 2))
    println(search(Array(4, 5, 6, 7, 0, 1, 2), 3))
    println(search(Array(), 2))
    println(search(Array(1), 2))
    println(search(Array(1), 1))
  }
}