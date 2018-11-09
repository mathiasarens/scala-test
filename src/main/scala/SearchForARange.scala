object SearchForARange {
  def searchRange(nums: Array[Int], target: Int): Array[Int] = {
    if (nums.length>0) {
      Array(searchLowerBorder(nums, target, 0, nums.length), searchUpperBorder(nums, target, 0, nums.length))
    } else {
      Array(-1,-1)
    }
  }

  def searchLowerBorder(nums: Array[Int], target: Int, lowerBound: Int, upperBound: Int): Int = {
    if (lowerBound >= upperBound-1) {
      if (nums(lowerBound) == target) {
        lowerBound
      } else {
        -1
      }
    } else {
      val middleIndex = lowerBound + (upperBound - lowerBound) / 2
      if (nums(middleIndex) < target) {
        searchLowerBorder(nums, target, middleIndex, upperBound)
      } else if (nums(middleIndex) > target) {
        searchLowerBorder(nums, target, lowerBound, middleIndex)
      } else {
        if (middleIndex > 0 && nums(middleIndex-1) != target) {
          middleIndex
        } else {
          searchLowerBorder(nums, target, lowerBound, middleIndex)
        }
      }
    }
  }

  def searchUpperBorder(nums: Array[Int], target: Int, lowerBound: Int, upperBound: Int): Int = {
    if (lowerBound >= upperBound-1) {
      if (nums(lowerBound) == target) {
        lowerBound
      } else {
        -1
      }
    } else {
      val middleIndex = lowerBound + (upperBound - lowerBound) / 2
      if (nums(middleIndex) > target) {
        searchUpperBorder(nums, target, lowerBound, middleIndex)
      } else if (nums(middleIndex) < target) {
        searchUpperBorder(nums, target, middleIndex, upperBound)
      } else {
        if (middleIndex < nums.length-1 && nums(middleIndex+1) != target) {
          middleIndex
        } else {
          searchUpperBorder(nums, target, middleIndex, upperBound)
        }
      }
    }
  }


  def main(args: Array[String]): Unit = {
    searchRange(Array(5, 7, 7, 8, 8, 10), 8).foreach(x => print(x + " ")); println()
    searchRange(Array(5, 7, 7, 8, 8, 10), 6).foreach(x => print(x + " ")); println()
    searchRange(Array(1), 1).foreach(x => print(x + " ")); println()
    searchRange(Array(0), 1).foreach(x => print(x + " ")); println()
    searchRange(Array(0), 0).foreach(x => print(x + " ")); println()
    searchRange(Array(1,1), 1).foreach(x => print(x + " ")); println()
  }
}
