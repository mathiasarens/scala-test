object FindPeakElement {
  val random = new scala.util.Random()
  def findPeakElement(nums: Array[Int]): Int = {
    if (nums.length > 0) {
      findPeakRecrusive(nums, 0, nums.length)
    } else {
      0
    }
  }

  def findPeakRecrusive(nums:Array[Int], lowerIndex:Int, upperIndex:Int):Int = {
    if (lowerIndex >= upperIndex-1) {
      lowerIndex
    } else {
      val randomIndex:Int = random.nextInt(upperIndex - lowerIndex) + lowerIndex
      if (randomIndex > 0 && randomIndex < nums.length-1) {
        val indexValue = nums(randomIndex)
        val left = nums(randomIndex-1)
        val right = nums(randomIndex+1)
        if (left < indexValue && indexValue > right) {
          //peak
          randomIndex
        } else if(left < indexValue && indexValue < right) {
          findPeakRecrusive(nums, randomIndex, upperIndex)
        } else if(left > indexValue && indexValue > right){
          findPeakRecrusive(nums, lowerIndex, randomIndex)
        } else {
          // found low // choose one
          findPeakRecrusive(nums, lowerIndex, randomIndex)
        }
      } else if (randomIndex==0) {
        if (nums(0) < nums(randomIndex+1)) {
          findPeakRecrusive(nums, randomIndex, upperIndex)
        } else {
          0
        }
      } else {
        if (nums(randomIndex-1) > nums(randomIndex)) {
          findPeakRecrusive(nums, lowerIndex, randomIndex)
        } else {
          randomIndex
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(findPeakElement(Array(1,2,3,1)))
    println(findPeakElement(Array(1,2,1,3,5,6,4)))
    println(findPeakElement(Array(1,2)))
    println(findPeakElement(Array(2,1)))
    println(findPeakElement(Array(1)))
    println(findPeakElement(Array()))
  }
}
