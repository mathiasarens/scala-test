object JumpGame {
  def canJump(nums: Array[Int]): Boolean = {
    if (nums.length>0) {
      val dp = new Array[Int](nums.length)
      dp(0) = nums(0)
      for (i <- 1 until dp.length) {
        if (dp(i - 1) >= i) {
          dp(i) = scala.math.max(dp(i - 1), nums(i) + i)
        }
      }
      dp(nums.length - 1) >= nums.length - 1
    } else {
      false
    }
  }

  def main(args: Array[String]): Unit = {
    println(canJump(Array(2,3,1,1,4)))
    println(canJump(Array(3,2,1,0,4)))
    println(canJump(Array(0)))
    println(canJump(Array(0,1)))
    println(canJump(Array()))
  }
}
