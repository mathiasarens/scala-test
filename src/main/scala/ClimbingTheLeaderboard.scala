object ClimbingTheLeaderboard {

  // Complete the climbingLeaderboard function below.
  def climbingLeaderboard(scores: Array[Int], alice: Array[Int]): Array[Int] = {
    val scoresToRankMap = createScoresToRankMap(scores)
    var j = scores.length-1
    val result = Array.fill[Int](alice.length){0}
    for (i <- 0 until alice.length) {
        while (alice(i)>scores(j) && j > 0) {
          j-=1
        }
        if(alice(i) >= scores(j)) {
          result(i) = scoresToRankMap(scores(j))
        } else {
          result(i) = scoresToRankMap(scores(j))+1
        }
    }
    result
  }

  private def createScoresToRankMap(scores: Array[Int]): scala.collection.immutable.Map[Int, Int] = {
    val scoresToRankMap = scala.collection.mutable.Map[Int, Int]()
    var i = 1
    for (score <- scores) {
      if (scoresToRankMap.get(score).isEmpty) {
        scoresToRankMap.put(score, i)
        i += 1
      }
    }
    scoresToRankMap.toMap
  }

  def main(args: Array[String]): Unit = {
    climbingLeaderboard(Array(100, 90, 90, 80), Array(70, 80, 105)).foreach(result => print(s"$result "));println()
    climbingLeaderboard(Array(100, 100, 50, 40, 40, 20, 10), Array(5, 25, 50, 120)).foreach(result => print(s"$result "));println()
    climbingLeaderboard(Array(10), Array(120)).foreach(result => print(s"$result "));println()
    climbingLeaderboard(Array(1), Array(1)).foreach(result => print(s"$result "));println()
    climbingLeaderboard(Array(2), Array(1)).foreach(result => print(s"$result "));println()
  }
}
