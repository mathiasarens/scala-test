
class RecentCounter() {
  private val map = scala.collection.mutable.TreeMap[Int,Int]()
  private var lastTime = 0
  def ping(t: Int): Int = {
    map.put(t, map.getOrElse(lastTime, 0)+1)
    var sum = map(t)
    if (map.to(t-3000).lastOption.nonEmpty) {
      sum -= map.to(t-3000).last._2
    }
    lastTime = t
    sum
  }

}

object NumberOfRecentCalls {
  def main(args: Array[String]): Unit = {
    val recentCounter = new RecentCounter()
    println(recentCounter.ping(1))
    println(recentCounter.ping(100))
      println(recentCounter.ping(3001))
      println(recentCounter.ping(3002))
  }
}
