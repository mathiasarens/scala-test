

/**
  * Definition for an interval.
  * class Interval(var _start: Int = 0, var _end: Int = 0) {
  *   var start: Int = _start
  *   var end: Int = _end
  * }
  */

class Interval(var _start: Int = 0, var _end: Int = 0) {
  var start: Int = _start
  var end: Int = _end
}

object MergeIntervals {
  def merge(intervals: List[Interval]): List[Interval] = {
    val sortedIntervals = intervals.sortBy(_._start)
    val resultList = new scala.collection.mutable.ListBuffer[Interval]()
    for (sortedInterval <- sortedIntervals) {
        if (resultList.isEmpty || resultList.last._end < sortedInterval._start) {
          resultList.append(sortedInterval)
        } else {
          val interval = new Interval(resultList.last._start, scala.math.max(sortedInterval._end, resultList.last._end))
          resultList.remove(resultList.length-1)
          resultList.append(interval)
        }
    }
    resultList.toList
  }

  def main(args: Array[String]): Unit = {
    merge(List(new Interval(1, 3), new Interval(2, 6), new Interval(8, 10), new Interval(15, 18))).foreach(i=>print(s"[${i._start}, ${i._end}] ")); println()
    merge(List(new Interval(1, 3), new Interval(2, 15), new Interval(8, 10), new Interval(15, 18))).foreach(i=>print(s"[${i._start}, ${i._end}] ")); println()
    merge(List(new Interval(1, 1), new Interval(2, 6), new Interval(8, 10), new Interval(15, 18))).foreach(i=>print(s"[${i._start}, ${i._end}] ")); println()
    merge(List(new Interval(1, 28), new Interval(2, 6), new Interval(8, 10), new Interval(15, 18))).foreach(i=>print(s"[${i._start}, ${i._end}] ")); println()
    merge(List(new Interval(1, 28))).foreach(i=>print(s"[${i._start}, ${i._end}] ")); println()
    merge(List(new Interval(1, 1))).foreach(i=>print(s"[${i._start}, ${i._end}] ")); println()
    merge(List()).foreach(i=>print(s"[${i._start}, ${i._end}] ")); println()
  }

}