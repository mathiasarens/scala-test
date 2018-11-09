import scala.collection.mutable

object WordSearch {
  def exist(board: Array[Array[Char]], word: String): Boolean = {
    var result = false
    if (board != null && board.length > 0 && board(0).length>0) {
      for (i <- board.indices) {
        for (j <- board(0).indices) {
          if (word.startsWith(String.valueOf(board(i)(j)))) {
            val visited = new scala.collection.mutable.HashSet[(Int, Int)]()
            result |= search(board, visited, word, i, j)
          }
        }
      }
    }
    result
  }

  def search(board: Array[Array[Char]], visited: scala.collection.mutable.Set[(Int, Int)], word: String, row: Int, column: Int): Boolean = {
      visited.add((row,column))
      if (word.length == 1) {
        word == String.valueOf(board(row)(column))
      } else if (word(0) == board(row)(column)) {
        var result = false
        for ((x, y) <- Array((row - 1, column), (row, column + 1), (row + 1, column), (row, column - 1))) {
          if (x >= 0 && x < board.length && y >= 0 && y < board(0).length && !visited.contains((x,y))) {
            if (search(board, visited, word.substring(1), x, y)) {
              result = true
            } else {
              visited.remove((x,y))
            }
          }
        }
        result
      } else {
        false
      }
  }

  def main(args: Array[String]): Unit = {
    val array1 = Array(Array('A', 'B', 'C', 'E'), Array('S', 'F', 'C', 'S'), Array('A', 'D', 'E', 'E'))
    val array2 = Array(Array('C'))
    val array3 = Array(Array('a', 'b'), Array('c', 'd'))
    val array4 = Array(Array('A','B','C','E'), Array('S','F','E','S'), Array('A','D','E','E'))
    println(exist(array1, "ABCCED"))
    println(exist(array1, "SEE"))
    println(exist(array1, "ABCB"))
    println(exist(array2, "C"))
    println(exist(array2, "D"))
    println(exist(array3, "acdb"))
    println(exist(array3, "acdba"))
    println(exist(array4, "ABCEFSADEESE"))
  }
}
