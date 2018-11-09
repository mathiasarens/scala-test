object SearchA2DMatrixII {
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    if (matrix.length > 0 && matrix(0).length > 0) {
      var col = matrix(0).length-1
      var row = 0
      var result = false
      while(col>=0 && row < matrix.length && !result) {
        if (matrix(row)(col) == target) {
          result = true
        } else if (matrix(row)(col) > target) {
          col-=1
        } else if (matrix(row)(col) < target) {
          row+=1
        }
      }
      result
    } else {
      false
    }
  }

  def main(args: Array[String]): Unit = {
    val matrix = Array(Array(1, 4, 7, 11, 15), Array(2, 5, 8, 12, 19), Array(3, 6, 9, 16, 22),
      Array(10, 13, 14, 17, 24), Array(18, 21, 23, 26, 30))
    println(searchMatrix(matrix, 6))
    println(searchMatrix(matrix, 20))
    println(searchMatrix(matrix, 30))
    println(searchMatrix(Array(Array(1)), 1))
    println(searchMatrix(Array(Array()), 30))
    println(searchMatrix(Array(Array(1)), 2))
  }
}
