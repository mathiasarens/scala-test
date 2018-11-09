object MaximumPalindroms {
  private var input: String = ""
  private val factorialCache = scala.collection.mutable.HashMap[Long, Long]()
  private val modularInverseCache = scala.collection.mutable.HashMap[Long, Long]()

  // Complete the initialize function below.
  def initialize(s: String) {
    // This function is called once before all queries.
    input = s
    // prefill factorial cache
    factorialCache.put(0,1)
    for (i <- 1 until s.length+1) {
      factorialCache.put(i, (factorialCache(i-1) * i)%1000000007)
    }

    // prefill modular multiplicative inverse
    //int dp[n + 1];
    //dp[0] = dp[1] = 1;
    //for (int i = 2; i <= n; i++)
    //dp[i] = dp[prime % i] *
    //  (prime - prime / i) % prime;
  }

  // Complete the answerQuery function below.
  def answerQuery(l: Int, r: Int): Int = {
    // Return the answer for this query modulo 1000000007.
    val characterMap = scala.collection.mutable.HashMap[Char, Long]()

    // count characters
    for (i <- l - 1 to r - 1) {
      characterMap.put(input(i), characterMap.getOrElseUpdate(input(i), 0) + 1)
    }

    // count by 2 dividable number of characters because we need only the left side
    var numberOfOddValues = 0
    var numberOfCharactersOfTheLeft = 0L
    for ((character, count) <- characterMap) {
      if (count % 2 != 0) {
        numberOfOddValues += 1
      }
      numberOfCharactersOfTheLeft += count / 2
    }

    // divide by factorial of duplicate characters
    var permutations = factorial(numberOfCharactersOfTheLeft)
    for ((character, count) <- characterMap) {
      if (count / 2 > 1) {
        permutations = (permutations * calculateModularInverse(factorial(count / 2)))%1000000007
      }
    }

    if (numberOfOddValues > 0) {
      permutations = (permutations * numberOfOddValues) % 1000000007
    }
    (permutations % 1000000007).toInt
  }

  def calculateModularInverse(a: Long): Long = {
    val modularInverseOption = modularInverseCache.get(a)
    if (modularInverseOption.nonEmpty) {
      modularInverseOption.get
    } else {
      val prime = 1000000007
      val result = power(a, prime - 2, prime)
      modularInverseCache.put(a, result)
      result
    }
  }

  def power( x:Long, y:Int, m:Int):Long =
  {
    if (y == 0)
      return 1
    var p = power(x, y/2, m) % m
    p = (p * p) % m

    if (y%2 == 0) {
      p
    } else {
      (x * p) % m
    }
  }


  def factorial(num: Long): Long = {
    val factorialOption = factorialCache.get(num)
    if (factorialOption.nonEmpty) {
      factorialOption.get
    } else {
      val result = (1L to num).foldLeft(1L)((a, b) => (a * b) % 1000000007)
      factorialCache.put(num, result)
      result
    }
  }

  def main(args: Array[String]): Unit = {
    initialize("week")
    println(answerQuery(1, 4))
    println(answerQuery(2, 3))
  }
}
