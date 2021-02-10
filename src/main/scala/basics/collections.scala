package basics

// All this tasks from Leetcode I guess I'll call it "Legend of FAANG"

object collections {

  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.scanLeft(0)(_ + _).drop(1)
  }

  // https://leetcode.com/problems/shuffle-the-array
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    nums.zipWithIndex.sortBy{case (_, i) => i % n}.map(_._1)
  }

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(_.sum).max
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    candies.map(_ + extraCandies >= candies.max)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    points
      .map(arr => arr(0))
      .sortWith(_ < _)
      .foldLeft((0, Option.empty[Int])) {(tup, value) =>
        (math.max(tup._1, tup._2.map(x => value - x).getOrElse(0)),
          Option(value)
        )
      }._1
  }

  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  def maxDepth(s: String): Int = {
    s.toList.map(c => if (c == '(') 1 else if (c == ')') -1 else 0).scanLeft(0)(_+_).drop(1).max
  }

  // https://leetcode.com/problems/split-a-string-in-balanced-strings
  def balancedStringSplit(s: String): Int =
    s.foldLeft((0, 0))((acc, ch) => acc match {
      case (1, count) if ch == 'L' => (0, count + 1)
      case (-1, count) if ch == 'R' => (0, count + 1)
      case (flag, count) if ch == 'R' => (flag + 1, count)
      case (flag, count) if ch == 'L' => (flag - 1, count)
    })._2

  // https://leetcode.com/problems/matrix-block-sum/
  def matrixBlockSum(a: Array[Array[Int]], k: Int): Array[Array[Int]] = {
    if(k == 0) return a
    if(a.isEmpty) return a
    val res = Array.ofDim[Int](a.length, a(0).length)
    def getA(i: Int, j: Int) = if(i < 0 || j < 0 || i >= a.length || j >= a(i).length) 0 else a(i)(j)
    def getRes(i: Int, j: Int): Int = {
      if(i < 0 || j < 0) return 0
      val ii = if(i < a.length) i else a.length -1
      val jj = if(j < a(ii).length) j else a(ii).length - 1
      a(ii)(jj)
    }
    for(i <- a.indices; j <- a(i).indices) a(i)(j) += getA(i, j-1)
    for(i <- a.indices; j <- a(i).indices) a(i)(j) += getA(i-1, j)
    for(i <- a.indices; j <- a(i).indices){
      res(i)(j) = getRes(i+k, j+k) - getRes(i+k, j-k-1) - getRes(i-k-1, j+k) + getRes(i-k-1, j-k-1)
    }
    res
  }
}
