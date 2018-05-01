package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 || r == 1) 1
    else if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def isOpen(c: Char) = c == '('

    def isClose(c: Char) = c == ')'

    def loop(chars: List[Char], stack: List[Char]): Boolean = {
      if (chars.isEmpty) stack.isEmpty
      else if (isOpen(chars.head)) loop(chars.tail, chars.head :: stack)
      else if (isClose(chars.head)) {
        !stack.isEmpty && isOpen(stack.head) && loop(chars.tail, stack.tail)
      }
      else loop(chars.tail, stack)
    }

    loop(chars, Nil)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(amount: Int, numCoins: Int) : Int =
      if (amount == 0) 1
      else if (amount < 0) 0
      else if (numCoins <= 0 && amount >= 1) 0
      else count (amount, numCoins - 1) + count(amount - coins(numCoins - 1), numCoins)
    count(money, coins.length)
  }
}
