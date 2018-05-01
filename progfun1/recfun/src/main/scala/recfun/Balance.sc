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

balance("()".toList)
balance("(if (zero? x) max (/ 1 x))".toList)
balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList)
!balance(":-)".toList)
!balance("())(".toList)

def countChange(money: Int, coins: List[Int]): Int = {
  def count(amount: Int, numCoins: Int) : Int =
    if (amount == 0) 1
    else if (amount < 0) 0
    else if (numCoins <= 0 && amount >= 1) 0
    else count (amount, numCoins - 1) + count(amount - coins(numCoins - 1), numCoins)
  count(money, coins.length)
}

countChange(4, List(1, 2, 3))
