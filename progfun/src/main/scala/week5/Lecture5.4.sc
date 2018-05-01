def scaleList(list: List[Double], factor: Double): List[Double] = list match {
  case Nil => list
  case head :: tail => head * factor :: scaleList(tail, factor)
}

val nums = List(2, -4, 5, 7, 1)
val fruits = List("apple", "pineapple", "orange", "banana")

nums.filter(x => x > 0)
nums filterNot(x => x > 0)
nums partition(x => x > 0)

nums.takeWhile(x => x > 0)
nums.dropWhile(x => x > 0)
nums.span(x => x > 0)

def pack[T](list: List[T]): List[List[T]] = list match {
  case Nil => Nil
  case head :: _ =>
    val (prefix, rest) = list.span(x => x == head)
    prefix :: pack(rest)
}

def encode[T](list: List[T]): List[(T, Int)] =
  pack(list).map(sublist => (sublist.head, sublist.length))

val testList = List("a", "a", "a", "b", "c", "c", "a")
pack(testList)
encode(testList)
