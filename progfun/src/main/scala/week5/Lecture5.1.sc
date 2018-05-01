def last[T](list: List[T]): T = list match {
  case List() => throw new Error("last of empty list")
  case List(element) => element
  case head :: tail => last(tail)
}

def init[T](list: List[T]): List[T] = list match {
  case List() => throw new Error("init of empty list")
  case List(element) => List()
  case head :: tail => head :: init(tail)
}

def concat[T](left: List[T], right: List[T]): List[T] = left match {
  case List() => right
  case head :: tail => head :: concat(tail, right)
}

def reverse[T](list: List[T]): List[T] = list match {
  case List() => list
  case head :: tail => reverse(tail) ++ List(head)
}

def removeAt[T](list: List[T], n: Int): List[T] =
  list.take(n) ++ list.drop(n + 1)