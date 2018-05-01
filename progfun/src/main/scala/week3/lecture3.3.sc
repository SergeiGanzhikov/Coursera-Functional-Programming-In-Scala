import scala1.week3.{Cons, Nil}

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

singleton[Int](1)
singleton[Boolean](true)

singleton(1)
singleton(true)

def getNth[T](n: Int, list: List[T]): T = {
  if (list.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) list.head
  else getNth(n - 1, list.tail)
}

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
getNth(2, list)
getNth(4, list)
getNth(-1, list)