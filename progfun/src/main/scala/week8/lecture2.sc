(1 to 1000).toStream

def streamRange(low: Int, high: Int): Stream[Int] =
  if (low >= high) Stream.empty
  else Stream.cons(low, streamRange(low + 1, high))

streamRange(1, 10).take(3).toList

