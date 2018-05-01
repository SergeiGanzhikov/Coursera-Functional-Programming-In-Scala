val test = List(1, 2, 3, 4, 5, 6, 7, 8 , 9, 10)
test.reduceLeft(_ + _)

def sum(list: List[Int]) = list reduceLeft(_ + _)
sum(test)

test.foldLeft(0)(_ + _)

