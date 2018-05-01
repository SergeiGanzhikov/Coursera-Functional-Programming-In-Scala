def sum2(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum2(f, a + 1, b)

//def sumInts(a: Int, b: Int) = sum2(x => x, a, b)
//def sumCubes(a: Int, b: Int) = sum2(x => x * x * x, a, b)

def sum3(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  }
  loop(a, 0)
}

//sum(x => x, 1, 5)
sum3(x => x, 1, 5)
sum3(x => x * x, 3, 5)

def sum(f: Int => Int)(a: Int, b: Int) : Int =
  if (a > b) 0 else f(a) + sum(f)(a + 1, b)

def sum4(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sumF(a + 1, b)
  }
  sumF
}

//def sumInts = sum(x => x)
//def sumCubes = sum(x => x * x * x)

//sumCubes(1, 10)

def cube(x: Int) = x * x * x

//sum4(cube)(1, 10))

sum(cube)(1, 10)


//def product(f: Int => Int)(a: Int, b: Int): Int =
//  if (a > b) 1 else f(a) * product(f)(a + 1, b)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, unit: Int)(a: Int, b: Int): Int =
  if (a > b) unit else combine(f(a), mapReduce(f, combine, unit)(a + 1, b))

def product(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x * y, 1)(a, b)

def factorial(n: Int): Int = product(x => x)(1, n)

factorial(5)

product(x => x)(1, 5)
product(x => x * x)(3, 4)
