val nums = Vector(1, 2, 3, -88)
val people = Vector("Bob", "James", "Peter")

val array = Array(1, 2, 3, 44)
array map (x => x * 2)

val string = "Hello World"
string filter (c => c.isUpper)
string exists (c => c.isUpper)
string forall (c => c.isUpper)

val r = 1 until 5
val s = 1 to 5

1 to 10 by 3

val pairs = List(1, 2, 3) zip string
pairs unzip

string flatMap(c => List('.', c))

val N = 10
val M = 5

def isPrime(num: Int): Boolean = (2 until num).forall(d => num % d != 0)
isPrime(10)
isPrime(11)