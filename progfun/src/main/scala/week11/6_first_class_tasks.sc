import scala.collection.parallel.Task
// omit writing.join -> create implicit conversion
//implicit def getJoin[T](x: Task[T]): T = x.join

def parallel[A, B](cA: => A, cB: => B): (A, B) = {
  val tB: Task[B] = task { cB}
  val tA: A = cA
  (tA, tB.join)
}