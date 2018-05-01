class Polynom(inputTerms: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = inputTerms withDefaultValue(0.0)

  def + (other: Polynom) = new Polynom((other.terms foldLeft terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exponent, coefficient) = term
    terms + (exponent -> (coefficient + terms(exponent)))
  }

  override def toString =
    (for ((exponent, coefficient) <- terms.toList.sorted.reverse)
      yield coefficient + "x^" + exponent) mkString " + "
}

val p1 = new Polynom(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new Polynom(0 -> 3.0, 3 -> 7.0)
p1 + p2
p1.terms(7)