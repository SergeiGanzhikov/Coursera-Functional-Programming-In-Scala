val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

capitalOfCountry("US")
capitalOfCountry get "Russia"
capitalOfCountry get "US"

def showCapital(country: String) = capitalOfCountry.get(country) match {
  case Some(capital) => capital
  case None => "missing data"
}

showCapital("Russia")
showCapital("US")


class Polynom(val terms: Map[Int, Double]) {
  def + (other: Polynom) = new Polynom(terms ++ (other.terms map adjust))

  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exponent, coefficient) = term
    terms get exponent match {
      case Some(existing) => exponent -> (existing + coefficient)
      case None => exponent -> coefficient
    }
  }

  override def toString =
    (for ((exponent, coefficient) <- terms.toList.sorted.reverse)
      yield coefficient + "x^" + exponent) mkString " + "
}

val p1 = new Polynom(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Polynom(Map(0 -> 3.0, 3 -> 7.0))
p1 + p2

val cap1 = capitalOfCountry withDefaultValue "<unknown>"
cap1("Andorra")
