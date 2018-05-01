import scala.io.Source

val in = Source.fromFile("linuxwords.txt")

val words = in.getLines.toList filter (word => word forall (char => char.isLetter))

val mnem = Map( '2' -> "ABC", '3' -> "DEF",
  '4' -> "GHI", '5' -> "JKL", '6' -> "MNO",
  '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

val charCode: Map[Char, Char] =
  for ((digit, string ) <- mnem; letter <- string) yield letter -> digit

def wordCode(word: String): String = word.toUpperCase map charCode

val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq()

def encode(number: String): Set[List[String]] =
  if (number.isEmpty) Set(List())
  else {
    for {
      split <- 1 to number.length
      word <- wordsForNum(number take split)
      rest <- encode(number drop split)
    } yield word :: rest
  }.toSet

def translate(number: String): Set[String] =
  encode(number) map (_ mkString " ")

translate("7225247386")