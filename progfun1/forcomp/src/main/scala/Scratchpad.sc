import forcomp.loadDictionary

List("Every", "student", "likes", "Scala").groupBy(element => element.length)
List(0, 1, 2, 1, 0).groupBy(element => element)

type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]

def wordOccurrences(w: Word): Occurrences =
  w.toLowerCase.groupBy(char => char).map{case (char, repetitions) => (char, repetitions.length)}.toList.sorted

def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s mkString)

var test = List("I", "love", "you")

sentenceOccurrences(test)

val dictionary: List[Word] = loadDictionary

lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
  dictionary groupBy (word => wordOccurrences(word)) withDefaultValue List()

def wordAnagrams(word: Word): List[Word] =
  dictionaryByOccurrences.getOrElse(wordOccurrences(word), List[Word]())

wordAnagrams("eat")

def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
  case Nil => List(Nil)
  case (char, n) :: tail => {
    val subsets = combinations(tail)
    (for {count <- 1 to n; set <- subsets}
      yield (char, count) :: set)
      .toList ++ subsets
  }
}

combinations(List(('a', 2), ('b', 2)))

/** Subtracts occurrence list `y` from occurrence list `x`.
  *
  *  The precondition is that the occurrence list `y` is a subset of
  *  the occurrence list `x` -- any character appearing in `y` must
  *  appear in `x`, and its frequency in `y` must be smaller or equal
  *  than its frequency in `x`.
  *
  *  Note: the resulting value is an occurrence - meaning it is sorted
  *  and has no zero-entries.
  */
// type Occurrences = List[(Char, Int)]
def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  y.foldLeft(x.toMap) { case (map, (char, count)) => {
    val diff = map(char) - count
    if (diff == 0) map - char
    else map updated (char, diff)
  } }.toList
}

val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
val y = List(('r', 1))
subtract(x, y)

//def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
//  def iterate(occurrences: Occurrences): List[Sentence] = {
//    if (occurrences.isEmpty) List(Nil)
//    else for {
//      occurrence <- combinations(occurrences)
//      word <- dictionaryByOccurrences(occurrence)
//      sentence <- iterate(subtract(occurrences, wordOccurrences(word)))
//    } yield word :: sentence
//  }
//  iterate(sentenceOccurrences(sentence))
//}

def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  if (sentence.isEmpty) List(List())
  else {
    def occurrenceAnagrams(occurrences: Occurrences): List[Sentence] =
      if (occurrences.isEmpty) List(List())
      else {
        for {
          occurrence <- combinations(occurrences)
          word <- dictionaryByOccurrences(occurrence)
          sentence <- occurrenceAnagrams(subtract(occurrences, occurrence))
        } yield word :: sentence
      }
    occurrenceAnagrams(sentenceOccurrences(sentence))
  }
}

sentenceAnagrams(List("I", "love", "you"))