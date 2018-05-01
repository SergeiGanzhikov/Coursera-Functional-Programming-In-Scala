abstract class CodeTree

case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

case class Leaf(char: Char, weight: Int) extends CodeTree

def string2Chars(str: String): List[Char] = str.toList

def times(chars: List[Char]): List[(Char, Int)] = chars.groupBy(identity).mapValues(_.length).toList

def sortListOfPairs(freqs: List[(Char, Int)]): List[(Char, Int)] = freqs.sortBy(- _._2)

def weight(tree: CodeTree): Int = tree match {
  case Fork(_, _, _, weight) => weight
  case Leaf(_, weight) => weight
}

def chars(tree: CodeTree): List[Char] = tree match {
  case Fork(_, _, chars, _) => chars
  case Leaf(char, _) => List(char)
}

def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

def makeCodeTree(left: CodeTree, right: CodeTree) =
  Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

// A = 5, B = 2, R = 2, C = 1, D = 1
val test = string2Chars("ABRACADABRA")
val freqs = times(test)
val sorted = sortListOfPairs(freqs)

def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
  freqs.sortBy(_._2).map(p => Leaf(p._1, p._2))

var leafList = makeOrderedLeafList(freqs)

def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
  case Nil => trees
  case _ :: Nil => trees
  case first :: second :: tail => makeCodeTree(first, second) :: tail
}

def until(singleton: List[CodeTree] => Boolean, combine: List[CodeTree] => List[CodeTree])
         (trees: List[CodeTree]): List[CodeTree] =
  if (singleton(trees)) trees
  else until(singleton, combine)(combine(trees))

val combined = until(singleton, combine)(leafList)

/**
  * This function creates a code tree which is optimal to encode the text `chars`.
  *
  * The parameter `chars` is an arbitrary text. This function extracts the character
  * frequencies from that text and creates a code tree based on them.
  */
def createCodeTree(chars: List[Char]): CodeTree = {
  val leafList = makeOrderedLeafList(times(chars))
  val combined = until(singleton, combine)(leafList)
  combined.head
}

type Bit = Int

/**
  * This function decodes the bit sequence `bits` using the code tree `tree` and returns
  * the resulting list of characters.
  */
def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
  def traverse(rest: CodeTree, bits: List[Bit]): List[Char] = rest match {
    case Leaf(char, _) =>
      if (bits.isEmpty) List(char)
      else char :: traverse(tree, bits)
    case Fork(left, right, _, _) =>
      if (bits.head == 0) traverse(left, bits.tail)
      else traverse(right, bits.tail)
  }
  traverse(tree, bits)
}

/**
  * A Huffman coding tree for the French language.
  * Generated from the data given at
  * http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
  */
val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

/**
  * What does the secret message say? Can you decode it?
  * For the decoding use the `frenchCode' Huffman tree defined above.
  **/
val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

val message = decode(frenchCode, secret)