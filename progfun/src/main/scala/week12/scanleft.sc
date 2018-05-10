def scanLeft[T](input: Array[T], initial: T, f: (T, T) => T, output: Array[T]): Unit = {
  output(0) = initial
  for (i <- 1 until output.length)
    output(i) = f(output(i - 1), input(i))
}

def scanLeft2[T](input: Array[T], initial: T, f: (T, T) => T, output: Array[T]): Unit = {
  output(0) = initial

  var a = initial
  var i = 0

  while (i < input.length) {
    a = f(a, input(i))
    i += 1
    output(i) = a
  }
}

sealed abstract class Tree[T]
case class Leaf[T](value: T) extends Tree[T]
case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]

sealed abstract class TreeRes[T] { val res: T }
case class LeafRes[T](override val res: T) extends TreeRes[T]
case class NodeRes[T](left: TreeRes[T], override val res: T, right: TreeRes[T]) extends TreeRes[T]

def reduceRes[T](tree: Tree[T], f: (T,T) => T): TreeRes[T] = tree match {
  case Leaf(value) => LeafRes(value)
  case Node(left, right) => {
    val (reducedLeft, reducedRight) = (reduceRes(left, f), reduceRes(right, f))
    NodeRes(reducedLeft, f(reducedLeft.res, reducedRight.res), reducedRight)
  }
}

def upsweep[T](tree: Tree[T], f: (T,T) => T): TreeRes[T] = tree match {
  case Leaf(value) => LeafRes(value)
  case Node(left, right) => {
    val (reducedLeft: TreeRes[T], reducedRight: TreeRes[T]) =
      parallel(upsweep(left, f), upsweep(right, f))
    NodeRes(reducedLeft, f(reducedLeft.res, reducedRight.res), reducedRight)
  }
}

def downsweep[T](tree: TreeRes[T], initial: T, f: (T,T) => T): Tree[T] = tree match {
  case LeafRes(value) => Leaf(f(initial, value))
  case NodeRes(left, _, right) => {
    val (reducedLeft, reducedRight) = parallel(
      downsweep(left, initial, f),
      downsweep(right, f(initial, left.res), f))
    Node(reducedLeft, reducedRight)
  }
}

def scanLeft[T](tree: Tree[T], initial: T, f: (T, T) => T): Tree[T] = {
  val treeRes = upsweep(tree, f)
  val scan = downsweep(treeRes, initial, f)
  prepend(initial, scan)
}

def prepend[T](element: T, tree: Tree[T]): Tree[T] = tree match {
  case Leaf(value) => Node(Leaf(element), Leaf(value))
  case Node(left, right) => Node(prepend(element, left), right)
}

val test = Node(
  Node(
    Leaf(1), Leaf(3)
  ),
  Node(
    Leaf(8), Leaf(50)
  )
)

val plus = (x: Int, y: Int) => x + y

val result = reduceRes(test, plus)
result
