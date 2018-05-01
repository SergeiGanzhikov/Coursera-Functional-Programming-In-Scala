case class Pos(row: Int, col: Int) {
  /** The position obtained by changing the `row` coordinate by `d` */
  def deltaRow(d: Int): Pos = copy(row = row + d)

  /** The position obtained by changing the `col` coordinate by `d` */
  def deltaCol(d: Int): Pos = copy(col = col + d)
}

/**
  * This method returns terrain function that represents the terrain
  * in `levelVector`. The vector contains parsed version of the `level`
  * string. For example, the following level
  *
  *   val level =
  *     """ST
  *       |oo
  *       |oo""".stripMargin
  *
  * is represented as
  *
  *   Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
  *
  * The resulting function should return `true` if the position `pos` is
  * a valid position (not a '-' character) inside the terrain described
  * by `levelVector`.
  */
def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean =
  pos =>
    pos.row > 0 && pos.row < levelVector.size &&
    pos.col > 0 && pos.col < levelVector(pos.row).size &&
    levelVector(pos.row)(pos.col) != '-'

/**
  * This function should return the position of character `c` in the
  * terrain described by `levelVector`. You can assume that the `c`
  * appears exactly once in the terrain.
  *
  * Hint: you can use the functions `indexWhere` and / or `indexOf` of the
  * `Vector` class
  */
def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
  val pos =
    for {
      row <- levelVector.indices.toStream
      col <- levelVector(row).indices
      if levelVector(row)(col) == c
    } yield Pos(row, col)
  pos.headOption.get
}

val test = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
findChar('S', test)

terrainFunction(test)

