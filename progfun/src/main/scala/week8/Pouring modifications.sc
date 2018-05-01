// Paths
// Last move comes first in the list
/*
class Path(history: List[Move]) {
  def endState: State = trackState(history)

  private def trackState(moves: List[Move]): State = moves match {
    case Nil => initialSate
    case move :: previousMoves => move change trackState(previousMoves)

  def extend(move: Move) = new Path(move :: history)

  override def toString: String = (history.reverse mkString " ") + "--> " + endState
}
*/