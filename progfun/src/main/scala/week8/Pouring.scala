package week8

class Pouring(capacity: Vector[Int]) {

  // States
  type State = Vector[Int]
  val initialSate = capacity map (_ => 0)

  // Moves
  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    def change(state: State): State = state updated (glass, 0)
  }

  case class Fill(glass: Int) extends Move {
    def change(state: State): State = state updated (glass, capacity(glass))
  }

  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State): State = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated (from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  val glasses = 0 until capacity.length

  val moves =
    (for (glass <- glasses) yield Empty(glass)) ++
      (for (glass <- glasses) yield Fill(glass)) ++
      (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  // Paths
  // Last move comes first in the list
  class Path(history: List[Move], val endState: State) {
    def extend(move: Move) = new Path(move :: history, move change endState)
    override def toString: String = (history.reverse mkString " ") + "--> " + endState
  }

  val initialPath = new Path(Nil, initialSate)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] = {
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }
  }

  val pathSets = from(Set(initialPath), Set(initialSate))

  def solutions(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}
