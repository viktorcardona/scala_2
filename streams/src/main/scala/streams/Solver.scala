package streams

import common._

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
   * Returns `true` if the block `b` is at the final position
   *
   * origen: def done(b: Block): Boolean = ???
   */
  def done(b: Block): Boolean = b.isStanding && b.b1 == goal

  /**
   * This function takes two arguments: the current block `b` and
   * a list of moves `history` that was required to reach the
   * position of `b`.
   *
   * The `head` element of the `history` list is the latest move
   * that was executed, i.e. the last move that was performed for
   * the block to end up at position `b`.
   *
   * The function returns a stream of pairs: the first element of
   * the each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   *
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   *
   * origen:def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = ???
   */
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] =
    b.legalNeighbors.map{ case (blo, mov) => (blo, mov::history)}.toStream
  //moraleja: no se adelante, primero lea el problema y luego revise si hay un ejemplo para garantizar que entendió el problema antes de solucionarlo

  /*
  1st version that works:
def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {
    def neigbor(theMove: Move) = theMove match {
      case Left => if (b.left.isLegal) Option(b.left,Left::history) else Option.empty
      case Right => if (b.right.isLegal) Option(b.right,Right::history) else Option.empty
      case Up => if (b.up.isLegal) Option(b.up,Up::history) else Option.empty
      case Down => if (b.down.isLegal) Option(b.down,Down::history) else Option.empty
    }
    Set(neigbor(Left),neigbor(Right),neigbor(Up),neigbor(Down)).filter(!_.isEmpty).map(_.get).toStream
  }

  1version: NICE but .... I had understand something different
            This version reconstruct the road
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {
    def loop(block: Block, moveHistory: List[Move]): List[(Block, List[Move])] = {
      moveHistory match {
        case headMove :: tailMoves => List((block, moveHistory)) ::: loop( buildLastBlock(block, headMove), tailMoves )
        case Nil => List()
      }
    }
    def buildLastBlock(block: Block, m: Move): Block = m match {
      case Left => block.right
      case Right => block.left
      case Up => block.down
      case Down => block.up
    }
    loop(b, history).toStream
  }
  */

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   *
   * origen:def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] = ???
   */
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] =
      neighbors.filter{ case (block, moves) => !explored.contains(block)}

  /**
   * The function `from` returns the stream of all possible paths
   * that can be followed, starting at the `head` of the `initial`
   * stream.
   *
   * The blocks in the stream `initial` are sorted by ascending path
   * length: the block positions with the shortest paths (length of
   * move list) are at the head of the stream.
   *
   * The parameter `explored` is a set of block positions that have
   * been visited before, on the path to any of the blocks in the
   * stream `initial`. When search reaches a block that has already
   * been explored before, that position should not be included a
   * second time to avoid cycles.
   *
   * The resulting stream should be sorted by ascending path length,
   * i.e. the block positions that can be reached with the fewest
   * amount of moves should appear first in the stream.
   *
   * Note: the solution should not look at or compare the lengths
   * of different paths - the implementation should naturally
   * construct the correctly sorted stream.
   *
   * init: def from(initial: Stream[(Block, List[Move])],
    * explored: Set[Block]): Stream[(Block, List[Move])] = ???
   */
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
    if (initial.isEmpty) Stream.empty
    else {
      val paths =
        for (
          (block, moves) <- initial;
          e <- newNeighborsOnly(neighborsWithHistory(block, moves), explored)
        ) yield e

      initial ++ from(paths, explored ++ initial.map { case (block, moves) => block }.toSet)
    }
  }

  /*
  last try: all tests works:
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
    if (initial.isEmpty) Stream.empty
    else {
      val paths =
        for (
          (block, moves) <- initial;
          e <- newNeighborsOnly(neighborsWithHistory(block, moves), explored) // Set(block) ++ explored
        ) yield e

      initial ++ from(paths, explored ++ initial.map { case (block, moves) => block }.toSet)
    }
  }

  last try: never ends:::
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
    if (initial.isEmpty) Stream.empty
    else {
      val paths =
      for (
        (block, moves) <- initial;
        e <- newNeighborsOnly(neighborsWithHistory(block, moves), explored);// Set(block) ++ explored
        r = from(List(e).toStream, Set(block) ++ explored)
      ) yield r
      paths.flatten
      //initial ++ from(more, explored)
    }
  }

  1st try:
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
    def paths(t: (Block, List[Move])): Stream[(Block, List[Move])] = t match {
        case (block, moves) => newNeighborsOnly(block.legalNeighbors.map{ case(blockNeighbor, move) => (blockNeighbor, move::moves) }.toStream, explored)
    }
    initial.map(paths).flatten
    //neighborsWithHistory
  }
  2nd try: iincomplete:
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
    def paths(t: (Block, List[Move])): Stream[(Block, List[Move])] = t match {
        //case (block, moves) => newNeighborsOnly(block.legalNeighbors.map{ case(blockNeighbor, move) => (blockNeighbor, move::moves) }.toStream, explored)
      case (block, moves) => newNeighborsOnly(neighborsWithHistory(block, moves), explored)
    }

    initial.head match {
      case (block, moves) =>
    }

    initial.map(paths).flatten
    //
  }


  */

  /**
   * The stream of all paths that begin at the starting block.
   *
   * init:lazy val pathsFromStart: Stream[(Block, List[Move])] = ???
   */
  lazy val pathsFromStart: Stream[(Block, List[Move])] = from(List((startBlock, List.empty)).toStream, Set.empty)

  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   *
   * init:lazy val pathsToGoal: Stream[(Block, List[Move])] = ???
   */
  lazy val pathsToGoal: Stream[(Block, List[Move])] = pathsFromStart.filter{ case (block, moves) => done(block)}

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  lazy val solution: List[Move] = if (!pathsToGoal.isEmpty) pathsToGoal.head._2.reverse else List()
}
