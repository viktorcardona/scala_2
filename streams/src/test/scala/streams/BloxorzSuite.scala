package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1: startPos") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  //NEW: TEST
  test("findChar level 1: goal") {
    new Level1 {
      assert(goal == Pos(4,7))
    }
  }


	test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }


  //NEW: TEST
  test("isStanding [Pos(4,7), Pos(4,7)] should be true ") {
    new Level1 {
      assert(Block(Pos(4,7), Pos(4,7)).isStanding)
    }
  }

  //NEW: TEST
  test("isStanding [startPos, startPos] should be true ") {
    new Level1 {
      assert(Block(startPos, startPos).isStanding)
    }
  }

  //NEW: TEST
  test("isStanding [goal, goal] should be true ") {
    new Level1 {
      assert(Block(goal, goal).isStanding)
    }
  }

  //NEW: TEST
  test("isStanding [startPos, goal] should be false ") {
    new Level1 {
      assert(!Block(startPos, goal).isStanding)
    }
  }

  //NEW: TEST
  test("isLegal [startPos, goal] should be true. Char different than -") {
    new Level1 {
      assert(Block(startPos, goal).isLegal)
    }
  }

  //NEW: TEST
  test("isLegal [Pos(5,6), Pos(5,6)] should be true. Char different than -") {
    new Level1 {
      assert(Block(Pos(5,6), Pos(5,6)).isLegal)
    }
  }

  //NEW: TEST
  test("isLegal [Pos(5,9), Pos(5,9)] should be false. Char aqueal to -") {
    new Level1 {
      assert(!Block(Pos(5,9), Pos(5,9)).isLegal)
    }
  }

  //NEW: TEST
  test("isLegal [Pos(50,90), Pos(50,90)] should be false. Positions out of the tarrain") {
    new Level1 {
      assert(!Block(Pos(50,90), Pos(50,90)).isLegal)
    }
  }


  //NEW: TEST
  test("Move Right from Pos(0,0), Pos(0,0)") {
    new Level1 {
      assert(Block(Pos(0,0), Pos(0,0)).right == Block(Pos(0,1), Pos(0,2)))
    }
  }

  //NEW: TEST
  test("Move Right from Pos(0,1), Pos(0,2)") {
    new Level1 {
      assert(Block(Pos(0,1), Pos(0,2)).right == Block(Pos(0,3), Pos(0,3)))
    }
  }

  //NEW: TEST
  test("Move from Pos(0,0), Pos(0,0) right.right.down.down.left.left is (3,0),(3,0)") {
    new Level1 {
      val b = Block(Pos(0,0), Pos(0,0))
      assert(b.right.right.down.down.left.left == Block(Pos(3,0), Pos(3,0)))
    }
  }

  //NEW: TEST
  /*
  test("neighborsWithHistory [Block(Pos(3,0), Pos(3,0)), List(Left, Left, Down, Down, Right, Right)] should be XYZ") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(3,0), Pos(3,0)), List(Left, Left, Down, Down, Right, Right)) == List((Block(Pos(3,0),Pos(3,0)),List(Left, Left, Down, Down, Right, Right)), (Block(Pos(3,1),Pos(3,2)),List(Left, Down, Down, Right, Right)), (Block(Pos(3,3),Pos(3,3)),List(Down, Down, Right, Right)), (Block(Pos(1,3),Pos(2,3)),List(Down, Right, Right)), (Block(Pos(0,3),Pos(0,3)),List(Right, Right)), (Block(Pos(0,1),Pos(0,2)),List(Right))).toStream)
    }
  }
  */

  //NEW: TEST
  test("neighborsWithHistory [Block(Pos(1,1),Pos(1,1)), List(Left,Up))] should be XYZ") {
    new Level1 {
      assert(neighborsWithHistory(
        Block(Pos(1,1),Pos(1,1)), List(Left,Up)) ==
        List(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream)
    }
  }

  //NEW: TEST
  test("newNeighborsOnly test Ok") {
    new Level1 {
      assert(
        newNeighborsOnly(
          Set(
            (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
            (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
          ).toStream,
          Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
        )
        ==
        Set(
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream
      )
    }
  }

}
