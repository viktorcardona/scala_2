package streams

import common._

/**
 * This component implements a parser to define terrains from a
 * graphical ASCII representation.
 *
 * When mixing in that component, a level can be defined by
 * defining the field `level` in the following form:
 *
 *   val level =
 *     """------
 *       |--ST--
 *       |--oo--
 *       |--oo--
 *       |------""".stripMargin
 *
 * - The `-` character denotes parts which are outside the terrain
 * - `o` denotes fields which are part of the terrain
 * - `S` denotes the start position of the block (which is also considered
  * inside the terrain)
 * - `T` denotes the final position of the block (which is also considered
  * inside the terrain)
 *
 * In this example, the first and last lines could be omitted, and
 * also the columns that consist of `-` characters only.
 */
trait StringParserTerrain extends GameDef {

  /**
   * A ASCII representation of the terrain. This field should remain
   * abstract here.
   */
  val level: String

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
    *
    * original:
    * def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = ???
   */
  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = (pos: Pos) => pos match {
    case Pos(row, col) => ( row >=0 &&
                            col >=0 &&
                            row < levelVector.length &&
                            col < levelVector(row).length &&
                            levelVector(row).apply(col) != '-')
    case _ => false
  }

  /*
  version 0:
  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = {
    def colSize(theRow: Vector[Char]): Int = theRow.length
    def validContent(row: Int, col: Int): Boolean = levelVector(row).apply(col) != '-'
    val rowSize = levelVector.length
    def p(pos: Pos): Boolean = pos match {
      case Pos(row, col) => ( row >=0 &&
                              col >= 0 &&
                              row < rowSize &&
                              col < colSize(levelVector(row)) &&
                              validContent(row, col))
      case _ => false
    }
    p
  }

  */

  /**
   * This function should return the position of character `c` in the
   * terrain described by `levelVector`. You can assume that the `c`
   * appears exactly once in the terrain.
   *
   * Hint: you can use the functions `indexWhere` and / or `indexOf` of the
   * `Vector` class
    *
    * origen:def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = ???
   */
  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    val row = levelVector.indexWhere(col => col.indexOf(c) != -1)
    if (row != -1) Pos(row, levelVector(row).indexOf(c)) else null
  }

  /**
    * 1st version:
     def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
     val row = levelVector.indexWhere(col => col.indexWhere(e => e.equals(c)) != -1)
     if (row != -1) Pos(row, levelVector(row).indexWhere(e => e.equals(c)))
     else null
     }
    *
    * 2nd version:
    def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    var colIndex = -1
    def containsChar(column: Vector[Char]): Boolean = {
      colIndex = column.indexWhere(e => e.equals(c))
      colIndex != -1
    }
    val row = levelVector.indexWhere(col => containsChar(col))
    if (row != -1) Pos(row, colIndex)
    else null
  }

    3rd version:
    def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    var colIndex = -1
    def containsChar(column: Vector[Char]): Boolean = {
      colIndex = column.indexWhere(_.equals(c))
      colIndex != -1
    }
    val row = levelVector.indexWhere(colVector => containsChar(colVector))
    Pos(row, colIndex)
  }
  }

    4th version:
    def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    var col = -1
    val rowIndex = levelVector.zipWithIndex.find(r => {
      col = r._1.indexWhere(_.equals(c))
      col!= -1
    })
    val row = rowIndex.getOrElse((null, -1))._2
    Pos(row, col)
  }
    */

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\n").map(str => Vector(str: _*)): _*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)

}
