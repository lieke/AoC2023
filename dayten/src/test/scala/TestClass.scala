

import org.scalatest.*
import flatspec.*
import matchers.*

class TilegridSpec extends AnyFlatSpec with should.Matchers {

  val simpleGridInput: List[List[Char]] = List(".....",".F-7.",".|.|.",".L-J.",".....").map(_.toCharArray.toList)
  val testGridInput: List[List[Char]] = List("..F7.",".FJ|.","SJ.L7","|F--J","LJ...").map(_.toCharArray.toList)

  "The loop on a simple grid" should "be found" in {
    val simpleGrid = new Tilegrid(simpleGridInput, (1,1))
    simpleGrid.setFirstStepOfLoop((2,1))
    simpleGrid.findLoop()
    simpleGrid.visitedPositions.length should be (9)
  }

  "The loop on a more complex grid" should "be found" in {
    val grid = new Tilegrid(testGridInput, (0, 2))
    grid.setFirstStepOfLoop((1, 2))
    grid.findLoop()
    grid.visitedPositions.length should be(17)
  }

  "The correct starting position" should "be found" in {
    val grid = new Tilegrid(testGridInput, (0,2))
    grid.findStartingPosition() should be ((0,2))
  }
}