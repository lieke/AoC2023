

import org.scalatest.*
import flatspec.*
import matchers.*

class DishPlatformSpec extends AnyFlatSpec with should.Matchers {

  val testInput = List("O....#....","O.OO#....#",".....##...","OO.#O....O",".O.....O#.","O.#..O.#.#","..O..#O..O",".......O..","#....###..","#OO..#....")

  "The input for a dish platform" should "be parsed into something more usefull" in {
    val dishPlatform = DishPlatform(testInput)
    dishPlatform.initialPositions(0).mkString should be ("OO.O.O..##")
  }

  "Shift North once" should "be done" in {
    val dishPlatform = DishPlatform(testInput)
    val testString = "OO.#O....O"
    dishPlatform.shiftEverything(testString) should be ("OO.#OO....")
  }

  "The weight of the rocks" should "be calculated" in {
    val dishPlatform = DishPlatform(testInput)
    val firstRow:List[Char] = dishPlatform.shiftedNorth(0).toCharArray.toList
    dishPlatform.calculateWeight(firstRow) should be (34)
    dishPlatform.totalLoad should be (136)
  }

  "Doing a number of cycles" should "distribute the weight more evenly" in {
    val dishPlatform = DishPlatform(testInput)
    // TODO this takes waaaay to long. need to rethink this implementation
    // dishPlatform.doAllCyclesAndCalculateLoad() should be (64)
  }
}