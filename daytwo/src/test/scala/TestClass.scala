import org.scalatest._
import flatspec._
import matchers._
import scala.util.matching.Regex

import DayTwo.*

val game1: String = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
val game4: String = "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"

val testInput: List[String] = List("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green", "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue", "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red", "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red", "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

val maxCubes: Tuple3[Int, Int, Int] = (12, 13, 14)

class DayTwoSpec extends AnyFlatSpec with should.Matchers {

  "For a single game the max amount per colour" should "be determined" in {
    val game1Parsed = findMaxForColour(game1)
    game1Parsed should be (4, 2, 6)
    val game4Parsed = findMaxForColour(game4)
    game4Parsed should be (14, 3, 15)
  }

  "The entire testInput" should "be parsed" in {
    val parsed = testInput.map(findMaxForColour)
    parsed should be (List((4, 2, 6), (1, 3, 4), (20, 13, 6), (14, 3, 15), (6, 3, 2)))
  }

  "A game" should " only be possible if it doesn't exceed the max colour cubes" in {
    val game1Parsed = findMaxForColour(game1)
    val game1Possible = isGamePossible(game1Parsed, maxCubes)
    game1Possible should be (true)
    val game4Parsed = findMaxForColour(game4)
    val game4Possible = isGamePossible(game4Parsed, maxCubes)
    game4Possible should be (false)
  }

  "For all games it" should "be determined if they are possible" in {
    val parsed = testInput.map(findMaxForColour)
    val possible = parsed.map(isGamePossible(_, maxCubes))
    possible should be (List(true, true, false, false, true))
  }

  "The sum of all possible game ids" should "be determined" in {
    val parsed = testInput.map(findMaxForColour)
    val sumOfPossibleGameIds = calculateSumOfPossibleGameIds(parsed, maxCubes)
    sumOfPossibleGameIds should be (8)
  }

  "The power of a game" should "be determined" in {
    val game1Parsed = findMaxForColour(game1)
    val game1Power = calculateGamePower(game1Parsed)
    game1Power should be (48)
    val game4Parsed = findMaxForColour(game4)
    val game4Power = calculateGamePower(game4Parsed)
    game4Power should be (630)
  }
}