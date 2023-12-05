import org.scalatest._
import flatspec._
import matchers._
import scala.util.matching.Regex

//import DayThree.*

val testInput: List[List[Char]] = List("467..114..","...*......","..35..633.","......#...","617*......",".....+.58.","..592.....","......755.","...$.*....",".664.598..").map(_.toList)

val symbols: List[Char] = List('*', '-', '#', '/', '=', '%', '$', '&', '@', '+')

class DayThreeSpec extends AnyFlatSpec with should.Matchers {

  "The test input" should "be represented as a list of list of chars" in {
    testInput.length should be (10)
    testInput(0).length should be (10)
    testInput(0)(0) should be ('4')
  }

  "The right value" should "be located" in {
    getFromEngineSchema((0,0), testInput) should be ('4')
    getFromEngineSchema((9,9), testInput) should be ('.')
    getFromEngineSchema((2,0), testInput) should be ('7')
    getFromEngineSchema((3,1), testInput) should be ('*')
    getFromEngineSchema((6,3), testInput) should be ('#')
  }

  "For a symbol it" should "be determined if it is part of the part number of the engine" in {
    val location = (0,0)
    isNextToSymbol(location, testInput) should be (false)
    val location2 = (9,9)
    isNextToSymbol(location2, testInput) should be (false)
    val location3 = (2,0)
    isNextToSymbol(location3, testInput) should be (true)
    val location4 = (2, 4)
    isNextToSymbol(location4, testInput) should be(true)
  }

  "The engine number sum" should "be found from the test data" in {
    findEngineNumbersFromLine(testInput) should be (4361)
  }

  "The possible gear locations" should "be found" in {

  }

  def isPossibleGear(location: (Int, Int), engineSchema: List[List[Char]]): Boolean = {
    val (x,y) = location
    val value = getFromEngineSchema(location, engineSchema)
    value == '*'
  }

  def findEngineNumbersFromLine(engineSchema: List[List[Char]]): Int = {
    var engineNumberSum = 0
    for (y <- 0 until engineSchema.length) {
      val lineLength = engineSchema(y).length
      var x = 0
      while (x < lineLength) {
        val char1 = getFromEngineSchema((x,y), engineSchema)
        val char2 = if (x < lineLength-1) getFromEngineSchema((x+1,y), engineSchema) else ' '
        val char3 = if (x < lineLength-2) getFromEngineSchema((x+2,y), engineSchema) else ' '
        //println("For x: " + x + " and y: " + y + " -- " + char1 + char2 + char3)
        if (char1.isDigit && char2.isDigit && char3.isDigit && (isNextToSymbol((x,y), engineSchema) || isNextToSymbol((x+1,y), engineSchema)|| isNextToSymbol((x+2,y), engineSchema))) {
          val engineNumber = char1.asDigit*100 + char2.asDigit*10 + char3.asDigit
          println("Found engine number: " + engineNumber)
          engineNumberSum += engineNumber
          x += 2
        }
        else if (char1.isDigit && char2.isDigit && (isNextToSymbol((x, y), engineSchema) || isNextToSymbol((x + 1, y), engineSchema))) {
          val engineNumber = char1.asDigit * 10 + char2.asDigit
          println("Found engine number: " + engineNumber)
          engineNumberSum += engineNumber
          x += 2
        }
        else if (char1.isDigit && (isNextToSymbol((x, y), engineSchema))) {
          val engineNumber = char2.asDigit
          println("Found engine number: " + engineNumber)
          engineNumberSum += engineNumber
        }
        x += 1
      }
    }
    engineNumberSum
  }

  def isNextToSymbol(location: (Int, Int), engineSchema: List[List[Char]]): Boolean = {
    val (x,y) = location
    val value = getFromEngineSchema(location, engineSchema)
    if (!value.isDigit) return false
    else {
      val neighbours = List((x-1, y-1), (x, y-1), (x+1, y-1),
        (x-1, y),             (x+1, y),
        (x-1, y+1), (x, y+1), (x+1, y+1)).filter(_._1 >= 0).filter(_._2 >= 0).filter(_._1 < engineSchema(0).length).filter(_._2 < engineSchema.length)
      val neighbourValues = neighbours.map(getFromEngineSchema(_, engineSchema))
      neighbourValues.intersect(symbols).length > 0
    }
  }

  def getFromEngineSchema(location: (Int, Int), engineSchema: List[List[Char]]): Char = {
    val (x,y) = location
    engineSchema(y)(x)
  }

}