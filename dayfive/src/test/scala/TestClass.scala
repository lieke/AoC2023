import org.scalatest.*
import flatspec.*
import matchers.*

import scala.util.matching.Regex


val testInput: List[List[(BigInt, BigInt, BigInt)]] = List(List((BigInt("50"),BigInt("98"),BigInt("2")),(BigInt("52"), 50, BigInt("48"))),List((BigInt("0"), 15, 37), (37, 52, 2), (39, 0, 15)), List((49, 53, 8), (0, 11, 42), (42, 0, 7), (57, 7, 4)), List((88, 18, 7), (18, 25, 70)), List((45, 77, 23), (81, 45, 19), (68, 64, 13)), List((0, 69, 1), (1, 0, 69)), List((60, 56, 37), (56, 93, 4)))

val bigIntTest: (BigInt, BigInt, BigInt) = (BigInt("2425244517"),BigInt("2483951770"),157286279)

class DayFiveSpec extends AnyFlatSpec with should.Matchers {

  val seedToSoilMap: List[(BigInt, BigInt, BigInt)] = List((50, 98, 2),(52, 50, 48))

  "For a rangeConversion a source" should "be converted to the right destination" in {
    val rangeExample: (BigInt, BigInt, BigInt) = (50,98,2)
    val (destinationStart, sourceStart, rangeLenth) = rangeExample

    mapToPossibleNewValue(rangeExample, 98) should be (50)
    mapToPossibleNewValue(rangeExample, 99) should be (51)
    mapToPossibleNewValue(rangeExample, 100) should be (100)
    mapToPossibleNewValue(rangeExample, 10) should be (10)
  }

  "For a rangeConversion with large numbers a source" should "be converted to the right destination" in {
    val rangeExample: (BigInt, BigInt, BigInt) = (62761777,1691901751,235673208)
    val (destinationStart, sourceStart, rangeLenth) = rangeExample

    mapToPossibleNewValue(rangeExample, 98) should be(98)
    mapToPossibleNewValue(rangeExample, 1691901751) should be(62761777)
    mapToPossibleNewValue(rangeExample, (sourceStart + rangeLenth-1)) should be (destinationStart + rangeLenth-1)
  }

  "For multipleRangeConversions a source" should "be converted to the right destination" in {
    mapToNewValue(seedToSoilMap, 1) should be (1)
    mapToNewValue(seedToSoilMap, 48) should be (48)
    mapToNewValue(seedToSoilMap, 50) should be (52)
    mapToNewValue(seedToSoilMap, 96) should be (98)
    mapToNewValue(seedToSoilMap, 99) should be (51)
  }

  "The final map location for a seed" should "be found" in {
    val seed = 79
    findTheDestination(seed, testInput) should be (82)

    val seed2 = 13
    findTheDestination(seed2, testInput) should be (35)
  }

  def findTheDestination(seed: BigInt, conversions: List[List[(BigInt, BigInt, BigInt)]]): BigInt = {
    var destinationLocation = seed
    for (conversion <- conversions) {
      destinationLocation = mapToNewValue(conversion, destinationLocation)
    }
    destinationLocation
  }

  def mapToNewValue(newMap: List[(BigInt, BigInt, BigInt)], value: BigInt): BigInt = {
    val possibleDestinations = newMap.map(range => mapToPossibleNewValue(range, value)).distinct
    var destination: BigInt = 0
    if (possibleDestinations.length == 1) {
      destination = value
    } else {
      destination = possibleDestinations.filter( _ != value).head
    }
    destination
  }

  def mapToPossibleNewValue(rangeConversion: (BigInt, BigInt, BigInt), value: BigInt): BigInt = {
    val (destinationStart, sourceStart, rangeLength) = rangeConversion
    if (value >= sourceStart && value < sourceStart + rangeLength) {
      destinationStart + (value - sourceStart)
    } else {
      value
    }
  }

}