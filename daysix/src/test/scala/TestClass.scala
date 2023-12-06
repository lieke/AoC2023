import org.scalatest.*
import flatspec.*
import matchers.*

import scala.util.matching.Regex

val testInput = ""

class DaySixpec extends AnyFlatSpec with should.Matchers {

  "The correct distance" should "be calculated for a race" in {
    val race1time = 7
    var holdTime = 0
    calculateDistance(race1time, holdTime) shouldEqual 0
    holdTime = 1
    calculateDistance(race1time, holdTime) shouldEqual 6
    holdTime = 6
    calculateDistance(race1time, holdTime) shouldEqual 6
    holdTime = 4
    calculateDistance(race1time, holdTime) shouldEqual 12
    holdTime = 7
    calculateDistance(race1time, holdTime) shouldEqual 0
  }

  "The margin of error to win a boat race" should "be determined" in {

    val errorMargin1 = determineErrorMarginForARace(7, 9)
    errorMargin1 should be (4)
    val errorMargin2 = determineErrorMarginForARace(15, 40)
    errorMargin2 should be (8)
    val errorMargin3 = determineErrorMarginForARace(30, 200)
    errorMargin3 should be(9)
  }

  "The margin of error to win a boat race" should "be more quickly be determined" in {

    val errorMargin1 = sophisticatedErrorMarginDetermination(7, 9)
    errorMargin1 should be(4)
    val errorMargin2 = sophisticatedErrorMarginDetermination(15, 40)
    errorMargin2 should be(8)
    val errorMargin3 = sophisticatedErrorMarginDetermination(30, 200)
    errorMargin3 should be(9)
  }

  def calculateDistance(time: BigInt, holdTime: BigInt): BigInt = {
    val speed = holdTime
    (time - holdTime) * speed
  }

  def determineErrorMarginForARace(raceTime: Int, currentRecord: Int): Int = {
    val possibleHoldTimes = List.range(0, raceTime + 1)
    val raceDistances = possibleHoldTimes.map(holdTime => calculateDistance(raceTime, holdTime))
    raceDistances.filter(_ > currentRecord).size
  }

  def sophisticatedErrorMarginDetermination(raceTIme: BigInt, currentRecord: BigInt): BigInt = {
    var lowerBoundary: BigInt = 0
    var distance: BigInt = 0
    while ((distance <= currentRecord) && (lowerBoundary <= raceTIme)) {
      distance = calculateDistance(raceTIme, lowerBoundary)
      lowerBoundary += 1
    }
    var upperBoundary: BigInt = raceTIme
    distance = 0
    while (distance <= currentRecord && upperBoundary >= 0) {
      distance = calculateDistance(raceTIme, upperBoundary)
      upperBoundary -= 1
    }
    (upperBoundary+1) - (lowerBoundary-1) + 1
  }
}