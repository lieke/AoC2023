
import scala.collection.mutable.ListBuffer

@main def DaySix2023: Unit = {
  val initTime = System.currentTimeMillis
  println("Welcome to day six of the Advent of Code 2023!")
  val errorMargin1 = DaySix.determineErrorMarginForARace(35, 212)
  val errorMargin2 = DaySix.determineErrorMarginForARace(93, 2060)
  val errorMargin3 = DaySix.determineErrorMarginForARace(73, 1201)
  val errorMargin4 = DaySix.determineErrorMarginForARace(66, 1044)

  println("Multiplying the number of ways how you can beat the record is " + errorMargin1 * errorMargin2 * errorMargin3 * errorMargin4)

  val errorMargin5 = DaySix.sophisticatedErrorMarginDetermination(35937366, BigInt("212206012011044"))
  println("The amount of ways to beat the record in the longer race is " + errorMargin5)
  val elapsedTime = System.currentTimeMillis-initTime
  println("This took " + elapsedTime + " milliseconds")

}

object DaySix {

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
    (upperBoundary + 1) - (lowerBoundary - 1) + 1
  }
}


