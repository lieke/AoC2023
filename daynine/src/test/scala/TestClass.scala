

import org.scalatest.*
import flatspec.*
import matchers.*

class InstabilitySensorSpec extends AnyFlatSpec with should.Matchers {

  val testdata = List(List(0,3,6,9,12,15),List(1,3,6,10,15,21),List(10,13,16,21,30,45))

  val sensor = new InstabilitySensor(testdata)

  "The steps in a list" should "be found" in {
    val history = testdata(0)
    sensor.findHistoryDifferences(history) should be (List(3,3,3,3,3))

    val history2 = testdata(2)
    sensor.findHistoryDifferences(history2) should be (List(3,3,5,9,15))
  }

  "The amount of sequences it takes to find a 0 sequence" should "be found" in {
    val history = testdata(0)
    sensor.findTheAmountOfSequences(history).length should be (2)

    val history2 = testdata(2)
    sensor.findTheAmountOfSequences(history2).length should be (4)
  }

  "The extrapolated value of a history" should "be determined" in {
    val history = testdata(0)
    sensor.extrapolateValue(history) should be (18)

    val history2 = testdata(2)
    sensor.extrapolateValue(history2) should be (68)
  }

  "The sum of the extrapolated values" should "be found" in {
    sensor.findSumOfExtrapolatedValues() should be (114)
  }

  "The backwards extrapolated value of a history" should "be determined" in {
    val history = testdata(0)
    sensor.backwardsExtrapolatedValue(history) should be(-3)
  }
}