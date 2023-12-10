

import org.scalatest.*
import flatspec.*
import matchers.*

import scala.util.matching.Regex

class HauntedWastelandSpec extends AnyFlatSpec with should.Matchers {

  val instruction = List(false, false, true)

  val map = List(("AAA", ("BBB", "BBB")), ("BBB", ("AAA", "ZZZ")), ("ZZZ", ("ZZZ", "ZZZ")))

  val nav = new HauntedWastelandNavigationSystem(instruction, map)

  "The right next step" should "be found" in {
    val result = nav.nextStep("AAA", true)
    result should be ("BBB")

    val result2 = nav.nextStep("BBB", true)
    result2 should be("ZZZ")

    val result3 = nav.nextStep("BBB", false)
    result3 should be("AAA")
  }

  "The amount of steps to Z" should "be found" in {
    nav.findThePathToTheDestination("AAA", "ZZZ") should be(6)
  }


}