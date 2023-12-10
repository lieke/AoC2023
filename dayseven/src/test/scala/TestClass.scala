import DaySeven.*
import org.scalatest.*
import flatspec.*
import matchers.*

import scala.util.matching.Regex

val hand1 = "22222"
val hand2 = "AAAAK"
val hand3 = "TJ382"
val hand4 = "TJ384"
val hand5 = "2222A"
val hand6 = "33332"

val testData = List(("32T3K",765),("T55J5",684),("KK677",28),("KTJJT",220),("QQQJA",483))


class CamelCardsSpec extends AnyFlatSpec with should.Matchers {

  "Two different hands" should "have the correct relative strength" in {
    (getHandValue(hand1) > getHandValue(hand2)) should be (true)
    println(getHandValue(hand6))
    println(getHandValue(hand5))
    (getHandValue(hand6) > getHandValue(hand5)) should be (true)
  }

  "The total winnings of a set of hands" should "be calculated" in {
    getTotalWinnings(testData) should be (6440)
  }


}