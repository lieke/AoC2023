import org.scalatest._
import flatspec._
import matchers._
import scala.util.matching.Regex


val testInput = List((List(41,48,83,86,17),List(83,86,6,31,17,9,48,53)),(List(13,32,20,16,61),List(61,30,68,82,17,32,24,19)),(List(1,21,53,59,44),List(69,82,63,72,16,21,14,1)),(List(41,92,73,84,69),List(59,84,76,51,58,5,54,83)),(List(87,83,26,28,32),List(88,30,70,12,93,22,82,36)),(List(31,18,13,56,72),List(74,77,10,23,35,67,36,11)))

class DayFourSpec extends AnyFlatSpec with should.Matchers {

  "For a scratchcard the right winning numbers" should "be found" in {
    val firstScratchCard: (List[Int], List[Int]) = testInput.head
    val winningNumbers: List[Int] = firstScratchCard._1.intersect(firstScratchCard._2).sorted
    winningNumbers should be (List(17, 48 , 83 , 86))

    val thirdScratchCard: (List[Int], List[Int]) = testInput(2)
    val winningNumbers2: List[Int] = thirdScratchCard._1.intersect(thirdScratchCard._2).sorted
    winningNumbers2 should be (List(1, 21))
  }

  "For a scratchcard the points" should "be calculated" in {
    val firstScratchCard: (List[Int], List[Int]) = testInput.head
    val points1: Int = calculatePointsForCard(firstScratchCard)
    points1 should be (8)
  }

  def calculatePointsForCard = (card: (List[Int], List[Int])) => {
    val winningNumbers: List[Int] = card._1.intersect(card._2)
    if (winningNumbers.length == 0) {
      0
    } else {
      for (number <- winningNumbers) yield number * 2
    }
  }

}