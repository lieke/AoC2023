import org.scalatest._
import flatspec._
import matchers._

import DayOne.*

val calibrationDocument: List[String] = List("1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet")
val calibrationDocumentWithNumbers: List[String] = List("two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen")

class DayOneSpec extends AnyFlatSpec with should.Matchers {

  "The calibration document" should "be transformed to chars" in {
    val calibrationList = calibrationDocument.map(_.toList)
    calibrationList should be (List(List('1', 'a', 'b', 'c', '2'), List('p', 'q', 'r', '3', 's', 't', 'u', '8', 'v', 'w', 'x'), List('a', '1', 'b', '2', 'c', '3', 'd', '4', 'e', '5', 'f'), List('t', 'r', 'e', 'b', '7', 'u', 'c', 'h', 'e', 't')))
  }

  "The calibration document" should "filtered into only digits" in {
    val digits = DayOne.getDigits(calibrationDocument)
    digits should be (List(List(1, 2), List(3, 8), List(1, 2, 3, 4, 5), List(7)))
  }

  "The calibration digits" should "be put back together as two digit numbers" in {
    val digits = DayOne.getDigits(calibrationDocument)
    val twoDigitNumbers = digits.map(DayOne.getFirstAndLastDigit)
    twoDigitNumbers should be (List((1, 2), (3, 8), (1, 5), (7, 7)))
  }

  "The value of a two digit group" should "be calculated correctly" in {
    val testNumber = (1, 2)
    val realNumber = DayOne.getTwoDigitValue(testNumber)
    realNumber should be (12)
  }

  "All the calibration values" should "be put back together as two digit numbers" in {
    val realNumbers = DayOne.findAllRealNumbers(calibrationDocument)
    realNumbers should be (List(12, 38, 15, 77))
  }

  "The sum of the calibration values" should "be calculated correctly" in {
    val realNumbers = DayOne.findAllRealNumbers(calibrationDocument)
    val sum = realNumbers.sum
    sum should be (142)
  }

  "The right numbers" should "be found" in {
    val test0 = ""
    DayOne.findDigits(test0).toList should be (List())
    val test1 = "1"
    DayOne.findDigits(test1).toList should be (List(1))
    val test2 = "two1nine"
    DayOne.findDigits(test2).toList should be (List(2,1,9))
  }

  "All the written out numbers in the calibration document" should "be replaced with their digits" in {
    val testdata2AsNumbers = calibrationDocumentWithNumbers.map(DayOne.findDigits(_).toList)
    println(testdata2AsNumbers)
    testdata2AsNumbers should be (List(List(2, 1, 9), List(8, 2, 3), List(1, 2, 3), List(2, 1, 3, 4), List(4, 9, 8, 7, 2), List(1, 8, 2, 3, 4), List(7, 6)))
  }
}