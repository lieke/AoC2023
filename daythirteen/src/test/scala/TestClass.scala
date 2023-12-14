

import org.scalatest.*
import flatspec.*
import matchers.*

class MirrorSpec extends AnyFlatSpec with should.Matchers {

  val testPattern1 = List("#.##..##.","..#.##.#.","##......#","##......#","..#.##.#.","..##..##.","#.#.##.#.")
  val testPattern2 = List("#...##..#","#....#..#","..##..###","#####.##.","#####.##.","..##..###","#....#..#")
  val testPattern3 = List("a", "b", "b", "a")

  val patternList = List(List("#.##..##.","..#.##.#.","##......#","##......#","..#.##.#.","..##..##.","#.#.##.#."), List("#...##..#","#....#..#","..##..###","#####.##.","#####.##.","..##..###","#....#..#"))

  "The mirror" should "also have columns based on the rows" in {
    val mirror1 = new Mirror(testPattern1)
    mirror1.patternColumns(0) should be ("#.##..#")
    val mirror2 = new Mirror(testPattern2)
    mirror2.patternColumns(0) should be("##.##.#")
  }

  "It" should "be determined if an index is a reflection index" in {
    val mirror = new Mirror(testPattern3)
    mirror.possibleHorizontalReflections should be (List(2))
    mirror.isReflection(2, testPattern3) should be (true)
    mirror.isReflection(1, testPattern3) should be (false)

  }

  "The possible reflectionLine for a mirror" should "be found" in {
    val mirror1 = new Mirror(testPattern1)
    mirror1.reflectionSummary should be(5)
    mirror1.smudgedReflectionSummary should be (300)

    val mirror2 = new Mirror(testPattern2)
    mirror2.possibleHorizontalReflections should be (List(4))
    mirror2.possibleVerticalReflections should be (List(7,3))
    mirror2.reflectionSummary should be (400)
    mirror2.smudgedReflectionSummary should be (100)
  }
}

