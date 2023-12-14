

import org.scalatest.*
import flatspec.*
import matchers.*


class SpringConditionAnalyzerSpec extends AnyFlatSpec with should.Matchers {

  "The right contiguousRegex" should "be built" in {
    val analyzer = new SpringConditionAnalyzer()
    val pattern = List(1,1,3)
    val regex = analyzer.createContiguousRegex(pattern)
    regex should be ("\\.*[\\.#]{1}\\.+[\\.#]{1}\\.+[\\.#]{3}\\.*")
  }

  "The right conditionRegex" should "be built" in {
    val analyzer = new SpringConditionAnalyzer()
    val condition = "???"
    val regex = analyzer.createConditionRegex(condition)
    regex should be ("[\\.#][\\.#][\\.#]")
  }

  "The steps in a list" should "be found" in {
    val analyzer = new SpringConditionAnalyzer()
    val testString1 = "#.#.### 1,1,3"
    analyzer.findTheAmountOfArrangements(testString1) should be (1)

    val testString2 = ".??..??...?##. 1,1,3"
    analyzer.findTheAmountOfArrangements(testString2) should be (4)

    val testString3 = "?###???????? 3,2,1"
    analyzer.findTheAmountOfArrangements(testString3) should be (10)
  }

}