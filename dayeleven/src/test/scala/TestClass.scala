import org.scalatest.*
import flatspec.*
import matchers.*

class GalaxyAnalyzerSpec extends AnyFlatSpec with should.Matchers {

  val testImage = List("...#......",".......#..","#.........","..........","......#...",".#........",".........#","..........",".......#..","#...#.....")

  "The galaxy analyzer" should "know where the galaxy should expand" in {
    val analyzer = new GalaxyAnalyzer(testImage)
    analyzer.expandingRows should be (List(3, 7))
    analyzer.expandingColumns should be (List(2, 5, 8))
  }

  "The galaxy analyzer" should "know where the galaxies are" in {
    val analyzer = new GalaxyAnalyzer(testImage)
    analyzer.galaxies should be (List((3, 0), (7, 1), (0, 2), (6, 4), (1, 5), (9, 6), (7, 8), (0, 9), (4, 9)))
  }

  "The shortest path between 2 galaxies" should "be calculated for an expanded galaxy" in {
    val analyzer = new GalaxyAnalyzer(testImage)
    val galaxy1 = analyzer.galaxies(0)
    val galaxy2 = analyzer.galaxies(1)
    val galaxy3 = analyzer.galaxies(2)
    val galaxy4 = analyzer.galaxies(3)
    val galaxy5 = analyzer.galaxies(4)
    val galaxy6 = analyzer.galaxies(5)
    val galaxy7 = analyzer.galaxies(6)
    val galaxy8 = analyzer.galaxies(7)
    val galaxy9 = analyzer.galaxies(8)
    analyzer.calculateExpandedShortestPath(galaxy1, galaxy7, 2) should be (15)
    analyzer.calculateExpandedShortestPath(galaxy3, galaxy6, 2) should be (17)
    analyzer.calculateExpandedShortestPath(galaxy8, galaxy9, 2) should be (5)
    analyzer.calculateExpandedShortestPath(galaxy1, galaxy2, 2) should be (6)
    analyzer.calculateExpandedShortestPath(galaxy1, galaxy6, 2) should be (15)
    analyzer.calculateExpandedShortestPath(galaxy4, galaxy5, 2) should be (8)
  }

  "All the shortest pairs of galaxies" should "be determined" in {
    val analyzer = new GalaxyAnalyzer(testImage)
    analyzer.galaxyPairs.length should be (36)
  }

  "The sum of all the shortests paths in the expanded universe" should "be determined" in {
    val analyzer = new GalaxyAnalyzer(testImage)
    analyzer.galaxyPairs.map(pair => analyzer.calculateExpandedShortestPath(pair(0), pair(1), 2)).sum should be (374)
  }

  "The sum of all the shortests paths in the extra expanded universe" should "be determined" in {
    val analyzer = new GalaxyAnalyzer(testImage)
    analyzer.galaxyPairs.map(pair => analyzer.calculateExpandedShortestPath(pair(0), pair(1), 10)).sum should be(1030)
    analyzer.galaxyPairs.map(pair => analyzer.calculateExpandedShortestPath(pair(0), pair(1), 100)).sum should be(8410)
  }
}