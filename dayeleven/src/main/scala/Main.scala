
import scala.collection.mutable.ListBuffer

@main def DayEleven2023: Unit = {
  val initTime = System.currentTimeMillis
  println("Welcome to day eleven of the Advent of Code 2023!")
  val analyzer = new GalaxyAnalyzer(galaxyInput)
  val sumOfShortedPaths: BigInt = analyzer.galaxyPairs.map(pair => analyzer.calculateExpandedShortestPath(pair(0), pair(1), 2)).sum
  println("The sum of the shortest paths between all pairs of galaxies is " + sumOfShortedPaths)
  val sumOfShortedPathsForOldUniverse: BigInt = analyzer.galaxyPairs.map(pair => analyzer.calculateExpandedShortestPath(pair(0), pair(1), 1000000)).sum
  println("The sum of the shortest paths between all pairs of galaxies in an old universe is " + sumOfShortedPathsForOldUniverse)
  val elapsedTime = System.currentTimeMillis-initTime
  println("This took " + elapsedTime + " milliseconds")
}

class GalaxyAnalyzer(imageInput: List[String]) {

  private val image: List[List[Char]] = imageInput.map(_.toCharArray.toList)
  val expandingRows: List[Int] = findEmptyRows()
  val expandingColumns: List[Int] = findEmptyColumns()
  val galaxies: List[(Int, Int)] = findGalaxies()
  val galaxyPairs: List[List[(Int, Int)]] = galaxies.combinations(2).toList

  private def findEmptyRows(): List[Int] = {
    val emptyRows = ListBuffer[Int]()
    for (i <- image.indices) {
      if (!image(i).exists(_ != '.')) emptyRows += i
    }
    emptyRows.toList
  }

  private def findEmptyColumns(): List[Int] = {
    val emptyColumns = ListBuffer[Int]()
    for (i <- image.head.indices) {
      val column = image.map(_(i))
      if (!column.exists(_ != '.')) emptyColumns += i
    }
    emptyColumns.toList
  }

  private def findGalaxies(): List[(Int, Int)] = {
    val galaxies = ListBuffer[(Int, Int)]()
    for (i <- image.indices) {
      for (j <- image.head.indices) {
        if (image(i)(j) == '#') {
          galaxies += ((j, i))
        }
      }
    }
    galaxies.toList
  }

  def calculateExpandedShortestPath(galaxy1: (Int, Int), galaxy2: (Int, Int), expansion: Int): BigInt = {
    val (x1, y1) = galaxy1
    val (x2, y2) = galaxy2
    val xDistance = Math.abs(x1 - x2)
    val yDistance = Math.abs(y1 - y2)
    val crossedEmptyColumns: Int = List.range(x1.min(x2), x1.max(x2)).intersect(expandingColumns).size
    val crossedEmptyRows: Int = List.range(y1, y2).intersect(expandingRows).size
    xDistance + yDistance + crossedEmptyColumns * (expansion-1) + crossedEmptyRows * (expansion-1)
  }
}

val galaxyInput: List[String] = List("...................#.............#...........................................#..............................................................",".......................................#.........#..................................................................#.........#........#....","..........#........................................................#................#............#..........................................","...#.................................................#......................................#...............................................",".................#........................................#...............................................#.....#...........................","..............................#...........#.................................................................................................","...............................................#.....................................................................................#......",".............................................................#...........................#..................................................","....#..........#...............................................................#............................................................","................................#................................................................#................#.......................#.","...........................#.............#......................#......................................#....................#...............","#..........#..........#....................................#.............#..................................................................","....................................#..............#..........................................................#........#....................","..........................................................................................#.............................................#...","............................................#.......................#..........................#............................................",".......#...........................................................................#.......................#...................#............","................#...........#.........................#.....................................................................................","................................................#...................................................................#.................#.....","....#....................................................................................#..................................................","............#.......................................................................................#.......................................","...................................#...........................#................................................#...........................",".......#.........#......#....................................................#........................................#.....#...............","......................................................#......................................#..............................................","..#.............................#............#.........................................................#....................................","...........................................................#..........#...........................#........................................#",".............#....................................#.......................................#.........................#...............#.......",".............................#..............................................................................................................","......................#....................#...................#...........#.........#.....................................#................","....................................#..................#.......................................#............#............................#..","..................#.............................#......................................................#....................................",".......#.......................#.......................................#........................................#................#..........","...........................................................................................#................................................",".....................#...........................................................................#........#..................#.......#......","...............................................................#..................#.........................................................","....#.......................................#.............#..........#......................................................................","...........................................................................#............#........................#..........................",".........#...............................................................................................................#..................","..........................#......#..........................................................................................................","..............#..................................#................#.......................................#................................#","..........................................#.................#...................#..................#........................................",".....................................................................................................................................#......",".............................#......................#..................................#..............................#.....................",".....................................#.........................................................................#...........#................","........#..................................................................#................................................................",".#..........................................#........................................................#......................................","....................#..............................................................................................#........................","..........................#........#..........................#......................#...........................................#..........",".......................................................#....................................................................................","................#................................#...........................#...........#.........#...................................#....","......................................................................#.....................................................................",".......................#....................................#.....................#..........#..............................................","........................................#...................................................................................................","..........#..........................................................................................#...........#..........................","...#.........................#....................#.........................................................................................","........................................................................#................................................#..............#...","............................................................................................................................................",".......................#..............................#......#..........................#...................................................",".............................................................................#................................#.......#.....................","............................................................................................................................................","................#..................................#.........................................................................#............#.","...........................#.........#..............................#.....#........#................................................#.......","......................#.............................................................................#......#................................",".........#.....................................#........................................................................#...................","..............#................................................#...............................#............................................",".....#...........................#.....................................................................#....................................","....................................................................................#..............................................#........","............................#..............#.......#......................................#................................................#","...........#.............................................#................#.....#.....................................#.....................","....................#............................................................................#...........................#..............","...................................#.............................................................................#...................#......","....#.........#.....................................................#......................................#................................",".........................#......................#..........#................#...............................................................","...................................................................................#......#.................................................",".................#.............................................#............................................................................",".............................#.....................#.................................................#......................................",".........#.............#..................................................................................#........................#........",".......................................#.................................#.....................#......................#...................#.","...............................................#................................#...........................................................","..#....................................................#............#.............................................#...........#.............","............................................................................................................................................","....................................#..............#............#...........................................................................","...........#...................#........................................#.................#..............#.................................#","....................#...............................................................#...............#.......................................","...........................................#...................................#..............................#.......#.....................","......#..........................................#.........#.........#..........................#................................#..........","..............#.............................................................................................................................","............................................................................................................................................","...#...........................................................#.....................................#.....#..............#.................","............................................................................................................................................","........................................#.......#.........#..................#....................................#.......................#.",".....................................................................................................................................#......",".................#.....#........#..........................................................#.................#..............................",".....................................#.................#.................#...............................................#.....#............",".........................................................................................................#..................................","..................................................................#.........................................................................","..............................................#...........#....................#.........................................................#..","..................................#.........................................................................................................","..........#..........................................................................#.......................#.....#........................",".....#..............#.......#..................................#............................................................................","............................................#.................................................#............................#.......#........","..............#..........................................#.................................................................................#","..........................................................................#..........................#......................................",".................................#..............................................#......#....................................................","#.......#..................................................................................................#.....#......................#...","........................................#..........#........................................................................................","............................................................................................#..............................#................","...........................#..........................................#.....................................................................","....................#..........................#..................................................................................#.........","..........................................................#.............................#.......#.............#.............................","#......#........#...................#..............................#.................................................#......................",".........................#.................#..............................#....................................................#............","..............................#....................................................#................#.......................................","................................................................................................................#........#..................","...........#...........................#............#.......................................................................................","...................#.........................................................................#..............................................","...........................................................................#.........#..........................................#...........","..#.............................#..................................#..................................#.............#.......................","................#........#...............#..................................................................#........................#......","...........................................................................................#................................................","..................................................#......#.....................#............................................................","#.........................................................................................................................#.................","......#............#..........................#.............................................................................................","..............#........................................................................#..........................#.........................","................................#............................................................................................#..............",".......................#.........................#......................#..................#.............................................#..","..#.....................................................#.......................#................#.......#..................................","............................................................................................................................................","..............................................................#.............................................................................","...................#.....................#..........................................................................#..........#............",".........#..........................................................................................#.......................................","..........................#......#...............#.....#....................................................................................",".............#.........................................................#......................#.............................................","............................................................#..................#........................#................................#..","..................#....................................................................................................#........#...........",".....#.....................................................................#.................................#..............................","..................................................#.........................................................................................",".....................................#..............................................#........#......#...............................#.......","............................................................................................................................................","...........................#............................#..........#......................................................#.................","#............#.......#...................................................#.......................#..........................................")


