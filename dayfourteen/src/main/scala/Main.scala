
import scala.collection.mutable.ListBuffer

@main def DayFourteen2023: Unit = {
  val initTime = System.currentTimeMillis
  println("Welcome to day fourteen of the Advent of Code 2023!")
  val dishPlatform = new DishPlatform(input)
  println("The total load is " + dishPlatform.totalLoad)
  val elapsedTime = System.currentTimeMillis-initTime
  println("This took " + elapsedTime + " milliseconds")
}

class DishPlatform(initialPositionsInput: List[String]) {

  val initialPositions: List[String] = initialPositionsInput.transpose.map(_.mkString)
  val shiftedNorth: List[String] = initialPositions.map(shiftEverything)
  val totalLoad: Int = shiftedNorth.map(x => calculateWeight(x.toCharArray.toList)).sum
  val cycles: Int = 1000000000

  def doAllCyclesAndCalculateLoad(): Int = {
    var positions = initialPositions
    for (i <- 1 to cycles) {
      positions = doACycle(initialPositions)
    }
    positions.map(x => calculateWeight(x.toCharArray.toList)).sum
  }

  def doACycle(initialPositions: List[String]): List[String] = {
    val shiftedNorth = initialPositions.map(shiftEverything)
    val beforeShiftWest = shiftedNorth.reverse.transpose.map(_.mkString)
    val shiftedWest = beforeShiftWest.map(shiftEverything)
    val beforeShiftSouth = shiftedWest.reverse.transpose.map(_.mkString)
    val shiftedSouth = beforeShiftSouth.map(shiftEverything)
    val beforeShiftEast = shiftedSouth.reverse.transpose.map(_.mkString)
    val shiftedEast = beforeShiftEast.map(shiftEverything)
    shiftedEast.reverse.transpose.map(_.mkString)
  }

  def shiftEverything(positions: String): String = {
    var shiftedPositions: String = positions
    var newPositions = shift1North(positions)
    while (!shiftedPositions.equals(newPositions)) {
      shiftedPositions = newPositions
      newPositions = shift1North(shiftedPositions)
    }
    newPositions
  }

  def shift1North(positions: String): String = {
    var shiftedPositions: String = ""
    positions match {
      case s".O$rest" => shiftedPositions = "O" + shift1North("." + rest)
      case s".$rest" => shiftedPositions = "." + shift1North(rest)
      case s"O$rest" => shiftedPositions = "O" + shift1North(rest)
      case s"#$rest" => shiftedPositions = "#" + shift1North(rest)
      case _ => //do nothing
    }
    shiftedPositions
  }

  def calculateWeight(positions: List[Char]): Int = {
    var weight: Int = 0
    positions match {
      case 'O' :: tail => weight = (positions.length + calculateWeight(tail))
      case _ :: tail => weight = calculateWeight(tail)
      case Nil => weight = 0
    }
    weight
  }
}

val input: List[String] = List("####....#..O#O..#.....OO.O#..O.......O.#OO.#...O.O..O..O..OO.##.O#...........#....##O#..O#.O.O......","....#..#.........O.O#..OO#OO....O.....#O.#.O##..#...OO#OOO.O...#...O..#.O.#....##.....#O............","...#O..#........#....O...O.......O#.##.O.......O..O#O#.#OO....O..O.......#O.........#O.#.O...#...#..","#O...O...O.O..O..OO.#...O.O#....##O.##..#.OOO#...OO.#...OOO.#.O.#....#....O..##.....OO....#..O...#O.",".#.......##.#...#...O............O...#......#..#.OO...O#....O.#..........O..O.OOO#.#O#....OO..#.#O.#","..#..........#..O.O#.....O.OOO.#.##...##...OO..#..#.O#..OO.O#..#O..#O.O#.O.O#..#.#OOO..#OO.O.##..#..","#..#....O.#O.O.#...O......O...###O.#...OOO#..#.O..#O.O..O.#OO...O...#..#OOO.....O...O...OO..##....O.","...##....#..#.O#..#O...#.#......##.OO.O.......O....#O#..OOOO#....O.#OO.#O....#...O#O#..O.#.....O....","...#O....#.#O........O.O#O....O...#.O....#....#...#O.....O.O.O.#.#.......#.........#O#........O.....","...O...#.O#....#O#O.....O..O#...#O........O.#.##.....O.#...O..#...##O.#.#O#..O..##......#O.#...O#..#","#..OO#.#.........O#......#O.##.O#...O..#.#.#.OO........#......#....#..#.......O.#O.##.###.####....O.","O..O...#O..O..##.O..............#O......OOO#...#.#.#.........OO....OO....O#.#.O#.O.........#.......O","..O#.#....O..#....#OO..##...#..#...O..##O...##.O..O.OO.#.###...O..#.OO..O#...#......#.....O.#..O..#.","O..####.O..O.OO##.O...O..O#.#O..O...O..........#O.#..#..O..O#..O##O.O.#O.#..O.OO.O..#...O.#O..O.O.#.",".......O..........#...OO.O#.#.#OOOO...OO#O..O.#.#......#O.#.#...#.O#......O.O.OO...#.##..#.O.#..O..#",".#.....#O........#.O...#........O............#O......##.O..O..#.#......O#O..O...O..O.#.O...O.#..OOOO",".......O#...O...OO#.....#..O.#O..##..#O.#....OO.##.#O#......O#..O....OO..#...#O...O...##O..O###..OOO","...O...#..O..O..#O.O......#.O...#..O#..#..O.OO...O....O.#O#..###O....O..##OOO.#O.....#...#.....#...O","O...OO.#...#...O.O..#O.OO..O.#.#.#O#.O.#.O..O#O.....##O..O#.O..O........O...#.......O#.#...#.#..OO..","..OO..#.#.O#.OOO.....##.O..OOO##O#.O..O.##OOO...O..##.O......#O.##..#O..O....#.##.O###...##..O......",".......O..OO....##......O..O#O.O.O#....#O.....#....O..O.OO...#.O.#..#....#.O..#O....O......##O.....#","......O..#...O..O#O.#.O.......#.O#.....O.O#..#.#...O..##......O.O..O#.#.#..#.O.#O.O#.O.#......#.O...","....O.....O..O.......#.O.OO.....O##....##.#OO..#O..#.#...##..O#O.O...O..OO..##....O#O..O....#...O.#O",".......O.##O.#O#.........O#.OO....###.OO..O.#..#.....OO.O.O...O......O..O..O....O...#.#....#....#O..","O..#O......#.O..#O.O.#......O#..#.O##..#.#.O.....#.O...O..OO..####.#...##......#.....#O#......OO.O.#","...OOO.#O....O#O...O.##OO..#....OO...O.#.....OO.#OOO.#.........#....O....#.#..O.#O#.#...O.O...O.#...","..O.#.OOO..O.#...#OO.#.O.##OO..O..####.#....#.OO##O..#.O..O#..O#....#..O#..#.O.O..O...O...O.....O.O.",".#.#O.O..#.O.O##.....O#..#..#..OO##...OOO.#O...#.O#...#O....#OO.OO..#.OO#.OO#O#............O......#.","O#..#....O...#.#.O....#.OOO.#.OO.....OO###.O..#..#.O..#...#.#....O#.O#....#.#.O.#.....#....O.....O#.","O..O.#.....#.....OOO..O..OOO.#..#.##.O..O.......#..O.##O#...#.#.......#.#....#.O.O..OO..#.O..#..O#O.","...O...O#....#O.......#........#.O.O..O.#O#...O...#..O.#..OO#O...#........O...#.....OO.#...#.O...#.#","O.#.O#...O#....#OO.O.##......O...##..#..O#.#.#O.....OOO..#O....#...O..O#O....#.#....OO.#..#.....OO.#","...O#..O.#.#..O........O.....#.........#.....O.......O......#..#.#O....#O.O#O.#..##O#....#O...O.O...","#..O.O...O#..O.O.#O..#...O#....#..#...O..#O...O.....O..OO#OOO#OOO...#.O...#.OO#O.....#O.#.........O.","O.O...#....O#OO...O.OO..#..OO...#..O..O.O....#...#..O..OOO#.....#..OO.#O....O.#.O.OOO.#OO....##OO..O",".#O#.#.##...###O..O#...#.O.#.O...O#.O.....O....O..OOO....#...O...OO.....#.#O#O..#O......#.O.....#.#O","O.#O.OO......##......O..OOO##.OOO.#..O...O.O#..#O.#O.O......#................##..#....#O......#.....","........#.....##...OO.O......#..#.......#O.....O....O....#O.##..OO...O...O..........#..O.....OO...#.","#..O.#.......O......O#.O..........#O#.O#.#..#..O..O#.O.O..O.O........O......O..O.O..O.O..#O....O#.#.","...#...OO#..#..##.O.....O...........O.#O#O...##...O..#..#..O....#..O#O.##..O##......OOO....##..###.#",".#..#....OOO......O#O...##O..O.OO...O..#.....#.O..##.....#.#....O...#...OO#O#..O...OO#......#.#...O#","##..#....O...OOOO....OO...O......O.#.####O.O.....O..O..O...##O.#.O...#O..O....#O.O..O..O....O..#.O..",".......OO.O#.#O.O.O.O...O.OO...O.#..#...##..O..O.O...O..#..O#..##..O#.#.#.#.O#...O.....O#OO.........","...#.O#.O#..#....O.....OO..........#O.........O...O.#.....O........#..O...O...OO.##........#OO...O.O","#..O#.#..............#.#.#...O..O...#...OO.........#O..O#OOO..#....O....###.....O.O........##O...O.O","O.O#......O....#.#O..O..##O....#.....O###...#..#.#.........##........#O#..O#...O..#.#..O##OO.#.OOO..","#.#...##OO........O.....#...#..OO...OO..OO...O.#...O##....O..O#.O.O.O....#..O.##.......O#......O....","..#.O..#.......OO.....#...OO.O.O..O#.........#O.....#...#O......O.....O.........O..#..OO...O...O..OO","......O....O.O.....#.OOO.#..O.O.O.O.OO.#.O##OO...O....OO....O....O....#.O...........O......#...O...#","#O..##....#...OOOO......O.#.O#O...OO...O#..##O....O.....O..###.O#....O.........O...O#.....O..O.O...O","OO..#.#O....##.#..OOO...O#.......O.#...#.#....O.O#.#..#O#..#....#.O.##.OO.....#....#.....O#..#.....#","#.O.....#..#.O.....O#..#O#O.#.##O.....O.OO..O#.OO............O..O...O...##O..OO..O...#...O.....O#O.O","O........##.#O.OO...#...#O.#...O............##...O..#...OO.O.......#.O.O.....#.O........OOO#.##.....","....#...##.....O.....O.....#....#OO..O..#O.......O#.O..OO.#..OO......O..O..O...O..#O.......O..O..O#.","##.O.O....O.O#.........O.OOO..O.....O.OO..O....#.O...#.....#.#...O...OO#.OO..O.#..#.O.O.O...#..O.OO.","O..#..OO..O..#O...O...#..O#...........#.#......#..O#.O.#.....#O...O.......O.O.......O#O.#.#O..OO#.O#","..........O..O#......#.......#..O..O....O#.....O....O#...OO..O.O...#...O.O...O.O.O..##........#.#..#",".........O..#.O..#..#..#....O....O#..#.#..O....O....O....O...#O.......##.##.......O##.#..#..O...#O.#","##O...#..........O#.....O....#..O....#.O#O......OO.O.O........O...O.#..O#.......O....#......O.....#.","#.#.O.O...#.#....#.....O...O.#..OO#..#....#.O#.#O.#...O..O..OO...........#.....#..OO..#OOO#O...O...#",".#O...#....OO#....O.#....O##.#....#.O....O.#...O..O#.#.O..#..OO#.O....O.#.OO..#....OO.O...........O.",".O.#..#.O....O.....O.O#.#....O..##....O#.OO.....#..O...#O...OO..#....#...#..OO.O#..O...OO....#O...O.",".O..#O.....##...#....#.OOO...#.....O#.#O.O#OO.......#...O..#...#O.#...O....#.O..O#........#....O.#..","......#...O.O.....O..........#..##....O#O.#....#O..OO#..O........O...###.#.#..O.#O#OO#.##.OO...#O.O.",".#....OO.O...O.O##.##...OO.###..O#OO#.O..#O##.OO......#.OO#.#..OOO...O..O#OO...OO#O..O#....O.OO..OOO","...#O.....#OO.....O.O.#O.O..O......#O#O..O....O.O.OO.......#OO#..#..#..O...#.#O...O.O..O.......O..##",".#.O...O....O...#O.#O..O...#..O...#OOO#...O#...O.#O..##.#...##..#O.......O.#..#.O##....O.#..........","...O.#.....#..OO...O...O...O.O.#O...O.#.O..#.##.O.O......#...##.#O.#..##O.......#.O..#.OO.OO...#.OOO","......O..#.#O..OO.#..O#..O#...OO#......#.#.....##.#.#..#O##.O#O..#.#...#....O..#.O.#..O...OOOO.O#.O.","...OO..#.#..O.O..OO.O.O..O........#.O#O.#.O.O..O..O....O...#..O#.##..OO.......O.O.OO..O.O..O...#...O",".O......#....#.........#.....#.....#.....O.O.OO.O.#..O...OO#O....O##..O#O...#O.#........#OO..#..#OOO","....#..#..#.#O......OO.O.OO#....##.#..#...O..O......O.##.#.O#OO.#.............OO..#...#....OO..O#.#.","#....O#O#..OO..O.O#..........#O..O.#OO..#.O.#...O...#...O.#.#..#.#..O...#..#...###O..#....O#.#.#....","...O#.O.#...#...O..O.O.O.O........#O.O..O.#O....OO............O.....#.......O..O...O........O#O#.#O.","O.O..O.O....#..OO......#..O.......OO.....#.O.O..O...OOO..OOO#..O...#O.#O..OO....#.#O#O.O..#O....#...",".#..##.....##...O..O..#.....##...O.#O.O...#.....O.....O.#.#O..O..#......O...O.##...O###..O#.#...#.#.",".O.#....O...O..O#......#OOOO.O#....#O.......O...#..OO...O...O.#.............OOO..##..OO......O......","...#O.....O#..#....#...#O.O......#..O..#..O#O..#O..OO#..#.#...##....#O#...OO....O.OOO.#.O##O.O.#O.#.","..O...O.#O........##.#.O.OO....#...#...O.O.....O........O.....O#...O..#.O#.O.O.....#..O...#OO.#O.O.#","O........O...#......#.......O...#O#O#O....O..OO.#...#.......O......O#O....O.##..O#O.O#O.O.....##...#","..O#...O.#..O..O.##O.#...O.#..##.##....##....O.OOO.O......#..#...#..O###.#.#.O...##.O#OO#.O.#..O#.O#","#O........#.OO...O.##.....O.O...#O.#.#.#.O.O.#...OO...#.#.O.O..##..O#...O#.........#.....#..#.#..#.O","#....#.....#.#...O.#.O.OO..#........O.#..OO...O.O......O..#..#....OO....#..OO.#.#....#.......O.#O..O","...#..####...OO#....O.O..O.....#O.OO....O#O..###.OO..O...#O#.O##...O..........O.##.#.OO.O....O.....#","O.#..O.O.#.#.....O....O#.....##....#.......O.#.#O....O...#....O..O..O....O.O#..O.O....OO..OOO...OOO#","O..O.#.....O#.O##O...O....O#......#....O...O#....##O.#...#...O........OO#O........O...O...O....#...#",".O#.#...OO#...O...O.#O#.O..OO....#.O....OO.#.#.O##.O....O.............#..#..OOO.O.#O.O...O#....#..#O","#O#..#.#.OO......#..O....##.....O.#.O...OO.O...#.#.O..#.#O.O..O.......#.O...O......O..#.OOOOO..O.#..","#OOO...#.O..#....OOO#.O...........O...#OO#.......O..#.O...O.O.#.#...#..OO.##O....#...O....#.OO.O...#",".OO.#.O..##..#.O..O....#..##......O.O..O........#.....#O..O...OO..#..OO.....O.....#...#.....OO.....O",".#.O.O...O..O.....O.#.........O..O.......O...#....O..#..#.OO.O...O.#.#..O.O....OO.#.......O......O.O",".O.....O....#..O#....O#.OO#....#...#........O......OO.O.O...O..OO.O..#.O##.......O..#..O#.##..O#.OO.","OO.......#O....OO....#....#.O..#..#..OO.#.O..O#.O.O..##O#....O.O#.....OO.#OO....O...O....O#......O#.","..#O...O.O.#O...#......O..#...#...O...OO.O.#...#O.............O#OO.#...#.#O.#.#O#O.#O....OO..##.#.O.","#....O#OO...O##.O.O.OO......O....O....#O...O.......#..##......O.#.O....#OOO......O#O#.O.#..#..#.....",".....O.O......O..#.#O.#O#..O.OO.#O.O...#..##.......#O..#O..O.....O...O.......O.O#.#.O..#...O.O.#..O.","........O...O.....#.O....#O..##O##..#..O...#...O.O.O##.....O.....#...............#O...O..#O...O.#O.#","..O.O#...#.#O..#..#O...#......#OOOO.....OO.....##...OO..#.O##.OO....OO..#..#.#.#..O.#.O#.........#..","..O.##.##......#O.##...#......O.#...###.......#O.#.O.....#....#.......O...........OO.#O#...#.##.O#O.","#..#O.#O.....O......O.#.O#....#.###..O..#......O.O#..O#O.#O..O#O...O.#....O..O.OO.#OO.........O#.#..")


