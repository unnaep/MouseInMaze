import scala.util.Random
import scala.util.Using
import java.io.{BufferedWriter, FileWriter}

//Handles the generating of the maze and solving it. 

class Maze(val width: Int, val height: Int) {

  if (width <= 0 || height <= 0) {
    throw new IllegalArgumentException("Width and height must be positive integers.")
  }

  val grid: Array[Array[Char]] = Array.fill(height, width)('#')
  var visited: Set[(Int, Int)] = Set()
  val rand = new Random
  
  
  def solveDFS(start: (Int, Int), goal: (Int, Int)): List[(Int, Int)] = {
    def dfs(path: List[(Int, Int)], visited: Set[(Int, Int)]): List[(Int, Int)] = path.lastOption match {
      case Some(current) if current == goal => path
      case Some(current) =>
        val moves = List((0, -1), (0, 1), (-1, 0), (1, 0))
        moves.flatMap { case (dx, dy) =>
          val next = (current._1 + dx, current._2 + dy)
          if (next._1 >= 0 && next._1 < width && next._2 >= 0 && next._2 < height &&
            !visited.contains(next) && (grid(next._2)(next._1) == ' ')) {
            val result = dfs(path :+ next, visited + next)
            if (result.nonEmpty) return result
          }
          None
        }
        List.empty
      case None => List.empty
    }
    dfs(List(start), Set(start))
  }

  def generateMaze(): Unit = {
    def carve(x: Int, y: Int): Unit = {
      val directions = List((0, -2), (0, 2), (-2, 0), (2, 0))
      val shuffledDirections = Random.shuffle(directions)

      shuffledDirections.foreach { case (dx, dy) =>
        val nx = x + dx
        val ny = y + dy
        if (ny >= 0 && ny < height && nx >= 0 && nx < width && !visited.contains((nx, ny))) {
          grid(y + dy / 2)(x + dx / 2) = ' '
          grid(ny)(nx) = ' '
          visited += ((nx, ny))
          carve(nx, ny)
        }
      }
    }
    // Starting the carving of paths from a random square
    val startX = rand.nextInt(width / 2) * 2 + 1
    val startY = rand.nextInt(height / 2) * 2 + 1
    visited += ((startX, startY))
    grid(startY)(startX) = ' '
    carve(startX, startY)
  }

  // For the File Handling
  def saveToFile(filename: String): Unit =
    Using(new BufferedWriter(new FileWriter(filename))) { writer =>
      grid.foreach { row =>
        writer.write(row.mkString + "\n")
      }
    }.getOrElse(throw new Exception("Failed to save maze to file."))

  def displayMaze(): Unit = {
    grid.map(_.mkString("")).foreach(println)
  }

  //Helper for finding the center free square to position player.. let's move these from main to maze for clarity
  def findCenterPath(maze: Maze): (Int, Int) =
    val centerX = maze.width / 2
    val centerY = maze.height / 2
    (centerY - 1 to centerY + 1).flatMap(y =>
      (centerX - 1 to centerX + 1).map(x => (x, y))
    ).find {
      case (x, y) => maze.grid(y)(x) == ' '
    }.getOrElse((centerX, centerY))

  // Helper for finding a random path-cell for the goal
  def findRandomPath(maze: Maze): (Int, Int) =
    val paths = for {
      y <- 0 until maze.height
      x <- 0 until maze.width
      if maze.grid(y)(x) == ' '
    } yield (x, y)

    Random.shuffle(paths.toList).head

  // Helper for first finding a random wall-cell for the elevator, but then changing it to a '^' marked elevator cell so that it can be moved to by the player
  def findRandomWall(maze: Maze): (Int, Int) =
    val walls = for
      y <- 0 until maze.height
      x <- 0 until maze.width
      if maze.grid(y)(x) == '#'
    yield (x, y)
    val selectedWallOption = Random.shuffle(walls.toList).headOption
    selectedWallOption match {
      case Some((x, y)) =>
        maze.grid(y)(x) = '^' // Replacing the selected wall with an elevator indicator
        (x, y) // Returning the coordinates of the wall that was replaced
      case None =>
        throw new NoSuchElementException("No walls available to place an elevator.")
    }
  }




