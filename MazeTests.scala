//Testing the Maze class

object MazeTests {
  def main(args: Array[String]): Unit = {
    println("Testing Maze Generation and Functionality")

    // Test 1: Creating a maze and ensuring it has the given dimensions
    val width = 10
    val height = 10
    val maze = new Maze(width, height)
    assert(maze.width == width, "Maze width is incorrect")
    assert(maze.height == height, "Maze height is incorrect")

    // Test 2: Generating the maze and checking the starting point
    maze.generateMaze()
    val (startX, startY) = (width / 2, height / 2) // Assuming start is center for this test
    assert(maze.grid(startY)(startX) == ' ', "Maze start point is not empty")

    // Test 3: Checking if finding the path from start to goal works
    val (goalX, goalY) = maze.findRandomPath(maze)
    val path = maze.solveDFS((startX, startY), (goalX, goalY))
    assert(path.nonEmpty, "No path found from start to goal")
    assert(path.head == (startX, startY) && path.last == (goalX, goalY), "Path does not start or end at the correct positions")

    println("All tests run")
  }
}
