//Manages the game's logic, user interactions, and ensures that the rules of the game are followed
//accordingly.

class GameEngine(val maze: Maze, var player: Player, val goal: (Int, Int), val elevator: (Int, Int), onElevatorReached: () => Unit) {
  def movePlayer(direction: String, onWin: () => Unit): Unit = {
    val (dx, dy) = direction match {
      case "UP" => (0, -1)
      case "DOWN" => (0, 1)
      case "LEFT" => (-1, 0)
      case "RIGHT" => (1, 0)
    }
    val newX = player.x + dx
    val newY = player.y + dy
    // Checking if the new position is within bounds and not a wall, or if it's an elevator
    if ((newX >= 0 && newX < maze.width && newY >= 0 && newY < maze.height) &&
      (maze.grid(newY)(newX) == ' ' || maze.grid(newY)(newX) == '^')) {
      player.x = newX
      player.y = newY

      // Handling if the new position is the elevator
      if (maze.grid(newY)(newX) == '^') {
        onElevatorReached() 
      }
      // Handling if the new position is the goal
      if (newX == goal._1 && newY == goal._2) {
        onWin()
      }
    }
  }
}