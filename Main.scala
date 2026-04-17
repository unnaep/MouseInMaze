import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color.*
import scalafx.scene.shape.Rectangle
import scalafx.scene.control.TextInputDialog
import scala.util.Try
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.Includes.*
import scalafx.scene.image.Image
import scalafx.scene.image.ImageView

// Running this object launches the game.

object Main extends JFXApp3:

  override def start(): Unit =
    val mazeSizeInput = new TextInputDialog(defaultValue = "21") {
      initOwner(stage)
      title = "Maze Configuration"
      headerText = "Maze Size Input"
      contentText = "Please enter the maze size:"
    }

    val result = mazeSizeInput.showAndWait()

    result match
      case Some(sizeString) =>
        val size = Try(sizeString.toInt).getOrElse(21)
        generateAndDisplayMaze(size, size)
      case None =>


  def generateAndDisplayMaze(mazeWidth: Int, mazeHeight: Int): Unit =
    val cellSize = 20
    val maze = new Maze(mazeWidth, mazeHeight)
    maze.generateMaze()

    // Saving the maze to a file
    val filename = "maze_" + System.currentTimeMillis() + ".txt"
    maze.saveToFile(filename)
    println(s"Maze saved to $filename")


    // Regenerating the maze after having moved to the elevator indicating moving to upper floor. However, this elevator is magcal and always brings you to different floors (different mazes).
    def regenerateMaze(): Unit = {
      generateAndDisplayMaze(mazeWidth, mazeHeight) // Using the current width and height
    }

    val (playerStartX, playerStartY) = maze.findCenterPath(maze)
    val player = new Player(playerStartX, playerStartY)
    val goal = maze.findRandomPath(maze)
    val elevator = maze.findRandomWall(maze)
    var gameEngine = new GameEngine(maze, player, goal, elevator, regenerateMaze)

    val root = Pane()

    for (row <- 0 until mazeHeight; col <- 0 until mazeWidth) {
      val cellFill = (col, row) match
        case _ if maze.grid(row)(col) == ' ' => White
        case (x, y) if x == elevator._1 && y == elevator._2 => Gray
        case _ => Black
      val rect = new Rectangle {
        width = cellSize
        height = cellSize
        fill = cellFill
        x = col * cellSize
        y = row * cellSize
      }
      root.children.add(rect)
    }

    def highlightPath(path: List[(Int, Int)]): Unit = {
      path.foreach { case (pathX, pathY) =>
        val pathRect = new Rectangle {
          width = cellSize
          height = cellSize
          fill = Blue
          x = pathX * cellSize
          y = pathY * cellSize
        }
        root.children.add(pathRect)
      }
    }
    

    val giveUpButton = new Button("Give Up") {
      layoutX = mazeWidth * cellSize - 80 // Positioning the button to the right
      layoutY = 10 // Positioning the button to the top
      onAction = _ => {
        val path = maze.solveDFS((player.x, player.y), goal)
        highlightPath(path: List[(Int, Int)])
      }
    }
    root.children.add(giveUpButton)
    

    //Defining player look and position
    val playerRect = new Rectangle {
      width = cellSize
      height = cellSize
      x = player.x * cellSize
      y = player.y * cellSize
      fill = HotPink
    }
    root.children.add(playerRect)


    //Defining goal look and position
    val goalRect = new Rectangle {
      width = cellSize
      height = cellSize
      x = goal._1 * cellSize
      y = goal._2 * cellSize
      fill = Green
    }
    root.children.add(goalRect)


    //Defining elevator look and position
    val elevatorImage = new Image("elevator.png")
    val elevatorImageView = new ImageView(elevatorImage) {
      fitWidth = cellSize
      fitHeight = cellSize
      x = elevator._1 * cellSize
      y = elevator._2 * cellSize
    }
    root.children.add(elevatorImageView)


    stage = new JFXApp3.PrimaryStage {
      title.value = "Mouse In Maze"
      width = mazeWidth * cellSize
      height = mazeHeight * cellSize + 25
      scene = new Scene(root)
    }


    // Winning
    def onWin(): Unit = {
      scalafx.application.Platform.runLater {
        val alert = new scalafx.scene.control.Alert(scalafx.scene.control.Alert.AlertType.Information) {
          initOwner(stage)
          title = "Game Over"
          headerText = "Congratulations!"
          contentText = "You've reached the goal and won the game!"
        }.showAndWait()
      }
    }

    def winConditionCheck(direction: String): Unit = {
      gameEngine.movePlayer(direction, onWin)
    }


    //Key event handlers
    root.requestFocus() // First making sure the root pane can receive the key events
    root.onKeyPressed = (keyEvent: KeyEvent) => {
      keyEvent.code match {
        case KeyCode.Up => winConditionCheck("UP")
        case KeyCode.Down => winConditionCheck("DOWN")
        case KeyCode.Left => winConditionCheck("LEFT")
        case KeyCode.Right => winConditionCheck("RIGHT")
        case _ =>
      }
      updateUI() // Updating the UI to reflect the player's new position
    }
    

    def updateUI(): Unit = {
      root.children.clear() // Clearing the previous UI elements
      // Re-drawing the maze, player, and goal
      for (row <- 0 until mazeHeight; col <- 0 until mazeWidth) {
        val cellNode = (col, row) match {
          case _ if maze.grid(row)(col) == ' ' =>
            new Rectangle {
              width = cellSize
              height = cellSize
              fill = White
              x = col * cellSize
              y = row * cellSize
            }

          case (elevatorX, elevatorY) if elevatorX == elevator._1 && elevatorY == elevator._2 =>
            new ImageView {
              image = elevatorImage
              fitWidth = cellSize
              fitHeight = cellSize
              x = elevatorX * cellSize
              y = elevatorY * cellSize
            }

          case _ =>
            new Rectangle {
              width = cellSize
              height = cellSize
              fill = Black // Walls
              x = col * cellSize
              y = row * cellSize
            }
        }

        root.children.add(cellNode)
      }
      // Drawing the player again
      val playerRect = new Rectangle {
        width = cellSize
        height = cellSize
        x = player.x * cellSize
        y = player.y * cellSize
        fill = HotPink // Player color
      }
      root.children.add(playerRect)

      // Drawing the goal again
      val goalRect = new Rectangle {
        width = cellSize
        height = cellSize
        x = goal._1 * cellSize
        y = goal._2 * cellSize
        fill = Green // Goal color
      }
      root.children.add(goalRect)

      root.children.add(giveUpButton) // Re-adding the button due to clearing

      root.requestFocus() // Ensuring that the Pane can still receive the key events after changes in players position
    }

end Main
