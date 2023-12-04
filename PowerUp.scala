package moleTerritory

import java.awt.Color

class PowerUp(
  val color: Color,
  val game: Game
) extends Entity {
  var pos = Pos(0, 0)

  def update(): Unit = ???

  def spawn(window: BlockWindow): Unit = {
  
      // finds all the possible poses
      val notPossiblePoses = game.moles
      .foldLeft(Set.empty[Pos]) { (accumulatedPoses, otherMole) =>
        accumulatedPoses ++ otherMole.area
      }
  
      import GameProperties.windowSize.*
      val possiblePoses = (padLef + 1 to padLef + width - 2).flatMap { xPos =>
        (padTop + 1 to padTop + height - 2).filter { yPos =>
          !notPossiblePoses.contains(Pos(xPos, yPos)) && !arePosCloseToAnyTerritory(Pos(xPos, yPos))(notPossiblePoses, 3)
        }.map { yPos =>
          Pos(xPos, yPos)
        }
      }.toList
  
      if (possiblePoses.length != 0) { // no spots left to be spawned (NOT)
        import scala.util.Random.nextInt
        pos = possiblePoses(nextInt(possiblePoses.length))
  
        window.setBlock(pos)(color)
      }
  }
}