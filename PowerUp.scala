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
      val notPossiblePoses = game.players
      .foldLeft(Set.empty[Pos]) { (accumulatedPoses, player) =>
        accumulatedPoses ++ player.area
      }
  
      val possiblePoses = findFreePoses(notPossiblePoses, 3)
  
      if (possiblePoses.length != 0) { // no spots left to be spawned (NOT)
        import scala.util.Random.nextInt
        pos = possiblePoses(nextInt(possiblePoses.length))
  
        window.setBlock(pos)(color)
      }
  }
}