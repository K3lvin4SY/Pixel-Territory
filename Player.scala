package moleTerritory
import java.awt.{Color as JColor}

class Player(
  val name: String,
  val color: java.awt.Color,
  val areaColor: java.awt.Color,
  val keyControl: KeyControl,
  val mole: Mole,
  val game: Game
) extends Entity {
  var area: Array[Pos] = Array.empty[Pos]
  var kills = 0
  var suicide = 0
  var deaths = 0
  var eliminated = false
  override def toString: String = {
    s"PLayer[name=$name, points=$area]"
  }
  def addArea(poses: Array[Pos]): Unit = {
    area ++= poses;
    game.moleWillClaimPos(poses, mole)
  }
  def removeArea(pos: Pos): Unit = {
    area = area.filter(_!=pos)
  }
  def killed(deadMole: Mole): Unit = {
    if (deadMole != mole) {
      kills += 1
    } else {
      suicide += 1
    }
    deadMole.player.deaths += 1
    if (GameProperties.lives-deadMole.player.deaths <= 0) {
      deadMole.player.eliminated = true
    }
  }

  def die(window: BlockWindow, otherMoles: Array[Mole]): Unit = {
    import GameProperties.Color
    for (pos <- area) {
      window.setBlock(pos)(Color.gameBackground)
    }
    for (otherMole <- otherMoles) {
      for (otherPathPos <- otherMole.currentPath) {
        if (area.contains(otherPathPos)) {
          val index = otherMole.currentPath.indexOf(otherPathPos)
          otherMole.currentPathColor(index) = Color.gameBackground
          window.setBlock(otherPathPos)(combineColors(areaColor, Color.gameBackground))
        }
      }
    }
    area = Array.empty[Pos]
    mole.die(window)
  }

  /** Om keyControl.has(key) så uppdateras riktningen dir enligt keyControl */
  def setDir(key: String): Unit = mole.setDir(key)

  def spawn(window: BlockWindow): Unit = {
    if (!eliminated) {
      area = Array.empty[Pos]
  
      // finds all the possible poses
      val notPossiblePoses = game.moles
      .filter(_ != this)
      .foldLeft(Set.empty[Pos]) { (accumulatedPoses, otherMole) =>
        accumulatedPoses ++ otherMole.player.area
      }
  
      import GameProperties.windowSize.*
      val possiblePoses = (padLef + 1 to padLef + width - 2).flatMap { xPos =>
        (padTop + 1 to padTop + height - 2).filter { yPos =>
          !notPossiblePoses.contains(Pos(xPos, yPos)) && !arePosCloseToAnyTerritory(Pos(xPos, yPos))(notPossiblePoses, 3)
        }.map { yPos =>
          (xPos, yPos)
        }
      }.toList
  
      if (possiblePoses.length == 0) { // no spots left to be spawned
        // cannot respawn or perma dead
        eliminated = true
      } else {
        import scala.util.Random.nextInt
        mole.pos = Pos(possiblePoses(nextInt(possiblePoses.length)))
  
        for (xDiff <- -1 to 1) {
          for (yDiff <- -1 to 1) {
            window.setBlock(Pos(mole.pos.x + xDiff, mole.pos.y + yDiff))(areaColor)
            area :+= Pos(mole.pos._1 + xDiff, mole.pos._2 + yDiff)
          }
        }
      }
    }
  }
}