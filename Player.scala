package moleTerritory
import java.awt.{Color as JColor}

class Player(
  val name: String,
  moleColor: java.awt.Color,
  val areaColor: java.awt.Color,
  val keyControl: KeyControl,
  val game: Game
) {
  val mole = new Mole(moleColor, this)
  var area: Array[Pos] = Array.empty[Pos]
  var kills = 0
  var suicide = 0
  var deaths = 0
  var eliminated = false
  override def toString: String = {
    s"Player[name=$name, points=$area]"
  }
  def addArea(poses: Array[Pos]): Unit = {
    area ++= poses;
    game.moleWillClaimPos(poses, this)
  }
  def removeArea(pos: Pos): Unit = {
    area = area.filter(_!=pos)
  }
  def killed(victim: Player): Unit = {
    if (victim != this) {
      kills += 1
    } else {
      suicide += 1
    }
    victim.deaths += 1
    if (GameProperties.lives-victim.deaths <= 0) {
      victim.eliminated = true
    }
  }

  def die(window: BlockWindow, otherPlayers: Array[Player]): Unit = {
    import GameProperties.Color
    for (pos <- area) {
      window.setBlock(pos)(Color.gameBackground)
    }
    for (otherPlayer <- otherPlayers) {
      val otherMole = otherPlayer.mole
      for (otherPathPos <- otherMole.currentPath) {
        if (area.contains(otherPathPos)) {
          val index = otherMole.currentPath.indexOf(otherPathPos)
          otherMole.currentPathColor(index) = Color.gameBackground
          window.setBlock(otherPathPos)(combineColors(otherPlayer.areaColor, Color.gameBackground))
        }
      }
    }
    area = Array.empty[Pos]
    mole.die(window)
    spawn(window)
  }

  /** Om keyControl.has(key) så uppdateras riktningen dir enligt keyControl */
  def setDir(key: String): Unit = mole.setDir(key)

  /** Spawnar molen och ger spelaren ett 3x3 startområde runt den */
  def spawn(window: BlockWindow): Unit = {
    if (!eliminated) {
      area = Array.empty[Pos]
      mole.spawn(window)

      if (!eliminated) {
        for (dir <- Dir(-1, -1) to Dir(1, 1)) {
          window.setBlock(mole.pos + dir)(areaColor)
          area :+= mole.pos + dir
        }
      }
    }
  }
}
