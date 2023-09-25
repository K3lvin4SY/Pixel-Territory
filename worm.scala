package blockbattle
import java.awt.{Color as JColor}

class Worm(pos: Pos, color: JColor, val energyWorth: Int) extends GameObject(pos, color) {
  val teleportProbability = 0.001

  def claim(mole: Mole): Unit = {
    mole.modifyEnergy(energyWorth);
  }

  def randomTeleport(bannedPos: Array[Pos]): Unit = {
    if (math.random() < teleportProbability) {
      move(bannedPos)
    }
  }

  override def drawUpdate(window: BlockWindow, moles: Array[Mole]): Unit = {
    for (mole <- moles) {
      if (isHere(mole.pos)) {
        claim(mole)
        move(getPosFromArray(moles))
      } else {
        import GameProperties.Color.soil
        window.setBlock(getObjPos())(soil)
        randomTeleport(getPosFromArray(moles))
      }
    }
    window.setBlock(getObjPos())(color);
  }
}