package blockbattle
import java.awt.{Color as JColor}

class Gem(pos: Pos, color: JColor, val pointWorth: Int) extends GameObject(pos, color) {
  def claim(mole: Mole): Unit = {
    mole.modifyPoints(pointWorth);
  }

  override def drawUpdate(window: BlockWindow, moles: Array[Mole]): Unit = {
    for (mole <- moles) {
      if (isHere(mole.pos)) {
        claim(mole)
        move(getPosFromArray(moles))
      } else {
        import GameProperties.Color.soil
        window.setBlock(getObjPos())(soil)
      }
    }
    window.setBlock(getObjPos())(color);
  }
}