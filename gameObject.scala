package blockbattle
import java.awt.{Color as JColor}

class GameObject(
  var pos: Pos,
  val color: JColor
) {
  def isHere(gameObjPos: Pos): Boolean = {
    //println(gameObjPos)
    //println(pos)
    (gameObjPos == pos)
  }
  def getObjPos(): Pos = {
    pos
  }
  def move(): Unit = {
    pos = getNewRandomPos()
  }
  def move(bannedPos: Array[Pos]): Unit = {
    while
      pos = getNewRandomPos()
      bannedPos.contains(pos)
    do ()
  }
  def getNewRandomPos(): Pos = {
    import scala.util.Random.nextInt
    import GameProperties.windowSize.*
    import GameProperties.grassRange
    val xPos = nextInt(width)
    val yPos = nextInt(height-(grassRange.last+1)) + (grassRange.last+1);
    (xPos, yPos)
  }

  def drawUpdate(window: BlockWindow, moles: Array[Mole]): Unit = {
    for (mole <- moles) {
      if (isHere(mole.pos)) {
        move(getPosFromArray(moles))
      } else {
        import GameProperties.Color.soil
        window.setBlock(pos)(soil)
      }
    }
    window.setBlock(pos)(color);
  }
}