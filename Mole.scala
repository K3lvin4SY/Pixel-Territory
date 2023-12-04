package moleTerritory
import java.awt.{Color as JColor}

class Mole(
  val name: String,
  var dir: Dir,
  val color: java.awt.Color,
  val areaColor: java.awt.Color,
  val keyControl: KeyControl,
  val player: Player,
  val game: Game
) extends Entity {
  var area = Array.empty[Pos]
  var lastDir = Pole
  var pos = Pos(-5, -5)
  var currentPath = Array.empty[Pos]
  var currentPathColor = Array.empty[JColor]
  var prevColor = java.awt.Color.white
  override def toString: String = {
    s"Mole[pos=$pos, dir=$dir]"
  }
  def addArea(poses: Array[Pos]): Unit = {
    area ++= poses;
    game.moleWillClaimPos(poses, this)
  }
  def removeArea(pos: Pos): Unit = {
    area = area.filter(_!=pos)
  }
  def killed(mole: Mole): Unit = {
    if (mole != this) {
      player.kills += 1
    } else {
      player.suicide += 1
    }
    mole.player.deaths += 1
    if (GameProperties.lives-mole.player.deaths <= 0) {
      mole.player.eliminated = true
    }
  }

  def die(window: BlockWindow): Unit = {
    for ((pos, color) <- currentPath.zip(currentPathColor)) {
      //val colorResult = window.getBlock(pos)
      //window.setBlock(pos)(getcolorFactor(areaColor, colorResult))
      window.setBlock(pos)(color)
    }
    currentPath = Array.empty[Pos]
    currentPathColor = Array.empty[JColor]
    //CODE123window.setBlock(pos)(prevColor)
    lastDir = Pole
    dir = Pole
    spawn(window)
  }

  /** Om keyControl.has(key) så uppdateras riktningen dir enligt keyControl */
  def setDir(key: String): Unit = {
    if (keyControl.has(key) || key == "FORCE") {
      val tempDir = Dir(keyControl.direction(key));
      if (tempDir != dir.reverse && tempDir != makeReverseDir(lastDir)) {
        dir = tempDir
        if (tempDir != Pole) {
          lastDir = tempDir
        }
      }
    }
  }
  /** Uppdaterar dir till motsatta riktningen. */
  def reverseDir(): Unit = {
    dir = dir.reverse
  }
  def makeReverseDir(dirToReverse: Dir): Dir = {
    Dir(dirToReverse.x * -1, dirToReverse.y * -1)
  }
  /** Uppdaterar pos så att den blir nextPos */
  def move(): Unit = {
    pos = nextPos;
    if (prevColor != areaColor) {
      if (!currentPath.contains(pos) && !area.contains(pos)) {
        currentPath :+= pos;
        currentPathColor :+= prevColor;
        //println("added: "+pos)
      }
    }
  }

  def update(): Unit = move()
  /** Ger nästa position enligt riktningen dir utan att uppdatera pos */
  def nextPos: Pos = {
    pos+dir
  }

  // checks if mole is out of bounds
  def isMoleOutOfBounds(): Unit = {
    import GameProperties.*
    if (isPosOutOfPlayingField(this.nextPos)) {
      setDir("FORCE");
      //reverseDir();
    }
  }

  def getNewRandomPos(): Pos = {
    import scala.util.Random.nextInt
    import GameProperties.windowSize.*
    val xPos = nextInt(width-2)+1
    val yPos = nextInt(height-2)+1;
    Pos(xPos, yPos)
  }
}

