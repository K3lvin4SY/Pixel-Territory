package moleTerritory
import java.awt.{Color as JColor}

class Mole(
  val color: java.awt.Color,
  val player: Player
) extends Entity {
  var dir = Pole
  var lastDir = Pole
  var pos = Pos(-5, -5)
  var currentPath = Array.empty[Pos]
  var currentPathColor = Array.empty[JColor]
  var prevColor = java.awt.Color.white
  override def toString: String = {
    s"Mole[pos=$pos, dir=$dir]"
  }

  /** Återställer svansen och stannar molen. Spelaren sköter respawn. */
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
  }

  /** Om keyControl.has(key) så uppdateras riktningen dir enligt keyControl */
  def setDir(key: String): Unit = {
    if (player.keyControl.has(key) || key == "FORCE") {
      val tempDir = player.keyControl.direction(key);
      if (tempDir != dir.reverse && tempDir != lastDir.reverse) {
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
  /** Uppdaterar pos så att den blir nextPos */
  def move(): Unit = {
    pos = nextPos;
    if (prevColor != player.areaColor) {
      if (!currentPath.contains(pos) && !player.area.contains(pos)) {
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

  /** Placerar molen på en ledig plats långt från andras territorium */
  def spawn(window: BlockWindow): Unit = {
    // finds all the possible poses
    val notPossiblePoses = player.game.players
    .filter(_ != player)
    .foldLeft(Set.empty[Pos]) { (accumulatedPoses, otherPlayer) =>
      accumulatedPoses ++ otherPlayer.area
    }

    val possiblePoses = findFreePoses(notPossiblePoses, 3)

    if (possiblePoses.length == 0) { // no spots left to be spawned
      // cannot respawn or perma dead
      player.eliminated = true
    } else {
      import scala.util.Random.nextInt
      pos = possiblePoses(nextInt(possiblePoses.length))
    }
  }
}
