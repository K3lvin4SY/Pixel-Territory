package moleTerritory
import java.awt.{Color as JColor}

class Mole(
  val name: String,
  var dir: (Int, Int),
  val color: java.awt.Color,
  val areaColor: java.awt.Color,
  val keyControl: KeyControl,
  val game: Game
) {
  var area = Array.empty[Pos]
  var kills = 0
  var suicide = 0
  var lastDir = (0,0)
  var pos = (-5, -5)
  var currentPath = Array.empty[Pos]
  var currentPathColor = Array.empty[JColor]
  var prevColor = java.awt.Color.white
  var eliminated = false
  override def toString: String = {
    s"Mole[name=$name, pos=$pos, dir=$dir, points=$area]"
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
      kills += 1
    } else {
      suicide += 1
    }
  }

  def die(window: BlockWindow, otherMoles: Array[Mole]): Unit = {
    for (pos <- area) {
      window.setBlock(pos)(JColor.white)
    }
    for (otherMole <- otherMoles) {
      for (otherPathPos <- otherMole.currentPath) {
        if (area.contains(otherPathPos)) {
          val index = otherMole.currentPath.indexOf(otherPathPos)
          otherMole.currentPathColor(index) = JColor.white
          window.setBlock(otherPathPos)(combineColors(otherMole.areaColor, JColor.white))
        }
      }
    }
    area = Array.empty[Pos]
    for ((pos, color) <- currentPath.zip(currentPathColor)) {
      //val colorResult = window.getBlock(pos)
      //window.setBlock(pos)(getcolorFactor(areaColor, colorResult))
      window.setBlock(pos)(color)
    }
    currentPath = Array.empty[Pos]
    currentPathColor = Array.empty[JColor]
    window.setBlock(pos)(prevColor)
    lastDir = (0, 0)
    dir = (0, 0)
    spawn(window)
  }

  /** Om keyControl.has(key) så uppdateras riktningen dir enligt keyControl */
  def setDir(key: String): Unit = {
    if (keyControl.has(key) || key == "FORCE") {
      val tempDir = keyControl.direction(key);
      if (tempDir != getReverseDir() && tempDir != makeReverseDir(lastDir)) {
        dir = tempDir
        if (tempDir != (0, 0)) {
          lastDir = tempDir
        }
      }
    }
  }
  /** Uppdaterar dir till motsatta riktningen. */
  def reverseDir(): Unit = {
    dir = getReverseDir()
  }
  def getReverseDir(): (Int, Int) = {
    (dir._1 * -1, dir._2 * -1)
  }
  def makeReverseDir(dirToReverse: (Int, Int)): (Int, Int) = {
    (dirToReverse._1 * -1, dirToReverse._2 * -1)
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
  /** Ger nästa position enligt riktningen dir utan att uppdatera pos */
  def nextPos: Pos = {
    (pos._1 + dir._1, pos._2 + dir._2);
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
    (xPos, yPos)
  }

  def spawn(window: BlockWindow): Unit = {
    area = Array.empty[Pos]
    /*while
      pos = getNewRandomPos()
      arePosCloseToTerritory(window, pos, 3)
    do()*/

    // finds all the possible poses
    var notPossiblePoses = Array.empty[Pos]
    var possiblePoses = Array.empty[Pos]
    for (otherMole <- game.moles.filter(_!=this)) {
      notPossiblePoses = notPossiblePoses ++ otherMole.area
    }
    import GameProperties.windowSize.*
    for (xPos <- padLef+1 to padLef+width-2) {
      for (yPos <- padTop+1 to padTop+height-2) {
        if (!notPossiblePoses.contains(xPos, yPos)) {
          if (!arePosCloseToAnyTerritory(xPos, yPos)(notPossiblePoses, 3)) {
            possiblePoses :+= (xPos, yPos)
          }
        }
      }
    }
    if (possiblePoses.length == 0) { // no spots left to be spawned
      // cannot respawn or perma dead
      eliminated = true
    } else {
      import scala.util.Random.nextInt
      pos = possiblePoses(nextInt(possiblePoses.length))

      for (xDiff <- -1 to 1) {
        for (yDiff <- -1 to 1) {
          window.setBlock(pos._1 + xDiff, pos._2 + yDiff)(areaColor)
          area :+= (pos._1 + xDiff, pos._2 + yDiff)
        }
      }
    }
  }
}

def arePosClose(pos1: Pos, pos2: Pos, distance: Int): Boolean = {
  val xDiff = (pos2._1 - pos1._1).abs
  val yDiff = (pos2._2 - pos1._2).abs
  (distance >= xDiff && distance >= yDiff)
}

def arePosCloseToAnyTerritory(pos: Pos)(area: Array[Pos], distance: Int): Boolean = {
  var suroundingArea = false
  for (xDiff <- -distance to distance) if (!suroundingArea) {
    for (yDiff <- -distance to distance) if (!suroundingArea) {
      import GameProperties.windowSize
      if (!windowSize.isPosOutOfBounds(pos._1 + xDiff, pos._2 + yDiff)) {
        if (area.contains(pos._1 + xDiff, pos._2 + yDiff)) {
          suroundingArea = true
        }
      } else {
        suroundingArea = true;
      }
    }
  }
  suroundingArea
}
def arePosCloseToTerritory(window: BlockWindow, pos: Pos, distance: Int): Boolean = {
  var suroundingColors = Array.empty[JColor]
  for (xDiff <- -distance to distance) {
    for (yDiff <- -distance to distance) {
      if (!window.windowSize.isPosOutOfBounds(pos._1 + xDiff, pos._2 + yDiff)) {
        suroundingColors :+= window.getBlock(pos._1 + xDiff, pos._2 + yDiff)
      } else {
        suroundingColors :+= JColor.black
      }
    }
  }
  suroundingColors.map(_ == JColor.white).contains(false)
}