package moleTerritory
import java.awt.{Color as JColor}

case class KeyControl(left: String, right: String, up: String, down: String) {
  def direction(key: String): Dir = {
    if (key == left) {
      West
    } else if (key == right) {
      East
    } else if (key == up) {
      North
    } else if (key == down) {
      South
    } else {
      Pole
    }
  }
  def has(key: String): Boolean = {
    Array(left, right, up, down).contains(key)
  }
}

// function for getting all the moles positions in one array
def getPosFromArray(players: Array[Player]): Array[Pos] = {
  players.map(_.mole.pos)
}

def combineColors(color1: JColor, color2: JColor): JColor = {
  val combinedRed = (color1.getRed + color2.getRed) / 2
  val combinedGreen = (color1.getGreen + color2.getGreen) / 2
  val combinedBlue = (color1.getBlue + color2.getBlue) / 2
  
  new JColor(combinedRed, combinedGreen, combinedBlue)
}

def getcolorFactor(colorFactor: JColor, colorResult: JColor): JColor = {
  val combinedRed= colorResult.getRed *2 - colorFactor.getRed
  val combinedGreen = colorResult.getGreen *2 - colorFactor.getGreen
  val combinedBlue = colorResult.getBlue *2 - colorFactor.getBlue

  //println("("+combinedRed+", "+combinedGreen+", "+combinedBlue+")")
  if (combinedRed < 0 || combinedGreen < 0 || combinedBlue < 0 ||
    combinedRed > 255 || combinedGreen > 255 || combinedBlue > 255
  ) {
    new JColor(255, 255, 255)
  } else {
    new JColor(combinedRed, combinedGreen, combinedBlue)
  }
}

def getTouchingPoses(pos: Pos): Array[Pos] = {
  Array(pos+North, pos+East, pos+South, pos+West)
}

def arePosClose(pos1: Pos, pos2: Pos, distance: Int): Boolean = {
  val xDiff = (pos2.x - pos1.x).abs
  val yDiff = (pos2.y - pos1.y).abs
  (distance >= xDiff && distance >= yDiff)
}

def arePosCloseToAnyTerritory(pos: Pos)(area: Set[Pos], distance: Int): Boolean = {
  val windowSize = GameProperties.windowSize

  var loopQuit = false
  for (dir <- Dir(-distance, -distance) to Dir(distance, distance)) if (!loopQuit) {

    if (!windowSize.isPosOutOfBounds(pos+dir) && area.contains(pos+dir)) {
      loopQuit = true
    }
  }

  loopQuit
}
/** All poses inside the playing field that are not within distance of any pos in occupied */
def findFreePoses(occupied: Set[Pos], distance: Int): List[Pos] = {
  import GameProperties.windowSize.*
  (padLef + 1 to padLef + width - 2).flatMap { xPos =>
    (padTop + 1 to padTop + height - 2).map { yPos =>
      Pos(xPos, yPos)
    }.filter { pos =>
      !occupied.contains(pos) && !arePosCloseToAnyTerritory(pos)(occupied, distance)
    }
  }.toList
}

def arePosCloseToTerritory(window: BlockWindow, pos: Pos, distance: Int): Boolean = {
  var suroundingColors = Array.empty[JColor]
  for (dir <- Dir(-distance, -distance) to Dir(distance, distance)) {
    if (!window.windowSize.isPosOutOfBounds(pos + dir)) {
      suroundingColors :+= window.getBlock(pos + dir)
    } else {
      suroundingColors :+= JColor.black
    }
  }
  suroundingColors.map(_ == JColor.white).contains(false)
}