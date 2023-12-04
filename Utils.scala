package moleTerritory
import java.awt.{Color as JColor}

case class KeyControl(left: String, right: String, up: String, down: String) {
  def direction(key: String): (Int, Int) = {
    if (key == left) {
      (-1, 0);
    } else if (key == right) {
      (1, 0);
    } else if (key == up) {
      (0, -1);
    } else if (key == down) {
      (0, 1);
    } else {
      (0, 0);
    }
  }
  def has(key: String): Boolean = {
    Array(left, right, up, down).contains(key)
  }
}

// function for getting all the moles positions in one array
def getPosFromArray(moles: Array[Mole]): Array[Pos] = {
  moles.map(_.pos)
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