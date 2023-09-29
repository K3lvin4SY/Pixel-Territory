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