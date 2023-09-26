package moleTerritory

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
    if (Array(left, right, up, down).contains(key)) {
      true;
    } else {
      false;
    }
  }
}

// function for getting all the moles positions in one array
def getPosFromArray(moles: Array[Mole]): Array[Pos] = {
  var positions: Array[Pos] = Array()
  for (mole <- moles) {
    positions = positions :+ mole.pos
  }
  positions
}