package blockbattle

class Mole(
  val name: String,
  var pos: Pos,
  var dir: (Int, Int),
  val color: java.awt.Color,
  val keyControl: KeyControl
) {
  var points = 0
  var energy = 200
  override def toString: String = {
    s"Mole[name=$name, pos=$pos, dir=$dir, points=$points]"
  }
  def modifyPoints(points: Int): Unit = {
    this.points += points;
  }
  def modifyEnergy(energy: Int): Unit = {
    this.energy += energy;
  }
  /** Om keyControl.has(key) så uppdateras riktningen dir enligt keyControl */
  def setDir(key: String): Unit = {
    if (keyControl.has(key) || key == "FORCE") {
      dir = keyControl.direction(key);
    }
  }
  /** Uppdaterar dir till motsatta riktningen. */
  def reverseDir(): Unit = {
    dir = (dir._1 * -1, dir._2 * -1)
  }
  /** Uppdaterar pos så att den blir nextPos */
  def move(): Unit = {
    //isMoleOutOfBounds();
    pos = nextPos;
  }
  /** Ger nästa position enligt riktningen dir utan att uppdatera pos */
  def nextPos: Pos = {
    (pos._1 + dir._1, pos._2 + dir._2);
  }

  // checks if mole is out of bounds
  def isMoleOutOfBounds(): Unit = {
    import GameProperties.*
    if (isPosOutOfPlayingField(this.nextPos)) {
      //setDir("FORCE");
      reverseDir();
    }
  }
}