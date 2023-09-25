package text

type Pos = (Int, Int)

class Superclass(
  var pos: Pos
) {
  def changePos(value: Pos): Unit = {
    pos = value
  }
  def printPos(): Unit = {
    println(pos);
  }
}

class Subclass(pos: Pos) extends Superclass(pos) {
  def printSub(): Unit = {
    println(pos)
    //pos
  }
  def changePos2(value: Pos): Unit = {
    pos = value
  }
}

@main def run() = {
  val myPos = (3, 5)
  val myPos2 = (12, 9)
  val sub = Subclass(myPos)
  sub.printPos()
  sub.printSub()
  sub.changePos2(myPos2)
  sub.printPos()
  sub.printSub()
}