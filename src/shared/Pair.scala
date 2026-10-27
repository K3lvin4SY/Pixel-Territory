package moleTerritory

trait Pair[T]:
  def x: T
  def y: T
  def tuple: (T, T) = (x, y)


case class Pos(x: Int, y: Int) extends Pair[Int]:

  def +(p: Pair[Int]): Pos =
    copy(x + p.x, y + p.y)

  def -(p: Pair[Int]): Pos =
    copy(x - p.x, y - p.y)

object Pos:
  def apply(pos: (Int, Int)): Pos =
    new Pos(pos._1, pos._2)

object Dir:
  def apply(dir: (Int, Int)): Dir =
    new Dir(dir._1, dir._2)

case class Dir(x: Int, y: Int) extends Pair[Int]:
  def +(p: Pair[Int]): Dir =
    copy(x + p.x, y + p.y)

  def -(p: Pair[Int]): Dir =
    copy(x - p.x, y - p.y)

  def reverse: Dir =
    copy(x*(-1), y*(-1))

  /** All dirs in the rectangle spanned by this and dir (inclusive) */
  def to(dir: Dir): Vector[Dir] =
    var dirs = Vector.empty[Dir]
    for (xDiff <- (x min dir.x) to (x max dir.x)) {
      for (yDiff <- (y min dir.y) to (y max dir.y)) {
        dirs :+= Dir(xDiff, yDiff)
      }
    }
    dirs

val North : Dir = Dir( 0, -1)
val South : Dir = Dir( 0,  1)
val East  : Dir = Dir( 1,  0)
val West  : Dir = Dir(-1,  0)
val Pole  : Dir = Dir( 0,  0)
