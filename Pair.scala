package moleTerritory

trait Pair[T]:
  def x: T
  def y: T
  def tuple: (T, T) = (x, y)


case class Pos private (x: Int, y: Int) extends Pair[Int]:

  def +(p: Pair[Int]): Pos =
    copy(x + p.x, y + p.y)

  def -(p: Pair[Int]): Pos =
    copy(x - p.x, y - p.y)
    
object Pos:
  def apply(x: Int, y: Int): Pos =
    new Pos(x, y)
  def apply(pos: (Int, Int)): Pos =
    new Pos(pos._1, pos._2)

object Dir:
  def apply(x: Int, y: Int): Dir =
    new Dir(x, y)
  def apply(dir: (Int, Int)): Dir =
    new Dir(dir._1, dir._2)
      
case class Dir(val x: Int, val y: Int) extends Pair[Int]:
  def +(p: Pair[Int]): Dir =
    copy(x + p.x, y + p.y)

  def -(p: Pair[Int]): Dir =
    copy(x - p.x, y - p.y)
  
  def reverse: Dir =
    new Dir(x*(-1), y*(-1)) 

  def to(dir: Dir): Vector[Dir] =
    var dirs = Vector.empty[Dir]
    for (xDiff <- (x min dir.x) to (x max dir.x) by (if (x < dir.x) 1 else -1)) {
      for (yDiff <- (y min dir.y) to (y max dir.y) by (if (y < dir.y) 1 else -1)) {
        dirs :+= Dir(xDiff, yDiff)
      }
    }
    dirs

def North : Dir = Dir( 0, -1)
def South : Dir = Dir( 0,  1)
def East  : Dir = Dir( 1,  0)
def West  : Dir = Dir(-1,  0)
def Pole  : Dir = Dir(0,  0)
