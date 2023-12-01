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

abstract sealed class Dir(val x: Int, val y: Int) extends Pair[Int]


case object North extends Dir( 0, -1)
case object South extends Dir( 0,  1)
case object East  extends Dir( 1,  0)
case object West  extends Dir(-1,  0)
