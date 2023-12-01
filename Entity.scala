package moleTerritory

trait Entity:
  var pos: Pos

  def update(): Unit
  def spawn(window: BlockWindow): Unit