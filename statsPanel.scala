package moleTerritory

class StatsPanel(mole: Mole)(x: Int, y: Int) {
  import GameProperties.windowSize;
  import GameProperties.Color.*;
  val bar = new ProgressBar((x, y+3*4), windowSize.padLef-4, 4, backgroundEdge, background, mole.areaColor)

  def update(window: BlockWindow): Unit = {
    import GameProperties.Color;
    import window.windowSize.*;
    window.setRectangle(x, y)(padLef-3, 3*5)(Color.background)
    window.write(mole.name + " MOLE", (x, y), Color.white, blockSize*2)
    window.write("Area: " + mole.area.length, (x, y+3), Color.white, (blockSize*1.5).toInt)
    window.write("Kills: " + mole.kills, (x, y+3*2), Color.white, (blockSize*1.5).toInt)
    window.write("Suicides: " + mole.suicide, (x, y+3*3), Color.white, (blockSize*1.5).toInt)
    bar.update(mole.area.length)(x, y+3*4)(window)
  }
}