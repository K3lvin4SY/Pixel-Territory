package moleTerritory

class StatsPanel(mole: Mole)(x: Int, y: Int) {
  import GameProperties.windowSize;
  import GameProperties.Color.*;
  val areaBar = new ProgressBar((x, y+3*4), windowSize.padLef-4, 4, backgroundEdge, background, mole.areaColor)
  val healthBar = new HealthBar(x, y+3*2)(mole.areaColor)

  var lastAreaLength = 0;
  var lastMoleKills = 0;
  var lastMoleSuicides = 0;
  var lastMoleDeaths = 0;

  def update(window: BlockWindow): Unit = {
    if (anyValueChanged) {
      lastAreaLength = mole.area.length
      lastMoleKills = mole.kills
      lastMoleSuicides = mole.suicide
      lastMoleDeaths = mole.deaths
      //println("Updated: "+mole.name)
      import GameProperties.Color;
      import window.windowSize.*;
      window.setRectangle(x, y)(padLef-3, 3*5+4*2)(Color.background)
      window.write(mole.name + " MOLE", (x, y), Color.white, blockSize*2)
      //window.write("Area: " + mole.area.length, (x, y+3*1), Color.white, (blockSize*1.5).toInt)
      window.write("Kills: " + mole.kills, (x, y+3*1), Color.white, (blockSize*1.5).toInt)
      //window.write("Suicides: " + mole.suicide, (x, y+3*3), Color.white, (blockSize*1.5).toInt)
      //window.write("Deaths: " + mole.deaths, (x, y+3*4), Color.white, (blockSize*1.5).toInt)
      healthBar.update(mole.deaths)(window)
      areaBar.update(mole.area.length)(x, y+3*2+4*(healthBar.getLines))(window)
    }
  }

  def anyValueChanged: Boolean = {
    ((mole.area.length != lastAreaLength) || (mole.kills != lastMoleKills) || (mole.suicide != lastMoleSuicides) || (mole.deaths != lastMoleDeaths))
  }
}