package moleTerritory

class StatsPanel(player: Player)(x: Int, y: Int) {
  import GameProperties.windowSize;
  import GameProperties.Color.*;
  val areaBar = new ProgressBar(Pos(x, y+3*4), windowSize.padLef-4, 4, backgroundEdge, background, player.areaColor)
  val healthBar = new HealthBar(Pos(x, y+3*2))(player.areaColor)

  var lastAreaLength = 0;
  var lastKills = 0;
  var lastSuicides = 0;
  var lastDeaths = 0;

  def update(window: BlockWindow): Unit = {
    if (anyValueChanged) {
      lastAreaLength = player.area.size
      lastKills = player.kills
      lastSuicides = player.suicide
      lastDeaths = player.deaths
      //println("Updated: "+player.name)
      import GameProperties.Color;
      import window.windowSize.*;
      window.setRectangle(Pos(x, y))(padLef-3, 3*5+4*2)(Color.background)
      window.write(player.name + " MOLE", Pos(x, y), Color.white, blockSize*2)
      //window.write("Area: " + player.area.length, (x, y+3*1), Color.white, (blockSize*1.5).toInt)
      window.write("Kills: " + player.kills, Pos(x, y+3*1), Color.white, (blockSize*1.5).toInt)
      //window.write("Suicides: " + player.suicide, (x, y+3*3), Color.white, (blockSize*1.5).toInt)
      //window.write("Deaths: " + player.deaths, (x, y+3*4), Color.white, (blockSize*1.5).toInt)
      healthBar.update(player.deaths)(window)
      areaBar.update(player.area.size)(x, y+3*2+4*(healthBar.getLines))(window)
    }
  }

  def anyValueChanged: Boolean = {
    ((player.area.size != lastAreaLength) || (player.kills != lastKills) || (player.suicide != lastSuicides) || (player.deaths != lastDeaths))
  }
}