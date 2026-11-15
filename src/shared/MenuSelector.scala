package moleTerritory

class MenuSelector() {
  var players = 2
  var lastPlayers = 0
  var health = 5
  var lastHealth = 0
  var level = 1
  var lastLevel = 0


  def update(window: BlockWindow): Unit = {
    import GameProperties.windowSize.*
    import GameProperties.Color
    if (players != lastPlayers || health != lastHealth || level != lastLevel) {
      lastPlayers = players
      lastHealth = health
      lastLevel = level

      window.setRectangle(Pos(padLef, padTop))(size.tuple)(Color.white)
      window.write("Players:", Pos(padLef+((width*1.35)/4).toInt, padTop+(height/10).toInt), Color.yellow, blockSize*4)
      for ((label, index) <- Array("TWO", "THREE", "FOUR").zip((2 to 4).toArray)) {
        val offset = label.length()*10
        if (index == players) {
          if (level == 1) {
            window.setRectanglePixel(Pos(blockSize*(padLef+((width*(index-1))/4).toInt-1)-offset, blockSize*(padTop+(height/3).toInt-1)-1))(label.length()*25+blockSize*2, 55)(Color.yellow)
            window.setRectanglePixel(Pos(blockSize*(padLef+((width*(index-1))/4).toInt-1)-offset+5, blockSize*(padTop+(height/3).toInt-1)-1+5))(label.length()*25+blockSize*2-2*5, 55-2*5)(Color.white)
          }
          window.writePixel(label, Pos(blockSize*(padLef+((width*(index-1))/4).toInt)-offset, (padTop+(height/3).toInt-1)*blockSize), Color.yellow, blockSize*2)
        } else {
          window.writePixel(label, Pos(blockSize*(padLef+((width*(index-1))/4).toInt)-offset, (padTop+(height/3).toInt-1)*blockSize), Color.blue, blockSize*2)
        }
      }
      window.write("Health:", Pos(padLef+((width*1.5)/4).toInt, padTop+(height*4.5/10).toInt), Color.yellow, blockSize*4)
      if (level == 2) {
        window.setRectanglePixel(Pos(blockSize*(padLef+((width)/2).toInt-1), blockSize*(padTop+((height*2)/3).toInt-1)-1))(health.toString().length()*25+blockSize*2, 55)(Color.yellow)
        window.setRectanglePixel(Pos(blockSize*(padLef+((width)/2).toInt-1)+5, blockSize*(padTop+((height*2)/3).toInt-1)-1+5))(health.toString().length()*25+blockSize*2-2*5, 55-2*5)(Color.white)
        window.writePixel(health.toString(), Pos(blockSize*(padLef+((width)/2).toInt), (padTop+((height*2)/3).toInt-1)*blockSize), Color.yellow, blockSize*2)
      } else {
        window.writePixel(health.toString(), Pos(blockSize*(padLef+((width)/2).toInt), (padTop+((height*2)/3).toInt-1)*blockSize), Color.blue, blockSize*2)
      }

      // INFO button, centered under the health value
      val infoLabel = "INFO"
      val infoX = blockSize*(padLef+width/2) - (infoLabel.length()*blockSize*2*0.6).toInt/2
      val infoY = blockSize*(padTop+height*4/5)
      if (level == 3) {
        window.setRectanglePixel(Pos(infoX-blockSize, infoY-1))(infoLabel.length()*25+blockSize*2, 55)(Color.yellow)
        window.setRectanglePixel(Pos(infoX-blockSize+5, infoY-1+5))(infoLabel.length()*25+blockSize*2-2*5, 55-2*5)(Color.white)
        window.writePixel(infoLabel, Pos(infoX, infoY), Color.yellow, blockSize*2)
      } else {
        window.writePixel(infoLabel, Pos(infoX, infoY), Color.blue, blockSize*2)
      }

      // how to navigate the menu
      val noticeSize = 22
      val noticeLines = Array(
        "W/S or Up/Down: select   A/D or Left/Right: change",
        "ENTER: start game   I or INFO: rules and controls",
      )
      for ((line, row) <- noticeLines.zipWithIndex) {
        val lineX = blockSize*(padLef+width/2) - (line.length()*noticeSize*0.6).toInt/2
        window.writePixel(line, Pos(lineX, blockSize*(padTop+height-4) + row*(noticeSize+8)), Color.bgGray500, noticeSize)
      }
    }
  }
  /** Makes the next update redraw the whole menu, e.g. after returning from the info screen */
  def invalidate(): Unit = {
    lastPlayers = 0
    lastHealth = 0
    lastLevel = 0
  }
  def reset(): Unit = {
    players = 2
    lastPlayers = 0
    health = 5
    lastHealth = 0
    level = 1
    lastLevel = 0
  }
}