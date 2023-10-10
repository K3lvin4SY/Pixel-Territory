package moleTerritory

class MenuSelector() {
  var players = 2
  var lastPlayers = 0

  def update(window: BlockWindow): Unit = {
    import GameProperties.windowSize.*
    import GameProperties.Color
    if (players != lastPlayers) {
      lastPlayers = players

      window.setRectangle(padLef, padTop)(size)(Color.white)
      window.write("Players:", (padLef+((width*1.2)/4).toInt, padTop+(height/10).toInt), Color.yellow, blockSize*4)
      for ((label, index) <- Array("TWO", "THREE", "FOUR").zip((2 to 4).toArray)) {
        val offset = label.length()*10
        if (index == players) {
          window.setRectanglePixel(blockSize*(padLef+((width*(index-1))/4).toInt-1)-offset, blockSize*(padTop+(height/2).toInt-1)-1)(label.length()*25+blockSize*2, 55)(Color.yellow)
          window.setRectanglePixel(blockSize*(padLef+((width*(index-1))/4).toInt-1)-offset+5, blockSize*(padTop+(height/2).toInt-1)-1+5)(label.length()*25+blockSize*2-2*5, 55-2*5)(Color.white)
          window.writePixel(label, (blockSize*(padLef+((width*(index-1))/4).toInt)-offset, (padTop+(height/2).toInt-1)*blockSize), Color.yellow, blockSize*2)
        } else {
          window.writePixel(label, (blockSize*(padLef+((width*(index-1))/4).toInt)-offset, (padTop+(height/2).toInt-1)*blockSize), Color.blue, blockSize*2)
        }
      }

    }
  }
  def reset(): Unit = {
    players = 2
    lastPlayers = 0
  }
}