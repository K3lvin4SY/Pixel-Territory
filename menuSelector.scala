package moleTerritory

class MenuSelector() {
  var players = 2
  var lastPlayers = 2

  def update(window: BlockWindow): Unit = {
    import GameProperties.windowSize.*
    import GameProperties.Color
    if (players != lastPlayers) {
      lastPlayers = players

      window.setRectangle(padLef, padTop)(size)(Color.white)
      for ((label, index) <- Array("TWO", "THREE", "FOUR").zip((2 to 4).toArray)) {
        if (index == players) {
          window.write(label, (padLef+((width*(index-1))/4).toInt, padTop+(height/2).toInt), Color.yellow, blockSize*2)
        } else {
          window.write(label, (padLef+((width*(index-1))/4).toInt, padTop+(height/2).toInt), Color.blue, blockSize*2)
        }
      }

    }
  }
  def reset(): Unit = {
    players = 2
  }
}