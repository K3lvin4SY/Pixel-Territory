package moleTerritory
import java.awt.{Color as JColor}

class HealthBar(pos: Pos)(fillColor: JColor) {
  def update(deaths: Int)(window: BlockWindow): Unit = {
    val (x, y) = pos
    import GameProperties.windowSize.blockSize
    import GameProperties.Color
    import GameProperties.lives

    def drawHeart(x: Int, y: Int): Unit = {
      window.setBlock(x, y)(Color.heart)
      window.setBlock(x, y+1)(Color.heart)
      window.setBlock(x+1, y+1)(Color.heart)
      window.setBlock(x+1, y+2)(Color.heart)
      window.setBlock(x+2, y)(Color.heart)
      window.setBlock(x+2, y+1)(Color.heart)
    }
    for (offset <- 0 to (lives - deaths)-1) {
      val offsetFactorX = 6
      val offsetFactorY = 4
      drawHeart(
        x+(offset%3)*offsetFactorX/*+((math.floor(offset / 3).toInt)%2)*3*/,
        y+(math.floor(offset / 3).toInt)*offsetFactorY
      )
    }
  }
}