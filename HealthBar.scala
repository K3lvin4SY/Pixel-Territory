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
    var line = 0
    var offset = 0
    for (_ <- 0 to (lives - deaths)-1) {
      val offsetFactorX = 6
      val offsetFactorY = 4
      val n = 3
      if ((offset%n)*offsetFactorX+(line%2)*(offsetFactorX/2).toInt > (n-1)*offsetFactorX) {
        line += 1
        offset = 0
      }
      drawHeart(
        x+(offset%n)*offsetFactorX+(line%2)*(offsetFactorX/2).toInt,
        y+line*offsetFactorY
      )
      if ((offset%n)*offsetFactorX+(line%2)*(offsetFactorX/2).toInt >= (n-1)*offsetFactorX) {
        line += 1
      }
      offset += 1
    }
  }
  def getLines: Int = {
    import GameProperties.windowSize.blockSize
    import GameProperties.lives

    var line = 0
    var offset = 0
    val offsetFactorX = 6
    val offsetFactorY = 4
    val n = 3
    for (_ <- 0 to (lives)-1) {
      if ((offset%n)*offsetFactorX+(line%2)*(offsetFactorX/2).toInt > (n-1)*offsetFactorX) {
        line += 1
        offset = 0
      }
      if ((offset%n)*offsetFactorX+(line%2)*(offsetFactorX/2).toInt >= (n-1)*offsetFactorX) {
        line += 1
      }
      offset += 1
    }
    line
  }
}