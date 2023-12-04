package  moleTerritory
import java.awt.{Color as JColor}

class ProgressBar(
  pos: Pos,
  width: Int,
  height: Int,
  edgeColor: JColor,
  backgroundColor: JColor,
  fillColor: JColor,
) {
  /*def update(area: Int): Unit = {
    import moleTerritory.GameProperties.windowSize.height as gameHeight
    import moleTerritory.GameProperties.windowSize.width as gameWidth
    val fillAmount = (area.toFloat/(gameHeight.toFloat*gameWidth.toFloat))
    import moleTerritory.GameProperties.windowSize.blockSize
    val fillWidth = ((width-2)*blockSize*fillAmount).toInt
    
    window.setRectangle(pos)(width, height)(edgeColor)
    window.setRectangle(pos._1+1, pos._2+1)(width-2, height-2)(backgroundColor)
    window.setRectanglePixel((pos._1+1)*blockSize, (pos._2+1)*blockSize)(fillWidth, (height-2)*blockSize)(fillColor)
  }*/
  def update(area: Int)(x: Int, y: Int)(window: BlockWindow): Unit = {
    import moleTerritory.GameProperties.windowSize.height as gameHeight
    import moleTerritory.GameProperties.windowSize.width as gameWidth
    val fillAmount = (area.toFloat/(gameHeight.toFloat*gameWidth.toFloat))
    import moleTerritory.GameProperties.windowSize.blockSize
    val fillWidth = ((width-2)*blockSize*fillAmount).toInt

    window.setRectangle(Pos(x, y))(width, height)(edgeColor)
    window.setRectangle(Pos(x+1, y+1))(width-2, height-2)(backgroundColor)
    window.setRectanglePixel(Pos((x+1)*blockSize, (y+1)*blockSize))(fillWidth, (height-2)*blockSize)(fillColor)
  }
}