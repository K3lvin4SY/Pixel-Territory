//> using scala 3.3
//> using lib se.lth.cs::introprog:1.3.1
package moleTerritory
import java.awt.{Color as JColor}

class BlockWindow(
  val windowSize: WindowSize,
  val windowTitle: String
) {
  import introprog.PixelWindow

  val window = new PixelWindow(windowSize.windowWidth, windowSize.windowHeight, "BlockBattle")

  def setBlock(pos: Pos)(color: JColor = JColor.gray): Unit = {
    val (x, y) = pos //räkna ut blockets x- & y-koordinat i pixelfönstret
    window.fill(x*windowSize.blockSize, y*windowSize.blockSize, windowSize.blockSize, windowSize.blockSize, color = color)
  }

  def getBlock(pos: Pos): JColor = {
    val (x, y) = pos //räkna ut blockets x- & y-koordinat i pixelfönstret
    window.getPixel(x*windowSize.blockSize, y*windowSize.blockSize);
  }

  def updatePanel(moles: Array[Mole]): Unit = {
    import GameProperties.Color.black;
    import GameProperties.backgroundColorAtDepth;
    eraseBlocks((0, 0), (30, 2))(y => backgroundColorAtDepth(y))
    write(moles(0).name + " MOLE", (0,0), black)
    write(moles(1).name + " MOLE", (15,0), black)
    write("pts: " + moles(0).area, (0,1), black)
    write("pts: " + moles(1).area, (15,1), black)
    write("Energy: " + moles(0).energy, (0,2), black)
    write("Energy: " + moles(1).energy, (15,2), black)
  }

  def write(
    text: String,
    pos: Pos,
    color: java.awt.Color,
    textSize: Int = windowSize.blockSize
  ): Unit = {
    window.drawText(text, pos._1 * windowSize.blockSize, pos._2 * windowSize.blockSize, color, textSize)
  }

  def nextEvent(maxWaitMillis: Int = 10): BlockWindow.Event.EventType = {
    import BlockWindow.Event._
    window.awaitEvent(maxWaitMillis)
    window.lastEventType match
    case PixelWindow.Event.KeyPressed => KeyPressed(window.lastKey)
    case PixelWindow.Event.WindowClosed => WindowClosed
    case _ => Undefined
  }
  
  
  /** Use to erase old points, e.g updated score */
  def eraseBlocks(pos1: Pos, pos2: Pos)(colorFunction: Int => JColor = (_: Int) => JColor.gray): Unit = {
    val (x1, y1) = pos1;
    val (x2, y2) = pos2;
    for (x <- x1 to x2) {
      for (y <- y1 to y2) {
        setBlock(x, y)(colorFunction(y))
      }
    }
  }

  def setRectangle(leftTopPos: Pos)(size: (Int, Int))(color: JColor = JColor.gray): Unit = {
    val (xPos, yPos) = leftTopPos;
    val (width, height) = size;
    for (y <- yPos to yPos+height) {
      for (x <- xPos to xPos+width) {
        setBlock(x,y)(color)
      }
    }
  }
  def setBackground(leftTopPos: Pos)(size: (Int, Int))(colorFunction: Int => JColor = (_: Int) => JColor.gray): Unit = {
    val rightBottomPos = (leftTopPos._1 + size._1, leftTopPos._2 + size._2 )
    eraseBlocks(leftTopPos, rightBottomPos)(colorY => colorFunction(colorY));
  }
}

object BlockWindow {
  def delay(millis: Int): Unit = Thread.sleep(millis)
  object Event {
    trait EventType
    case class KeyPressed(key: String) extends EventType
    case object WindowClosed extends EventType
    case object Undefined extends EventType
  }
}

class WindowSize(
    val width: Int = 30,
    val height: Int = 50,
    val blockSize: Int
) {

    def windowWidth: Int = {
      blockSize*width
    }
    def windowHeight: Int = {
      blockSize*height
    }
    def size: Pos = {
      (width, height)
    }
    def isPosOutOfBounds(pos: Pos): Boolean = {
      val (xPos, yPos) = pos;
      !((0 to width-1).contains(xPos) && (0 to height-1).contains(yPos));
    }
}