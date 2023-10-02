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
    eraseBlocks((0, 0), (30, 1))(y => backgroundColorAtDepth(y))
    write(moles(0).name + " MOLE", (0,0), black)
    write(moles(1).name + " MOLE", (15,0), black)
    write("Area: " + moles(0).area.length, (0,1), black)
    write("Area: " + moles(1).area.length, (15,1), black)
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
  def fillPathOutline(path: Array[(Int, Int)], mole: Mole): Unit = {
    for (pos <- path) {
      setBlock(pos)(combineColors(JColor.darkGray, mole.areaColor))
    }
  }
  def floodFill(x: Int, y: Int, path: Array[(Int, Int)], mole: Mole): Unit = {

    var maxX: Int = path(0)._1;
    var minX: Int = path(0)._1;
    var maxY: Int = path(0)._2;
    var minY: Int = path(0)._2;
    for (pos <- path) {
      val x = pos._1
      if (x < minX) {
        minX = x
      } else if (x > maxX) {
        maxX = x
      }

      val y = pos._2
      if (y < minY) {
        minY = y
      } else if (y > maxY) {
        maxY = y
      }
    }

    if (x >= minX && x <= maxX && y >= minY && y <= maxY && !path.contains((x, y))) {
        setBlock(x, y)(combineColors(JColor.GRAY, mole.areaColor))
        floodFill(x + 1, y, path, mole)
        floodFill(x - 1, y, path, mole)
        floodFill(x, y + 1, path, mole)
        floodFill(x, y - 1, path, mole)
    }
  }
  def fillPath(path: Array[(Int, Int)], mole: Mole): Unit = {
    // find bounding box
    var maxX: Int = path(0)._1;
    var minX: Int = path(0)._1;
    var maxY: Int = path(0)._2;
    var minY: Int = path(0)._2;
    for (pos <- path) {
      val x = pos._1
      if (x < minX) {
        minX = x
      } else if (x > maxX) {
        maxX = x
      }

      val y = pos._2
      if (y < minY) {
        minY = y
      } else if (y > maxY) {
        maxY = y
      }
    }

    // begin filling
    var fillMode = false
    var longSwitch = false
    var lastDirs = Array.empty[String]

    def getBeforeAfterPos(x: Int, y: Int): (Pos, Pos) = {
      val posIndex = path.indexOf((x, y))
      var posAfterIndex = posIndex+1
      if (posAfterIndex == path.length) {
        posAfterIndex = 0
      }
      val posAfter = path (posAfterIndex)
      var posBeforeIndex = posIndex-1
      if (posBeforeIndex == -1) {
        posBeforeIndex = path.length-1
      }
      val posBefore = path (posBeforeIndex)
      (posBefore, posAfter)
    }
    def beginFlow(x: Int, y: Int, posBefore: Pos, posAfter: Pos): Unit = {
      if ((x, y-1) == posAfter || (x, y-1) == posBefore) {
        lastDirs :+= "UP"
      }
      if ((x, y+1) == posAfter || (x, y+1) == posBefore) {
        lastDirs :+= "DOWN"
      }
      longSwitch = true
    }
    def endFlow(x: Int, y: Int, posBefore: Pos, posAfter: Pos): Unit = {
      var currentDirs = Array.empty[String]
      if ((x, y-1) == posAfter || (x, y-1) == posBefore) {
        currentDirs :+= "UP"
      }
      if ((x, y+1) == posAfter || (x, y+1) == posBefore) {
        currentDirs :+= "DOWN"
      }
      val dirsInCommon = lastDirs.filter(dir => currentDirs.contains(dir))
      println(dirsInCommon)

      if (dirsInCommon.length == 0) {
        fillMode = !fillMode
      }
      longSwitch = false
      lastDirs = Array.empty[String]
    }

    for (y <- minY to maxY) {
      for (x <- minX to maxX) {
        if (!path.contains(x-1, y) && path.contains(x, y) && !path.contains(x+1, y)) { // 010
          fillMode = !fillMode
        } else if (!path.contains(x-1, y) && path.contains(x, y) && path.contains(x+1, y)) { // 011

          val (posBefore, posAfter) = getBeforeAfterPos(x, y)

          beginFlow(x, y, posBefore, posAfter)

          if (!((x+1, y) == posAfter || (x+1, y) == posBefore)) {
            fillMode = !fillMode
            longSwitch = false
            lastDirs = Array.empty[String]
          }

        } else if (path.contains(x-1, y) && path.contains(x, y) && !path.contains(x+1, y)) { // 110
          
          val (posBefore, posAfter) = getBeforeAfterPos(x, y)

          endFlow(x, y, posBefore, posAfter)

        } else if (path.contains(x-1, y) && path.contains(x, y) && path.contains(x+1, y)) { // 111
          
          val (posBefore, posAfter) = getBeforeAfterPos(x, y)

          if (!longSwitch) {
            beginFlow(x, y, posBefore, posAfter)
          }

          if (!((x+1, y) == posAfter || (x+1, y) == posBefore)) {
            endFlow(x, y, posBefore, posAfter)
          }

        } else if (fillMode && lastDirs.length == 0) {
          setBlock(x, y)(combineColors(JColor.GRAY, mole.areaColor))
        }
      }
      fillMode = false
      lastDirs = Array.empty[String]
    }
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