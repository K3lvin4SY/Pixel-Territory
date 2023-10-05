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
      //setBlock(pos)(combineColors(JColor.lightGray ,mole.areaColor))
      setBlock(pos)(mole.areaColor)
      mole.addArea(pos)
    }
  }
  def findRemainingPathV2(path: Array[Pos], area: Array[Pos]): Array[Pos] = {
    val startTime = System.nanoTime()
    class PathNode(val pathState: Array[Pos], val lastMove: Pos, val parent: PathNode, val depth: Int) {

      def isSolved(goalPos: Pos): Boolean = {
        (distance(goalPos) == 1)
      }

      def distance(goalPos: Pos): Int = {
        ( (lastMove._1 - goalPos._1).abs + (lastMove._2 - goalPos._2).abs )
      }
    }

    val goalPos = path.head
    val initialPath = Array(path.last)

    var openSet = Array.empty[PathNode]
    var closedSet = Array.empty[PathNode]

    val initialNode = new PathNode(initialPath, path.last, null, 0)
    openSet :+= initialNode

    var pathFound = false
    var pathToBeAdded = Array.empty[Pos]
    while (openSet.length > 0 && !pathFound) {
      //openSet = openSet.sortBy(_.distance(goalPos))
      openSet = openSet.sortBy(node => node.depth + node.distance(goalPos))
      val currentNode = openSet.head
      openSet = openSet.tail
      closedSet :+= currentNode

      // start test animation
      /*for (testPos <- currentNode.pathState) {
        import GameProperties.Color.sky
        setBlock(testPos)(combineColors(JColor.black, sky))
      }
      //Thread.sleep(150)
      for (testPos <- currentNode.pathState) {
        import GameProperties.Color.sky
        setBlock(testPos)(sky)
      }*/
      // end test animation

      if (currentNode.isSolved(goalPos)) {
        // exit - the path has been found
        pathFound = true
        pathToBeAdded = currentNode.pathState.tail
      } else {
        val dirOptions = Array((1, 0), (-1, 0), (0, 1), (0, -1))
        val dirOptionsDiagonal = Array((1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1))
        val dirOptionsLbl = Array("E", "W", "S", "N")

        var tempDirs = ""
        for (dirOption <- dirOptions) {
          val nextPossiblePos = (currentNode.lastMove._1 + dirOption._1, currentNode.lastMove._2 + dirOption._2)
          if (area.contains(nextPossiblePos) && !currentNode.pathState.contains(nextPossiblePos)) {
            val nonAreaNear = dirOptionsDiagonal.map(diaDir => area.contains(nextPossiblePos._1 + diaDir._1, nextPossiblePos._2 + diaDir._2)).contains(false)
            if (nonAreaNear) {
              val updatedPath = currentNode.pathState :+ nextPossiblePos
              tempDirs += dirOptionsLbl(dirOptions.indexOf(dirOption))+" | "
              if (!closedSet.map(_.pathState).contains(updatedPath)) {
                val newMove = nextPossiblePos
                val newDepth = currentNode.depth + 1
                val newNode = new PathNode(updatedPath, newMove, currentNode, newDepth)
                openSet :+= newNode
              }
            }
          }
        }
        //println(tempDirs)
      }
    }
    val endTime = System.nanoTime()
    val timeInMs = (endTime - startTime) / 1000000
    println(timeInMs+" ms")
    pathToBeAdded
  }
  def fillPath(molePath: Array[Pos], mole: Mole): Unit = {
    val areaPath = findRemainingPathV2(molePath, mole.area)
    val path = molePath ++ areaPath
    /*for ((xP, yP) <- areaPath) {
      setBlock(xP, yP)(combineColors(JColor.red, mole.areaColor))
    }*/
    

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
      //println(dirsInCommon)

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
          if (!mole.area.contains(x, y)) {
            //setBlock(x, y)(combineColors(JColor.gray, mole.areaColor))
            setBlock(x, y)(mole.areaColor)
            mole.addArea(x, y)
          }
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