//> using scala 3.3
//> using lib se.lth.cs::introprog:1.3.1
package moleTerritory
import java.awt.{Color as JColor}

class BlockWindow(
  val windowSize: WindowSize,
  val windowTitle: String,
  var statsPanels: Array[StatsPanel]
) {
  import introprog.PixelWindow

  val window = new PixelWindow(windowSize.windowWidth, windowSize.windowHeight, windowTitle)

  import GameProperties.Color.*;
  
  def resetPanels(panels: Array[StatsPanel]): Unit = {
    statsPanels = panels
  }

  def setBlock(pos: Pos)(color: JColor = JColor.gray): Unit = {
    val (x, y) = pos //räkna ut blockets x- & y-koordinat i pixelfönstret
    window.fill(x*windowSize.blockSize, y*windowSize.blockSize, windowSize.blockSize, windowSize.blockSize, color = color)
  }

  def getBlock(pos: Pos): JColor = {
    val (x, y) = pos //räkna ut blockets x- & y-koordinat i pixelfönstret
    window.getPixel(x*windowSize.blockSize, y*windowSize.blockSize);
  }

  def updatePanel(): Unit = {
    import windowSize.*;
    for (statPanel <- statsPanels) {
      statPanel.update(this)
    }


    /*eraseBlocks(((padLef/8).toInt, padTop), (padLef-1, padTop+height))(background)
    eraseBlocks(((padLef+width+(padRig/8).toInt), padTop), ((padLef+width+padRig)-1, padTop+height))(background)
    write(moles(0).name + " MOLE", ((padLef/8).toInt, padTop), white, blockSize*2)
    write(moles(1).name + " MOLE", ((padLef+width+(padRig/8).toInt), padTop), white, blockSize*2)
    write("Area: " + moles(0).area.length, ((padLef/8).toInt, padTop+3), white, (blockSize*1.5).toInt)
    write("Area: " + moles(1).area.length, ((padLef+width+(padRig/8).toInt), padTop+3), white, (blockSize*1.5).toInt)
    write("Kills: " + moles(0).kills, ((padLef/8).toInt, padTop+3*2), white, (blockSize*1.5).toInt)
    write("Kills: " + moles(1).kills, ((padLef+width+(padRig/8).toInt), padTop+3*2), white, (blockSize*1.5).toInt)
    write("Suicides: " + moles(0).suicide, ((padLef/8).toInt, padTop+3*3), white, (blockSize*1.5).toInt)
    write("Suicides: " + moles(1).suicide, ((padLef+width+(padRig/8).toInt), padTop+3*3), white, (blockSize*1.5).toInt)
    leftMoleAreaBar.update(moles(0).area.length)
    rightMoleAreaBar.update(moles(1).area.length)*/
  }

  def write(
    text: String,
    pos: Pos,
    color: java.awt.Color,
    textSize: Int = windowSize.blockSize
  ): Unit = {
    window.drawText(text, pos._1 * windowSize.blockSize, pos._2 * windowSize.blockSize, color, textSize)
  }
  def writePixel(
    text: String,
    pos: Pos,
    color: java.awt.Color,
    textSize: Int = windowSize.blockSize
  ): Unit = {
    window.drawText(text, pos._1, pos._2, color, textSize)
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
  def eraseBlocks(pos1: Pos, pos2: Pos)(color: JColor): Unit = {
    /*val (x1, y1) = pos1;
    val (x2, y2) = pos2;
    for (x <- x1 to x2) {
      for (y <- y1 to y2) {
        setBlock(x, y)(color)
      }
    }*/
    window.fill(pos1._1*windowSize.blockSize, pos1._2*windowSize.blockSize, (pos1._1-pos2._1).abs *windowSize.blockSize, (pos1._2-pos2._2).abs *windowSize.blockSize, color = color)
  }

  def setRectanglePixel(leftTopPos: Pos)(size: (Int, Int))(color: JColor = JColor.gray): Unit = {
    val (xPos, yPos) = leftTopPos;
    val (width, height) = size;
    window.fill(xPos, yPos, width, height, color = color)
  }
  def setRectangle(leftTopPos: Pos)(size: (Int, Int))(color: JColor = JColor.gray): Unit = {
    val (xPos, yPos) = leftTopPos;
    val (width, height) = size;
    window.fill(xPos*windowSize.blockSize, yPos*windowSize.blockSize, width*windowSize.blockSize, height*windowSize.blockSize, color = color)
  }
  def setBackground(leftTopPos: Pos)(size: (Int, Int))(colorFunction: Int => JColor = (_: Int) => JColor.gray): Unit = {
    val rightBottomPos = (leftTopPos._1 + size._1, leftTopPos._2 + size._2 )
    eraseBlocks(leftTopPos, rightBottomPos)(colorY => colorFunction(colorY));
  }
  def fillPathOutline(path: Array[(Int, Int)], mole: Mole, area: Array[Pos]): Unit = {
    var areaToAdd = area.clone()
    for (pos <- path) {
      //setBlock(pos)(combineColors(JColor.lightGray ,mole.areaColor))
      setBlock(pos)(mole.areaColor)
      areaToAdd :+= pos;
      //mole.addArea(pos)
    }
    mole.addArea(areaToAdd)
  }
  def findRemainingPathV2(path: Array[Pos], area: Array[Pos]): Array[Pos] = {
    val startTime = System.nanoTime()
    class PathNode(val pathState: Array[Pos], val lastMove: Pos, val parent: PathNode, val depth: Int, val currentEmptyPoses: Array[Pos]) {

      def isSolved(goalPos: Pos): Boolean = {
        if (path.length-1 <= 2) {
          ((distance(goalPos) == 1) && (pathState.length-1 >= 3))
        } else {
          (distance(goalPos) == 1)
        }
      }

      def distance(goalPos: Pos): Int = {
        ( (lastMove._1 - goalPos._1).abs + (lastMove._2 - goalPos._2).abs )
      }
    }

    val goalPos = path.head
    val initialPath = Array(path.last)

    var openSet = Array.empty[PathNode]
    var closedSet = Array.empty[PathNode]

    val nonAreaNearInit = Array((1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1))
    .map(diaDir => (path.last._1 + diaDir._1, path.last._2 + diaDir._2))
    .filter(nonAreaPos => !area.contains(nonAreaPos))

    val initialNode = new PathNode(initialPath, path.last, null, 0, nonAreaNearInit)
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
        import GameProperties.Color.blue
        setBlock(testPos)(combineColors(JColor.black, blue))
      }
      Thread.sleep(250)
      for (testPos <- currentNode.pathState) {
        import GameProperties.Color.blue
        setBlock(testPos)(blue)
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
            val nonAreaNear = dirOptionsDiagonal
            .map(diaDir => (nextPossiblePos._1 + diaDir._1, nextPossiblePos._2 + diaDir._2))
            .filter(nonAreaPos => !area.contains(nonAreaPos))
            if (nonAreaNear.length > 0 && nonAreaNear.intersect(currentNode.currentEmptyPoses).length > 0) {
              val updatedPath = currentNode.pathState :+ nextPossiblePos
              tempDirs += dirOptionsLbl(dirOptions.indexOf(dirOption))+" | "
              if (!closedSet.map(_.pathState).contains(updatedPath)) {
                val newMove = nextPossiblePos
                val newDepth = currentNode.depth + 1
                val newNode = new PathNode(updatedPath, newMove, currentNode, newDepth, nonAreaNear)
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
    //println(timeInMs+" ms")
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
    var areaToAdd = Array.empty[Pos]
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
            areaToAdd :+= (x, y)
            //mole.addArea(x, y)
          }
        }
      }
      fillMode = false
      lastDirs = Array.empty[String]
    }
    fillPathOutline(molePath, mole, areaToAdd)
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
  val padding: Array[Int] = Array.fill(4)(0), //1: top //2: bottom //3: left //4: right
  val width: Int = 30,
  val height: Int = 50,
  val blockSize: Int
) {
  val padTop = padding(0)
  val padBot = padding(1)
  val padLef = padding(2)
  val padRig = padding(3)

  def windowWidth: Int = {
    blockSize*(width+padLef+padRig)
  }
  def windowHeight: Int = {
    blockSize*(height+padTop+padBot)
  }
  def size: Pos = {
    (width, height)
  }
  def size(xDiff: Int, yDiff: Int): Pos = {
    (width+xDiff*2, height+yDiff*2)
  }
  def windowSize: Pos = {
    (width+padLef+padRig, height+padTop+padBot)
  }
  def windowSize(xDiff: Int, yDiff: Int): Pos = {
    (width+padLef+padRig-xDiff*2, height+padTop+padBot-yDiff*2)
  }
  def isPosOutOfBounds(pos: Pos): Boolean = {
    val (xPos, yPos) = pos;
    !((padLef to padLef+width-1).contains(xPos) && (padTop to padTop+height-1).contains(yPos));
  }
}