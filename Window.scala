//> using scala 3.3
package moleTerritory
import java.awt.{Color as JColor}

class BlockWindow(
  val windowSize: WindowSize,
  val windowTitle: String,
  var statsPanels: Array[StatsPanel]
) {
  import GameProperties.Color.*;

  val window = new ScalableWindow(windowSize.windowWidth, windowSize.windowHeight, windowTitle, backgroundEdge)
  
  def resetPanels(panels: Array[StatsPanel]): Unit = {
    statsPanels = panels
  }

  def setBlock(pos: Pos)(color: JColor = JColor.gray): Unit = {
    val (x, y) = pos.tuple //räkna ut blockets x- & y-koordinat i pixelfönstret
    window.fill(x*windowSize.blockSize, y*windowSize.blockSize, windowSize.blockSize, windowSize.blockSize, color = color)
  }

  def getBlock(pos: Pos): JColor = {
    val (x, y) = pos.tuple //räkna ut blockets x- & y-koordinat i pixelfönstret
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
    window.drawText(text, pos.x * windowSize.blockSize, pos.y * windowSize.blockSize, color, textSize)
  }
  def writePixel(
    text: String,
    pos: Pos,
    color: java.awt.Color,
    textSize: Int = windowSize.blockSize
  ): Unit = {
    window.drawText(text, pos.x, pos.y, color, textSize)
  }

  def nextEvent(maxWaitMillis: Int = 10): BlockWindow.Event.EventType = {
    import BlockWindow.Event._
    window.awaitEvent(maxWaitMillis)
    window.lastEventType match
    case ScalableWindow.Event.KeyPressed => KeyPressed(window.lastKey)
    case ScalableWindow.Event.WindowClosed => WindowClosed
    case _ => Undefined
  }
  
  
  /** Use to erase old points, e.g updated score */
  def eraseBlocks(pos1: Pos, pos2: Pos)(colorFunction: Int => JColor = (_: Int) => JColor.gray): Unit = {
    val (x1, y1) = pos1.tuple;
    val (x2, y2) = pos2.tuple;
    for (x <- x1 to x2) {
      for (y <- y1 to y2) {
        setBlock(Pos(x, y))(colorFunction(y))
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
    window.fill(pos1.x*windowSize.blockSize, pos1.y*windowSize.blockSize, (pos1.x-pos2.x).abs *windowSize.blockSize, (pos1.y-pos2.y).abs *windowSize.blockSize, color = color)
  }

  def setRectanglePixel(leftTopPos: Pos)(size: (Int, Int))(color: JColor = JColor.gray): Unit = {
    val (xPos, yPos) = leftTopPos.tuple;
    val (width, height) = size;
    window.fill(xPos, yPos, width, height, color = color)
  }
  def setRectangle(leftTopPos: Pos)(size: (Int, Int))(color: JColor = JColor.gray): Unit = {
    val (xPos, yPos) = leftTopPos.tuple;
    val (width, height) = size;
    window.fill(xPos*windowSize.blockSize, yPos*windowSize.blockSize, width*windowSize.blockSize, height*windowSize.blockSize, color = color)
  }
  def setBackground(leftTopPos: Pos)(size: (Int, Int))(colorFunction: Int => JColor = (_: Int) => JColor.gray): Unit = {
    val rightBottomPos = Pos(leftTopPos.x + size._1, leftTopPos.y + size._2)
    eraseBlocks(leftTopPos, rightBottomPos)(colorY => colorFunction(colorY));
  }
  def fillPathOutline(path: Array[Pos], player: Player, area: Array[Pos]): Unit = {
    var areaToAdd = area.clone()
    for (pos <- path) {
      //setBlock(pos)(combineColors(JColor.lightGray ,player.areaColor))
      setBlock(pos)(player.areaColor)
      areaToAdd :+= pos;
      //player.addArea(pos)
    }
    player.addArea(areaToAdd)
  }
  def findRemainingPathV2(path: Array[Pos], area: Set[Pos]): Array[Pos] = {
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
        ( (lastMove.x - goalPos.x).abs + (lastMove.y - goalPos.y).abs )
      }
    }

    val goalPos = path.head
    val initialPath = Array(path.last)

    // ordered by depth + distance, ties in insertion order
    var insertCount = 0
    val openSet = scala.collection.mutable.PriorityQueue.empty[(Int, Int, PathNode)](
      Ordering.by[(Int, Int, PathNode), (Int, Int)](entry => (entry._1, entry._2)).reverse
    )
    def addToOpenSet(node: PathNode): Unit = {
      openSet.enqueue((node.depth + node.distance(goalPos), insertCount, node))
      insertCount += 1
    }

    val nonAreaNearInit = Array(East, West, South, North, South+East, North+East, South+West, North+West)
    .map(diaDir => path.last + diaDir)
    .filter(nonAreaPos => !area.contains(nonAreaPos))

    val initialNode = new PathNode(initialPath, path.last, null, 0, nonAreaNearInit)
    addToOpenSet(initialNode)

    var pathFound = false
    var pathToBeAdded = Array.empty[Pos]
    while (openSet.length > 0 && !pathFound) {
      val currentNode = openSet.dequeue()._3

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
        val dirOptions = Array(East, West, South, North)
        val dirOptionsDiagonal = Array(East, West, South, North, South+East, North+East, South+West, North+West)
        val dirOptionsLbl = Array("E", "W", "S", "N")

        var tempDirs = ""
        for (dirOption <- dirOptions) {
          val nextPossiblePos = currentNode.lastMove + dirOption
          if (area.contains(nextPossiblePos) && !currentNode.pathState.contains(nextPossiblePos)) {
            val nonAreaNear = dirOptionsDiagonal
            .map(diaDir => nextPossiblePos + diaDir)
            .filter(nonAreaPos => !area.contains(nonAreaPos))
            if (nonAreaNear.length > 0 && nonAreaNear.intersect(currentNode.currentEmptyPoses).length > 0) {
              val updatedPath = currentNode.pathState :+ nextPossiblePos
              tempDirs += dirOptionsLbl(dirOptions.indexOf(dirOption))+" | "
              val newMove = nextPossiblePos
              val newDepth = currentNode.depth + 1
              val newNode = new PathNode(updatedPath, newMove, currentNode, newDepth, nonAreaNear)
              addToOpenSet(newNode)
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
  def fillPath(molePath: Array[Pos], player: Player): Unit = {
    val areaPath = findRemainingPathV2(molePath, player.area)
    val path = molePath ++ areaPath
    val pathSet = path.toSet
    val pathIndex = path.zipWithIndex.distinctBy(_._1).toMap
    /*for (areaPos <- areaPath) {
      setBlock(areaPos)(combineColors(JColor.red, player.areaColor))
    }*/
    

    // find bounding box
    var maxX: Int = path(0).x;
    var minX: Int = path(0).x;
    var maxY: Int = path(0).y;
    var minY: Int = path(0).y;
    for (pos <- path) {
      val x = pos.x
      if (x < minX) {
        minX = x
      } else if (x > maxX) {
        maxX = x
      }

      val y = pos.y
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
      val posIndex = pathIndex(Pos(x, y))
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
      if (Pos(x, y-1) == posAfter || Pos(x, y-1) == posBefore) {
        lastDirs :+= "UP"
      }
      if (Pos(x, y+1) == posAfter || Pos(x, y+1) == posBefore) {
        lastDirs :+= "DOWN"
      }
      longSwitch = true
    }
    def endFlow(x: Int, y: Int, posBefore: Pos, posAfter: Pos): Unit = {
      var currentDirs = Array.empty[String]
      if (Pos(x, y-1) == posAfter || Pos(x, y-1) == posBefore) {
        currentDirs :+= "UP"
      }
      if (Pos(x, y+1) == posAfter || Pos(x, y+1) == posBefore) {
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
        if (!pathSet.contains(Pos(x-1, y)) && pathSet.contains(Pos(x, y)) && !pathSet.contains(Pos(x+1, y))) { // 010
          fillMode = !fillMode
        } else if (!pathSet.contains(Pos(x-1, y)) && pathSet.contains(Pos(x, y)) && pathSet.contains(Pos(x+1, y))) { // 011

          val (posBefore, posAfter) = getBeforeAfterPos(x, y)

          beginFlow(x, y, posBefore, posAfter)

          if (!(Pos(x+1, y) == posAfter || Pos(x+1, y) == posBefore)) {
            fillMode = !fillMode
            longSwitch = false
            lastDirs = Array.empty[String]
          }

        } else if (pathSet.contains(Pos(x-1, y)) && pathSet.contains(Pos(x, y)) && !pathSet.contains(Pos(x+1, y))) { // 110
          
          val (posBefore, posAfter) = getBeforeAfterPos(x, y)

          endFlow(x, y, posBefore, posAfter)

        } else if (pathSet.contains(Pos(x-1, y)) && pathSet.contains(Pos(x, y)) && pathSet.contains(Pos(x+1, y))) { // 111
          
          val (posBefore, posAfter) = getBeforeAfterPos(x, y)

          if (!longSwitch) {
            beginFlow(x, y, posBefore, posAfter)
          }

          if (!(Pos(x+1, y) == posAfter || Pos(x+1, y) == posBefore)) {
            endFlow(x, y, posBefore, posAfter)
          }

        } else if (fillMode && lastDirs.length == 0) {
          if (!player.area.contains(Pos(x, y))) {
            //setBlock(Pos(x, y))(combineColors(JColor.gray, player.areaColor))
            setBlock(Pos(x, y))(player.areaColor)
            areaToAdd :+= Pos(x, y)
            //player.addArea(Pos(x, y))
          }
        }
      }
      fillMode = false
      lastDirs = Array.empty[String]
    }
    fillPathOutline(molePath, player, areaToAdd)
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
    Pos(width, height)
  }
  def size(xDiff: Int, yDiff: Int): Pos = {
    Pos(width+xDiff*2, height+yDiff*2)
  }
  def windowSize: Pos = {
    Pos(width+padLef+padRig, height+padTop+padBot)
  }
  def windowSize(xDiff: Int, yDiff: Int): Pos = {
    Pos(width+padLef+padRig-xDiff*2, height+padTop+padBot-yDiff*2)
  }
  def isPosOutOfBounds(pos: Pos): Boolean = {
    val (xPos, yPos) = pos.tuple;
    !((padLef to padLef+width-1).contains(xPos) && (padTop to padTop+height-1).contains(yPos));
  }
}