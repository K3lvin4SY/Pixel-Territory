package moleTerritory
import java.awt.{Color as JColor}

object GameProperties {
  val windowSize = WindowSize(50, 50, 21)
  val windowTitle = "MOLE TERRITORY"
  object Color {
    val black = new JColor(0, 0, 0)
    val white = new JColor(255, 255, 255)
    val mole = new JColor(51, 51, 0)
    val soil = new JColor(153, 102, 51)
    val tunnel = new JColor(204, 153, 102)
    val grass = new JColor(25, 130, 35)
    val sky = new JColor(140, 190, 255)
    val worm = new JColor(238, 121, 194)
    val gold = new JColor(255, 213, 0)
  }
/** Used with the different ranges and eraseBlocks */
  def backgroundColorAtDepth(y: Int): JColor = {
    /*if (skyRange.contains(y)) {
      Color.sky;
    } else if (grassRange.contains(y)) {
      Color.grass;
    } else {
      Color.soil;
    }*/
    Color.white
  }
  
  def isPosOutOfPlayingField(pos: Pos): Boolean = {
    val (xPos, yPos) = pos;
    !((0 to windowSize.width-1).contains(xPos) && (0 to windowSize.height-1).contains(yPos));
  }
}

class Game(
  val leftPlayerName: String = "LEFT",
  val rightPlayerName: String = "RIGHT"
) {
  import GameProperties.* // direkt tillgång till namn på medlemmar i kompanjon
  val window = new BlockWindow(windowSize, windowTitle)

  val offsetFromBorder = GameProperties.windowSize.width / 4;
  val y = 0;

  val leftMole: Mole = new Mole(leftPlayerName, (0, 0), Color.mole, Color.gold, new KeyControl("A", "D", "W", "S"), this)
  val rightMole: Mole = new Mole(rightPlayerName, (0, 0), Color.mole, Color.sky, new KeyControl("LEFT", "RIGHT", "UP", "DOWN"), this)
  val moles = Array(leftMole, rightMole);

  def drawWorld(): Unit = {
    window.setRectangle(0, 0)(windowSize.size)(Color.white)
    for (mole <- moles) {
      mole.spawn(window)
    }
    window.updatePanel(moles)
  }

  def gameover(mole: Mole): Unit = {
    window.setRectangle(0, 0)(windowSize.width, windowSize.height)(mole.areaColor)
    window.write(
      text = mole.name+" MOLE is the Winner!",
      pos = (4, 25),
      color = Color.black,
      textSize = 30
    )
  }
  
  def updateGeneral(): Unit = {
    window.updatePanel(moles)
  }

  def moleWillClaimPos(poses: Array[Pos], mole: Mole): Unit = {
    var amount = 0
    for (otherMole <- moles.filter(_!=mole)) {
      for (otherPos <- otherMole.area) {
        if (poses.contains(otherPos)) {
          otherMole.removeArea(otherPos)
          // whole area body gets eaten
          if (otherMole.area.length == 0) {
            val tempMoleDiePos = otherMole.pos
            otherMole.die(window, moles.filter(_!=otherMole))
            window.setBlock(tempMoleDiePos)(mole.areaColor)
          }
          amount += 1
        }
      }
    }
    println(amount)
  }

  def updateMoles(mole: Mole): Unit = {// update, draw new, erase old
    mole.isMoleOutOfBounds() // checking if mole will be out of bounds
    // draws the new mole block
    /*if (window.getBlock(mole.nextPos) == Color.soil) {
      mole.modifyArea(1);
      mole.modifyEnergy(-2);
    } else if (window.getBlock(mole.nextPos) == Color.tunnel) {
      mole.modifyEnergy(-1);
    }*/

    if (!windowSize.isPosOutOfBounds(mole.nextPos)) {
      val tempPrevColor = window.getBlock(mole.nextPos);

      // tail gets cut - kill
      if (tempPrevColor != JColor.white && !moles.map(_.areaColor).contains(tempPrevColor) && !moles.map(_.color).contains(tempPrevColor)) {
        for (otherMole <- moles) {
          if (otherMole.currentPath.contains(mole.nextPos)) {
            // /kill otherMole
            otherMole.die(window, moles.filter(_!=otherMole))
          }
        }
      }
      // collision between moles - kill
      for (otherMole <- moles.filter(_!=mole)) {
        if (otherMole.nextPos == mole.nextPos || otherMole.pos == mole.nextPos || otherMole.pos == mole.pos) {
          if (otherMole.currentPath.length > 0) {
            otherMole.die(window, moles.filter(_!=otherMole))
          }
          if (mole.currentPath.length > 0) {
            mole.die(window, moles.filter(_!=mole))
          }
        }
      }
      window.setBlock(mole.nextPos)(mole.color)

      if (window.getBlock(mole.nextPos) == mole.color && mole.dir != (0, 0)) {
        if (mole.area.contains(mole.pos)) {
          if (mole.currentPath.length > 0) {
            // fill path
            //println("Printed")
            window.fillPath(mole.currentPath, mole)
            //window.fillPathOutline(mole.currentPath, mole)
            mole.currentPath = Array.empty[Pos]
            mole.currentPathColor = Array.empty[JColor]
          }
        }
        
        if (mole.area.contains(mole.pos)) { // if it is moles territory
          window.setBlock(mole.pos)(mole.areaColor)
          //window.setBlock(mole.pos)(mole.prevColor)
        } else if (mole.prevColor == combineColors(mole.areaColor, Color.white)) { // if it is already a path
          window.setBlock(mole.pos)(mole.prevColor)
        } else {
          window.setBlock(mole.pos)(combineColors(mole.areaColor, mole.prevColor))
          //window.setBlock(mole.pos)(mole.prevColor)
        }
      }
      if (tempPrevColor != mole.color) {
        if (tempPrevColor != JColor.white && !moles.map(_.areaColor).contains(tempPrevColor) && !moles.map(_.color).contains(tempPrevColor)) {
          mole.prevColor = JColor.white
        } else {
          mole.prevColor = tempPrevColor
        }
      }
    }
    mole.move()
  }


  var quit = false
  val delayMillis = 80

  def gameLoop(): Unit = {
    while (!quit) {
      val t0 = System.currentTimeMillis
      handleEvents() // ändrar riktning vid tangenttryck etc.
      for (mole <- moles) {
        updateMoles(mole) // flyttar, ritar, suddar, etc.
        updateGeneral(); // updates text and other game mechanics
        if (mole.area.length >= (windowSize.width*windowSize.height) ) {
          quit = true;
          gameover(mole);
        }
      }
      if (moles.filter(mole => !mole.eliminated).length == 1) { // all but one eliminated
        quit = true
        gameover(moles.filter(mole => !mole.eliminated)(0))
      }
      val elapsedMillis = (System.currentTimeMillis - t0).toInt
      Thread.sleep((delayMillis - elapsedMillis) max 0)
    }

  }

  def handleEvents(): Unit = {
    var e = window.nextEvent()
    while (e != BlockWindow.Event.Undefined) {
      e match
        case BlockWindow.Event.KeyPressed(key) =>
          //println("pressed: "+key.toUpperCase())
          for (mole <- moles) { // ändra riktning på resp. mullvad
            if (mole.keyControl.has(key.toUpperCase())) {
              mole.setDir(key.toUpperCase());
            } else if (key.toUpperCase() == "F") {
              mole.setDir("FORCE");
            }
          }
        case BlockWindow.Event.WindowClosed =>
          quit = true; // avsluta spel-loopen
      e = window.nextEvent()
    }
  }
  def start(): Unit = {
    println("Start digging!")
    println(s"$leftPlayerName ${leftMole.keyControl}")
    println(s"$rightPlayerName ${rightMole.keyControl}")
    drawWorld()
    gameLoop()
  }
}