package moleTerritory
import java.awt.{Color as JColor}

object GameProperties {
  //val windowSize = WindowSize(Array(0, 0, 0, 0), 50, 50, 21)
  val windowSize = WindowSize(Array(5, 5, 20, 20), 50, 50, 20)
  val windowTitle = "MOLE TERRITORY"
  object Color {
    val black = new JColor(0, 0, 0)
    val white = new JColor(255, 255, 255)
    val mole = new JColor(51, 51, 0)
    val blue = new JColor(140, 190, 255)
    val red = new JColor(232, 46, 65)
    val yellow = new JColor(255, 213, 0)
    val green = new JColor(118, 247, 82)
    val bgGray300 = new JColor(209, 213, 219);
    val bgGray400 = new JColor(156, 163, 175);
    val bgGray500 = new JColor(107, 114, 128);
    val bgGray600 = new JColor(75, 85, 99);
    val bgGray700 = new JColor(55, 65, 81);
    val bgGray800 = new JColor(31, 41, 55);
    val bgGray900 = new JColor(17, 24, 39);
    val background = bgGray600
    val backgroundEdge = bgGray700
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
    !((windowSize.padLef to windowSize.padLef+windowSize.width-1).contains(xPos) && (windowSize.padTop to windowSize.padTop+windowSize.height-1).contains(yPos));
  }
}

class Game() {
  import GameProperties.* // direkt tillgång till namn på medlemmar i kompanjon

  val offsetFromBorder = GameProperties.windowSize.width / 4;
  val y = 0;

  var yellowMole: Mole = new Mole("YELLOW", (0, 0), Color.mole, Color.yellow, new KeyControl("A", "D", "W", "S"), this)
  var greenMole: Mole = new Mole("GREEN", (0, 0), Color.mole, Color.green, new KeyControl("J", "L", "I", "K"), this)
  var blueMole: Mole = new Mole("BLUE", (0, 0), Color.mole, Color.blue, new KeyControl("LEFT", "RIGHT", "UP", "DOWN"), this)
  var redMole: Mole = new Mole("RED", (0, 0), Color.mole, Color.red, new KeyControl("F", "H", "T", "G"), this)
  var moles = Array(yellowMole, greenMole, blueMole, redMole);

  var yellowMoleAreaBar = new StatsPanel(yellowMole)((windowSize.padLef/8).toInt, windowSize.padTop)
  var greenMoleAreaBar = new StatsPanel(greenMole)((windowSize.padLef/8).toInt, windowSize.padTop+20)
  var blueMoleAreaBar = new StatsPanel(blueMole)((windowSize.padLef+windowSize.width+(windowSize.padRig/8).toInt), windowSize.padTop)
  var redMoleAreaBar = new StatsPanel(redMole)((windowSize.padLef+windowSize.width+(windowSize.padRig/8).toInt), windowSize.padTop+20)
  var statsPanels = Array(yellowMoleAreaBar, greenMoleAreaBar, blueMoleAreaBar, redMoleAreaBar)

  val window = new BlockWindow(windowSize, windowTitle, statsPanels)
  val menuSelector = new MenuSelector()

  def drawBackground(): Unit = {
    window.setRectangle(0, 0)(windowSize.windowSize)(Color.backgroundEdge)
    window.setRectangle(1, 1)(windowSize.windowSize(1, 1))(Color.background)
    window.setRectangle(windowSize.padLef-1, windowSize.padTop-1)(windowSize.size(1, 1))(Color.backgroundEdge)
    window.setRectangle(windowSize.padLef, windowSize.padTop)(windowSize.size)(Color.white)
  }

  def drawMenu(): Unit = {
    drawBackground()
    menuSelector.update(window)
  }
  def drawWorld(): Unit = {
    drawBackground()
    for (mole <- moles) {
      mole.spawn(window)
    }
    window.updatePanel()
  }

  def gameover(mole: Mole): Unit = {
    window.setRectangle(windowSize.padLef, windowSize.padTop)(windowSize.width, windowSize.height)(mole.areaColor)
    window.write(
      text = mole.name+" MOLE is the Winner!",
      pos = (windowSize.padLef+4, windowSize.padTop+25),
      color = Color.black,
      textSize = 30
    )
  }
  
  def updateGeneral(): Unit = {
    window.updatePanel()
  }

  def moleWillClaimPos(poses: Array[Pos], mole: Mole): Unit = {
    class ExecuteMole(val victim: Mole, val killer: Mole, val pos: Pos) {
      def execute(window: BlockWindow): Unit = {
        victim.die(window, moles.filter(_!=victim))
        killer.killed(victim)
        window.setBlock(pos)(killer.areaColor)
      }
    }
    var amount = 0
    var executionOrder66 = Array.empty[ExecuteMole]

    val poseSet = poses.toSet
    for (otherMole <- moles.filter(_!=mole)) {
      for (otherPos <- otherMole.area) {
        if (poseSet(otherPos)) {
          otherMole.removeArea(otherPos)
          // whole area body gets eaten
          if (otherMole.pos == otherPos) {
            executionOrder66 :+= new ExecuteMole(otherMole, mole, otherMole.pos)
          }
          amount += 1
        }
      }
    }
    for (executeMole <- executionOrder66) {
      println(executeMole.victim.name)
      executeMole.execute(window)
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
            mole.killed(otherMole)
          }
        }
      }
      // collision between moles - kill
      for (otherMole <- moles.filter(_!=mole).filter(_.eliminated == false)) {
        if (otherMole.nextPos == mole.nextPos || otherMole.pos == mole.nextPos || otherMole.pos == mole.pos) {
          if (otherMole.currentPath.length > 0) {
            otherMole.die(window, moles.filter(_!=otherMole))
            mole.killed(otherMole)
          }
          if (mole.currentPath.length > 0) {
            mole.die(window, moles.filter(_!=mole))
            otherMole.killed(mole)
          }
        }
      }
      
      // Draw
      if (!mole.eliminated) {
        window.setBlock(mole.nextPos)(mole.color)
      }

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
        
        // Draw
        if (!mole.eliminated) {
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
      }
      if (tempPrevColor != mole.color) {
        if (tempPrevColor != JColor.white && !moles.map(_.areaColor).contains(tempPrevColor) && !moles.map(_.color).contains(tempPrevColor)) {
          mole.prevColor = JColor.white
        } else {
          mole.prevColor = tempPrevColor
        }
      }
    }
    if (!mole.eliminated) {
      mole.move()
    }
  }


  var quit = false
  var exitMenu = false
  val delayMillis = 80

  def menuLoop(): Unit = {
    while (!exitMenu) {
      val t0 = System.currentTimeMillis
      handleMenuEvents()
      menuSelector.update(window)
      val elapsedMillis = (System.currentTimeMillis - t0).toInt
      Thread.sleep((delayMillis - elapsedMillis) max 0)
    }

  }
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

  def handleMenuEvents(): Unit = {
    var e = window.nextEvent()
    while (e != BlockWindow.Event.Undefined) {
      e match
        case BlockWindow.Event.KeyPressed(key) =>
          println("pressed: "+key.toUpperCase())
          // Navigate menu
          if (key.toUpperCase() == "ENTER") {
            exitMenu = true
          } else if (Array("LEFT", "A").contains(key.toUpperCase())) {
            if (menuSelector.players > 2) {
              menuSelector.players -= 1
            }
          } else if (Array("RIGHT", "D").contains(key.toUpperCase())) {
            if (menuSelector.players < 4) {
              menuSelector.players += 1
            }
          }
        case BlockWindow.Event.WindowClosed =>
          exitMenu = true;
          quit = true;
      e = window.nextEvent()
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

  def reset(players: Int): Unit = {
    yellowMole = new Mole("YELLOW", (0, 0), Color.mole, Color.yellow, new KeyControl("A", "D", "W", "S"), this)
    greenMole = new Mole("GREEN", (0, 0), Color.mole, Color.green, new KeyControl("J", "L", "I", "K"), this)
    blueMole = new Mole("BLUE", (0, 0), Color.mole, Color.blue, new KeyControl("LEFT", "RIGHT", "UP", "DOWN"), this)
    redMole = new Mole("RED", (0, 0), Color.mole, Color.red, new KeyControl("F", "H", "T", "G"), this)
    moles = Array(yellowMole, blueMole, greenMole, redMole).take(players);

    yellowMoleAreaBar = new StatsPanel(yellowMole)((windowSize.padLef/8).toInt, windowSize.padTop)
    greenMoleAreaBar = new StatsPanel(greenMole)((windowSize.padLef/8).toInt, windowSize.padTop+20)
    blueMoleAreaBar = new StatsPanel(blueMole)((windowSize.padLef+windowSize.width+(windowSize.padRig/8).toInt), windowSize.padTop)
    redMoleAreaBar = new StatsPanel(redMole)((windowSize.padLef+windowSize.width+(windowSize.padRig/8).toInt), windowSize.padTop+20)
    statsPanels = Array(yellowMoleAreaBar, blueMoleAreaBar, greenMoleAreaBar, redMoleAreaBar).take(players)
    window.resetPanels(statsPanels)
    menuSelector.reset()
  }

  def start(): Unit = {
    println("Start conquering!")
    drawMenu()
    menuLoop()
    reset(menuSelector.players)
    drawWorld()
    gameLoop()
  }
}