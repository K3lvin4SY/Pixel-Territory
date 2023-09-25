package blockbattle
import java.awt.{Color as JColor}

object GameProperties {
  val windowSize = WindowSize(30, 50, 21)
  val windowTitle = "EPIC BLOCK BATTLE"
  val skyRange = 0 to 7
  val grassRange = 8 to 8
  object Color {
    val black = new JColor(0, 0, 0)
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
    if (skyRange.contains(y)) {
      Color.sky;
    } else if (grassRange.contains(y)) {
      Color.grass;
    } else {
      Color.soil;
    }
  }
  
  def isPosOutOfPlayingField(pos: Pos): Boolean = {
    val (xPos, yPos) = pos;
    !((0 to windowSize.width-1).contains(xPos) && (skyRange.last to windowSize.height-1).contains(yPos));
  }
}

class Game(
  val leftPlayerName: String = "LEFT",
  val rightPlayerName: String = "RIGHT"
) {
  import GameProperties.* // direkt tillgång till namn på medlemmar i kompanjon
  val window = new BlockWindow(windowSize, windowTitle)

  val offsetFromBorder = GameProperties.windowSize.width / 4;
  val y = GameProperties.skyRange.last;

  val leftMole: Mole = new Mole(leftPlayerName, (offsetFromBorder, y), (0, 0), Color.mole, new KeyControl("A", "D", "W", "S"))
  val rightMole: Mole = new Mole(rightPlayerName, (windowSize.width-offsetFromBorder, y), (0, 0), Color.mole, new KeyControl("LEFT", "RIGHT", "UP", "DOWN"))
  val moles = Array(leftMole, rightMole);

  var worms: Array[Worm] = Array.empty[Worm];
  for (i <- 1 to moles.length+1) {
    val newWorm = new Worm((15, 25), Color.worm, energyWorth = 55)
    newWorm.move()
    worms = worms :+ newWorm
  }
  var gems: Array[Gem] = Array.empty[Gem];
  for (i <- 1 to moles.length+1) {
    val newGem = new Gem((15, 25), Color.gold, pointWorth = 25)
    newGem.move()
    gems = gems :+ newGem
  }

  def drawWorld(): Unit = {
    window.setBackground(0, 0)(windowSize.size)(y => backgroundColorAtDepth(y))
    for (mole <- moles) {
      window.setBlock(mole.pos)(mole.color)
    }
    window.updatePanel(moles)
  }

  def gameover(mole: Mole, winner: Boolean): Unit = {
    var winningPlayer: Mole = mole;
    if (!winner) {
      val moleIndex = moles.indexOf(mole)
      if (moleIndex >= 0) {
        winningPlayer = moles(1 - moleIndex)
      }
    }
    window.setRectangle(0, 0)(windowSize.width, windowSize.height)(Color.gold)
    window.write(
      text = winningPlayer.name+" MOLE is the Winner!",
      pos = (4, 25),
      color = Color.black,
      textSize = 30
    )
  }
  
  def updateGeneral(): Unit = {
    window.updatePanel(moles)

    for (worm <- worms) {
      worm.drawUpdate(window, moles)
    }
    for (gem <- gems) {
      gem.drawUpdate(window, moles)
    }
  }

  def updateMoles(mole: Mole): Unit = {// update, draw new, erase old
    mole.isMoleOutOfBounds() // checking if mole will be out of bounds
    // draws the new mole block
    if (window.getBlock(mole.nextPos) == Color.soil) {
      mole.modifyPoints(1);
      mole.modifyEnergy(-2);
    } else if (window.getBlock(mole.nextPos) == Color.tunnel) {
      mole.modifyEnergy(-1);
    }
    window.setBlock(mole.nextPos)(mole.color)

    // fills in backgound color (including tunnel), this also removes the old mole block
    if (!skyRange.contains(mole.pos._2)) {
      if (!windowSize.isPosOutOfBounds(mole.nextPos)) {
        if (window.getBlock(mole.nextPos) == mole.color && mole.dir != (0, 0)) {
          window.setBlock(mole.pos)(Color.tunnel)
        }
      }
    } else {
      if (!windowSize.isPosOutOfBounds(mole.nextPos)) {
        if (window.getBlock(mole.nextPos) == mole.color && mole.dir != (0, 0)) {
          window.setBlock(mole.pos)(Color.sky)
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
        if (mole.points >= 1000) {
          quit = true;
          gameover(mole, true);
        }
        if (mole.energy <= 0) {
          quit = true;
          gameover(mole, false);
        }
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