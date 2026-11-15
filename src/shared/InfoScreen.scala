package moleTerritory
import java.awt.{Color as JColor}

/** Full window screen with the rules of the game and every control. Opened from the menu. */
object InfoScreen {
  import GameProperties.Color

  private val margin = 40
  private val left = 100
  private val descriptionColumn = 560
  private val textSize = 24
  private val lineHeight = 34

  private case class PlayerInfo(name: String, color: JColor, note: String, keys: Array[(String, String)])

  // same keys and order as Game.reset: TWO = yellow + blue, THREE adds green, FOUR adds red
  private val playerInfos = Array(
    PlayerInfo("YELLOW", Color.yellow, "(2+ players)", Array("Up" -> "W", "Left" -> "A", "Down" -> "S", "Right" -> "D")),
    PlayerInfo("BLUE", Color.blue, "(2+ players)", Array("Up" -> "Up arrow", "Left" -> "Left arrow", "Down" -> "Down arrow", "Right" -> "Right arrow")),
    PlayerInfo("GREEN", Color.green, "(3+ players)", Array("Up" -> "I", "Left" -> "J", "Down" -> "K", "Right" -> "L")),
    PlayerInfo("RED", Color.red, "(4 players)", Array("Up" -> "T", "Left" -> "F", "Down" -> "G", "Right" -> "H")),
  )

  private val aboutLines = Array(
    "Steer your mole around the board. Leaving your own territory draws a trail behind you.",
    "Get back to your territory to claim the trail and everything it encloses.",
    "Running into any trail (even your own), or into another mole while you are out on a trail,",
    "costs a life and all of your territory. Run out of lives and you are out of the game.",
    "The last mole standing, or the first to fill the whole board, wins!",
  )

  private val buttonLines = Array(
    "W / S  or  Up / Down" -> "Move between PLAYERS, HEALTH and INFO",
    "A / D  or  Left / Right" -> "Change the selected value",
    "Space / Tab" -> "Cycle through the selected value",
    "Enter" -> "Start the game (opens this screen when INFO is selected)",
    "I" -> "Open this info screen from the menu",
    "Enter (on game over)" -> "Go back to the menu",
    "Esc / Enter / Backspace" -> "Close this info screen",
  )

  /** Width of text in the bold monospace font, which is about 0.6 of the font size per character */
  private def textWidth(text: String, size: Int): Int = (text.length * size * 0.6).toInt

  def draw(window: BlockWindow): Unit = {
    import GameProperties.windowSize.*
    val windowWidthPx = windowWidth
    val windowHeightPx = windowHeight

    window.setRectanglePixel(Pos(0, 0))(windowWidthPx, windowHeightPx)(Color.backgroundEdge)
    window.setRectanglePixel(Pos(margin, margin))(windowWidthPx - 2*margin, windowHeightPx - 2*margin)(Color.bgGray800)

    // title
    val title = "PIXEL TERRITORY"
    window.writePixel(title, Pos((windowWidthPx - textWidth(title, 64)) / 2, 60), Color.yellow, 64)

    // about
    var y = 160
    window.writePixel("ABOUT THE GAME", Pos(left, y), Color.yellow, 32)
    y += 50
    for (line <- aboutLines) {
      window.writePixel(line, Pos(left, y), Color.white, textSize)
      y += lineHeight
    }

    // player controls
    y += 26
    window.writePixel("PLAYER CONTROLS", Pos(left, y), Color.yellow, 32)
    y += 50
    val columnWidth = (windowWidthPx - 2*left) / playerInfos.length
    for ((info, index) <- playerInfos.zipWithIndex) {
      val x = left + index*columnWidth
      window.writePixel(info.name + " MOLE", Pos(x, y), info.color, 32)
      window.writePixel(info.note, Pos(x, y + 40), Color.bgGray400, 20)
      for (((direction, key), row) <- info.keys.zipWithIndex) {
        val rowY = y + 74 + row*lineHeight
        window.writePixel(direction, Pos(x, rowY), Color.bgGray300, textSize)
        window.writePixel(key, Pos(x + textWidth("Right ", textSize), rowY), info.color, textSize)
      }
    }
    y += 74 + 4*lineHeight

    // menu and other buttons
    y += 26
    window.writePixel("MENU & OTHER BUTTONS", Pos(left, y), Color.yellow, 32)
    y += 50
    for ((keys, description) <- buttonLines) {
      window.writePixel(keys, Pos(left, y), Color.white, textSize)
      window.writePixel(description, Pos(descriptionColumn, y), Color.bgGray300, textSize)
      y += lineHeight
    }
    window.writePixel("PLAYERS sets how many moles play (2-4), HEALTH sets the lives per mole (1-10).", Pos(left, y + 10), Color.bgGray400, 20)

    // footer
    val footer = "(Press ESC or ENTER to go back)"
    window.writePixel(footer, Pos((windowWidthPx - textWidth(footer, textSize)) / 2, windowHeightPx - margin - 50), Color.white, textSize)
  }
}
