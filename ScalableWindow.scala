package moleTerritory
import java.awt.{Color as JColor}
import java.awt.{Dimension, Font, Graphics, Graphics2D, GraphicsEnvironment, RenderingHints}
import java.awt.event.{KeyAdapter, KeyEvent, WindowAdapter, WindowEvent}
import java.awt.image.BufferedImage
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import javax.swing.{JFrame, JPanel, SwingUtilities, WindowConstants}

/** A resizable replacement for introprog.PixelWindow.
 *  Everything is drawn to an image of a fixed size (width x height) which is
 *  scaled to fit the window, so getPixel always works in game coordinates. */
class ScalableWindow(
  val width: Int,
  val height: Int,
  val title: String,
  val letterboxColor: JColor = JColor.black
) {
  import ScalableWindow.Event

  private val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
  private val eventQueue = new LinkedBlockingQueue[(Int, String)]()

  private var _lastEventType = Event.Undefined
  private var _lastKey = ""
  def lastEventType: Int = _lastEventType
  def lastKey: String = _lastKey

  private val panel = new JPanel {
    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setColor(letterboxColor)
      g2.fillRect(0, 0, getWidth, getHeight)

      // keep the aspect ratio and center the image
      val imageWidth = ScalableWindow.this.width
      val imageHeight = ScalableWindow.this.height
      val scale = (getWidth.toDouble / imageWidth) min (getHeight.toDouble / imageHeight)
      val drawWidth = (imageWidth * scale).toInt
      val drawHeight = (imageHeight * scale).toInt
      val interpolation =
        if (scale >= 1) RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR
        else RenderingHints.VALUE_INTERPOLATION_BILINEAR
      g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, interpolation)
      g2.drawImage(image, (getWidth - drawWidth) / 2, (getHeight - drawHeight) / 2, drawWidth, drawHeight, null)
    }
  }

  private val frame = new JFrame(title)

  SwingUtilities.invokeAndWait(() => {
    // start as large as possible while still fitting on the screen
    val screen = GraphicsEnvironment.getLocalGraphicsEnvironment.getMaximumWindowBounds
    val startScale = 1.0 min ((screen.width * 0.95) / width) min ((screen.height * 0.9) / height)
    panel.setPreferredSize(new Dimension((width * startScale).toInt, (height * startScale).toInt))

    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.setResizable(true)
    frame.setFocusTraversalKeysEnabled(false) // so that TAB reaches the game
    frame.add(panel)
    frame.pack()
    frame.setLocationRelativeTo(null)

    frame.addKeyListener(new KeyAdapter {
      override def keyPressed(e: KeyEvent): Unit = {
        eventQueue.put((Event.KeyPressed, ScalableWindow.keyText(e)))
      }
    })
    frame.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = {
        eventQueue.put((Event.WindowClosed, ""))
      }
    })
    frame.setVisible(true)
  })

  def fill(x: Int, y: Int, width: Int, height: Int, color: JColor): Unit = {
    val g = image.createGraphics()
    g.setColor(color)
    g.fillRect(x, y, width, height)
    g.dispose()
    panel.repaint()
  }

  def getPixel(x: Int, y: Int): JColor = {
    new JColor(image.getRGB(x, y))
  }

  def drawText(text: String, x: Int, y: Int, color: JColor, size: Int): Unit = {
    val g = image.createGraphics()
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g.setFont(new Font("Monospaced", Font.BOLD, size))
    g.setColor(color)
    g.drawString(text, x, y + size)
    g.dispose()
    panel.repaint()
  }

  /** Waits at most maxWaitMillis for the next event and updates lastEventType and lastKey */
  def awaitEvent(maxWaitMillis: Long): Unit = {
    val event = eventQueue.poll(maxWaitMillis, TimeUnit.MILLISECONDS)
    if (event == null) {
      _lastEventType = Event.Undefined
    } else {
      _lastEventType = event._1
      _lastKey = event._2
    }
  }
}

object ScalableWindow {
  object Event {
    val Undefined = 0
    val KeyPressed = 1
    val WindowClosed = 2
  }

  // same key names as introprog.PixelWindow
  private val keyTextLookup = Map(
    KeyEvent.VK_META -> "Meta",
    KeyEvent.VK_CONTROL -> "Ctrl",
    KeyEvent.VK_ALT -> "Alt",
    KeyEvent.VK_ALT_GRAPH -> "Alt Gr",
    KeyEvent.VK_SHIFT -> "Shift",
    KeyEvent.VK_CAPS_LOCK -> "Caps Lock",
    KeyEvent.VK_ENTER -> "Enter",
    KeyEvent.VK_DELETE -> "Delete",
    KeyEvent.VK_BACK_SPACE -> "Backspace",
    KeyEvent.VK_ESCAPE -> "Esc",
    KeyEvent.VK_RIGHT -> "Right",
    KeyEvent.VK_LEFT -> "Left",
    KeyEvent.VK_UP -> "Up",
    KeyEvent.VK_DOWN -> "Down",
    KeyEvent.VK_PAGE_UP -> "Page up",
    KeyEvent.VK_PAGE_DOWN -> "Page down",
    KeyEvent.VK_HOME -> "Home",
    KeyEvent.VK_END -> "End",
    KeyEvent.VK_CLEAR -> "Clear",
    KeyEvent.VK_TAB -> "Tab",
  )

  def keyText(e: KeyEvent): String = {
    if (e.getKeyChar == KeyEvent.CHAR_UNDEFINED || e.getKeyChar < ' ') {
      keyTextLookup.getOrElse(e.getKeyCode, KeyEvent.getKeyText(e.getKeyCode))
    } else {
      e.getKeyChar.toString
    }
  }
}
