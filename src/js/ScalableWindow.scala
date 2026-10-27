package moleTerritory
import java.awt.{Color as JColor}
import org.scalajs.dom
import scala.collection.mutable
import scala.scalajs.js

/** Browser version of ScalableWindow, drawing to the <canvas id="game"> in index.html.
 *  The canvas has a fixed size (width x height) and is scaled to fit the page with CSS,
 *  so getPixel always works in game coordinates. */
class ScalableWindow(
  val width: Int,
  val height: Int,
  val title: String,
  val letterboxColor: JColor = JColor.black
) {
  import ScalableWindow.Event

  private val eventQueue = mutable.Queue.empty[(Int, String)]

  private var _lastEventType = Event.Undefined
  private var _lastKey = ""
  def lastEventType: Int = _lastEventType
  def lastKey: String = _lastKey

  private val canvas = dom.document.getElementById("game").asInstanceOf[dom.HTMLCanvasElement]
  canvas.width = width
  canvas.height = height
  // willReadFrequently keeps the pixels on the CPU, which makes getPixel cheap
  private val ctx = canvas
    .getContext("2d", js.Dynamic.literal(alpha = false, willReadFrequently = true))
    .asInstanceOf[dom.CanvasRenderingContext2D]
  dom.document.body.style.background = ScalableWindow.css(letterboxColor)

  dom.window.addEventListener("keydown", (e: dom.KeyboardEvent) => {
    if (!e.ctrlKey && !e.metaKey) { // leave browser shortcuts alone
      if (ScalableWindow.keysWithBrowserDefault.contains(e.key)) {
        e.preventDefault() // no scrolling or focus changes
      }
      eventQueue.enqueue((Event.KeyPressed, ScalableWindow.keyText(e)))
    }
  })

  def fill(x: Int, y: Int, width: Int, height: Int, color: JColor): Unit = {
    ctx.fillStyle = ScalableWindow.css(color)
    ctx.fillRect(x, y, width, height)
  }

  def getPixel(x: Int, y: Int): JColor = {
    val data = ctx.getImageData(x, y, 1, 1).data
    new JColor(data(0), data(1), data(2))
  }

  def drawText(text: String, x: Int, y: Int, color: JColor, size: Int): Unit = {
    ctx.font = s"bold ${size}px monospace"
    ctx.fillStyle = ScalableWindow.css(color)
    ctx.fillText(text, x, y + size)
  }

  /** Takes the next event if there is one and updates lastEventType and lastKey.
   *  Never waits, the browser can only deliver events between ticks. */
  def awaitEvent(maxWaitMillis: Long): Unit = {
    if (eventQueue.isEmpty) {
      _lastEventType = Event.Undefined
    } else {
      val event = eventQueue.dequeue()
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

  private val keysWithBrowserDefault = Set("ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown", " ", "Tab")

  // same key names as the JVM version (introprog.PixelWindow)
  private val keyTextLookup = Map(
    "Control" -> "Ctrl",
    "AltGraph" -> "Alt Gr",
    "CapsLock" -> "Caps Lock",
    "Escape" -> "Esc",
    "ArrowRight" -> "Right",
    "ArrowLeft" -> "Left",
    "ArrowUp" -> "Up",
    "ArrowDown" -> "Down",
    "PageUp" -> "Page up",
    "PageDown" -> "Page down",
  )

  def keyText(e: dom.KeyboardEvent): String = keyTextLookup.getOrElse(e.key, e.key)

  def css(color: JColor): String = s"rgb(${color.getRed}, ${color.getGreen}, ${color.getBlue})"
}
