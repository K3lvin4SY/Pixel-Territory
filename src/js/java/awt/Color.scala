package java.awt

/** Minimal stand-in for java.awt.Color, which does not exist in Scala.js.
 *  Only covers what the game uses. */
class Color(r: Int, g: Int, b: Int) {
  private val rgb = 0xff000000 | ((r & 0xff) << 16) | ((g & 0xff) << 8) | (b & 0xff)

  def getRed: Int = (rgb >> 16) & 0xff
  def getGreen: Int = (rgb >> 8) & 0xff
  def getBlue: Int = rgb & 0xff
  def getRGB: Int = rgb

  override def equals(other: Any): Boolean = other match
    case color: Color => color.getRGB == rgb
    case _ => false
  override def hashCode: Int = rgb
  override def toString: String = s"java.awt.Color[r=$getRed,g=$getGreen,b=$getBlue]"
}

object Color {
  val white = new Color(255, 255, 255)
  val lightGray = new Color(192, 192, 192)
  val gray = new Color(128, 128, 128)
  val black = new Color(0, 0, 0)
  val red = new Color(255, 0, 0)
}
