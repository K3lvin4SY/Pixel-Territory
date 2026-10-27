package moleTerritory
import org.scalajs.dom

object Main {
  @main def run = {
    val game = new Game();
    game.openMenu();
    dom.window.setInterval(() => game.tick(), game.delayMillis)
  }
}
