package moleTerritory

object Main {
  @main def run = {
    System.setProperty("apple.awt.application.name", "Pixel Territory")
    val game = new Game();
    game.openMenu();
    while (!game.windowClosed) {
      val t0 = System.currentTimeMillis
      game.tick()
      val elapsedMillis = (System.currentTimeMillis - t0).toInt
      Thread.sleep((game.delayMillis - elapsedMillis) max 0)
    }
    println("QUIT EXECUTED")
  }
}
