package moleTerritory

object Main {                
  @main def run = {
    System.setProperty("apple.awt.application.name", "Pixel Territory")
    val game = new Game();
    game.start();
  }
}