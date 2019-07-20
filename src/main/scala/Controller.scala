import scalafx.scene.input.KeyCode

class Controller {
  private var buttons = Array.fill(8)(false)
  private var index = 0
  private var strobe = 0

  def setButtons(b: Array[Boolean]): Unit = {
    buttons = b
  }

  def read: Int = {
    var value = 0
    if (index < 8 && buttons(index)) {
      value = 1
    }
    index += 1
    if ((strobe & 0x1) == 1) {
      index = 0
    }
    value
  }

  def write(value: Int): Unit = {
    strobe = value
    if ((strobe & 0x1) == 1) {
      index = 0
    }
  }
}

class Controllers(val c1: Controller, val c2: Controller) {
  private val b1 = Array.fill(8)(false)
  private val b2 = Array.fill(8)(false)

  private val buttons1: Map[KeyCode, Int] = Map(
    KeyCode.K -> 0, // A
    KeyCode.J -> 1, // B
    KeyCode.Control -> 2, // Select
    KeyCode.Enter -> 3, // Start
    KeyCode.W -> 4, // Up
    KeyCode.S -> 5, // Down
    KeyCode.A -> 6, // Left
    KeyCode.D -> 7 // Right
  )

  def changeState(ke: KeyCode, isPressed: Boolean): Unit = {
    // TODO: Implement player 2
    if (buttons1.contains(ke)) {
      if(isPressed) {
        b1(buttons1(ke)) = true
      } else {
        b1(buttons1(ke)) = false
      }
    }
  }

  def updateController1(): Unit = c1.setButtons(b1)
  def updateController2(): Unit = c2.setButtons(b2)
}

object Controllers {
  def apply(c1: Controller, c2: Controller) = new Controllers(new Controller, new Controller)
}