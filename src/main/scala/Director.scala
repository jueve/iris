class Director() {
  private var timeStamp: Float = System.nanoTime
  private def currentTime: Float = System.nanoTime

  def step(): Unit = {
    val delta: Float = (currentTime - timeStamp) / 1000000000
    timeStamp = currentTime
    View.update(delta)
  }

  def run(): Unit = {
    while(true) {
      step()
    }
  }
}

object Director {
  def apply(): Director = new Director()
}
