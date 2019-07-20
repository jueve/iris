import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.Includes._
import scalafx.scene.image.PixelFormat
import scalafx.scene.canvas.Canvas
import scalafx.scene.{Group, Scene}
import scalafx.scene.input.KeyEvent
import java.io.FileInputStream
import javafx.application.Platform

object View extends JFXApp {

  if (parameters.raw.isEmpty) {
    throw new RuntimeException("Need path to ROM file.")
    Platform.exit()
  }

  private val path = parameters.raw.head
  private val file = new FileInputStream(path)
  private val nes = Nes(file)
  private val director = Director()

  private var count = 0
  private var baseTime: Long = 0
  private var frameRate: Float  = 0

  private val c = new Canvas(256, 240)
  private val writer = c.graphicsContext2D.pixelWriter
  private val fmt = PixelFormat.getIntArgbInstance

  private val s: Scene = new Scene {
    root = new Group {
      children ++= Seq(c)
    }
  }

  c.setFocusTraversable(true)
  c.handleEvent(KeyEvent.Any) {
    ke: KeyEvent => {
      ke.eventType.name match {
        case "KEY_PRESSED" =>
          nes.changeControllerState(ke.code, isPressed = true)
        case "KEY_RELEASED"=>
          nes.changeControllerState(ke.code, isPressed = false)
        case _ =>
      }
    }
  }

  stage = new PrimaryStage {
    title = "iris"
    scene = s
  }

  stage.resizable = false

  def update(delta: Float): Unit = {
    var d: Float = 0
    d = if (delta > 1) 0 else delta
    nes.updateControllers()
    nes.stepSeconds(d)
  }

  def draw(buffer: Array[Int]): Unit = {
//    printFps()
    writer.setPixels(0, 0, 256, 240, fmt, buffer, 0, 256)
  }

  new Thread(() => {
    director.run()
  }).start()


  private def printFps(): Unit = {
    count += 1
    val now = System.currentTimeMillis()
    if ((now - baseTime) >= 1000) {
      frameRate = (count * 1000).toFloat / (now - baseTime).toFloat
      baseTime = now
      count = 0
    }
    println(frameRate)
  }
}

