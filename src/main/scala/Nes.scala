import java.io.{BufferedInputStream, FileInputStream}
import scala.collection.mutable.ArrayBuffer
import scalafx.scene.input.KeyCode

class Nes(file: FileInputStream) {
  private val data = new BufferedInputStream(file)
  private val romReader = RomReader(data)
  private val mapper = romReader.mapper
  private val ram = Memory(Array.fill(2048)(0xff))
  private val ppuMemory = PpuMemory(mapper)
  private val openBus = new OpenBus
  private val ppu = Ppu(ppuMemory, ram, openBus)
  private val controllers = Controllers(new Controller, new Controller)
  private val cpuMemory = CpuMemory(ram, mapper, ppu, controllers)
  private val cpu = Cpu(cpuMemory, openBus)
  private val buffer: ArrayBuffer[Int] = ArrayBuffer()
  private val cpuFrequency: Int = 1789773

  def stepSeconds(second: Float): Unit  = {
    var cycles: Int = (cpuFrequency * second).toInt
      while (cycles > 0) {
        cycles = cycles - step()
        if (buffer.nonEmpty) {
          View.draw(getToDraw)
        }
      }
  }

  private def step(): Int = {
    val cpuCycles = cpu.clock()
    (1 to cpuCycles * 3).flatMap(_ => ppu.clock()) ++=: buffer
    cpuCycles
  }

  def getToDraw: Array[Int] = {
    val len = 256 * 240
    val arr: Array[Int] = new Array[Int](len)
    buffer.copyToArray(arr, 0, len)
    buffer.clear()
    arr.reverse
  }

  def updateControllers(): Unit = {
    controllers.updateController1()
    controllers.updateController2()
  }

  def changeControllerState(ke: KeyCode, isPressed: Boolean): Unit = {
    controllers.changeState(ke, isPressed)
  }
}

object Nes {
  def apply(fileInputStream: FileInputStream): Nes = new Nes(fileInputStream)
}