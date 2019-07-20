abstract class Mapper {
  def readCartridge(addr: Int): Int

  def writeCartridge(addr: Int, d: Int): Unit

  def readPpu(addr: Int): Int

  def writePpu(addr: Int, d: Int): Unit
}
