import scala.collection.mutable.{Map => MutableMap}

class NromMapper(prgRom: Array[Int],
                 chrRom: Array[Int],
                 mirroring: Mirroring) extends Mapper {

  private val pput0, pput1, pput2, pput3: Array[Int] = new Array[Int](0x400)

  private val palette = new Array[Int](0x100)

  private val nt: MutableMap[Int, Array[Int]] = MutableMap()

  mirroring match {
    case Horizontal =>
      nt += (0 -> pput0, 1 -> pput0, 2 -> pput1, 3 -> pput1)

    case Vertical =>
      nt += (0 -> pput0, 1 -> pput1, 2 -> pput0, 3 -> pput1)

    case _ =>
      throw new RuntimeException
  }

  override def readCartridge(addr: Int): Int = {
    if (addr >= 0x8000) {
      prgRom(addr - 0x8000)
    } else {
      throw new RuntimeException
    }
  }

  override def writeCartridge(addr: Int, d: Int): Unit = {}

  override def readPpu(addr: Int): Int = {
    if (addr < 0x2000) {
      chrRom(addr)

    } else {
      if (addr < 0x2400) {
        nt(0)(addr & 0x3ff)

      } else if (addr < 0x2800) {
        nt(1)(addr & 0x3ff)

      } else if (addr < 0x2c00) {
        nt(2)(addr & 0x3ff)

      } else if (addr < 0x3000) {
        nt(3)(addr & 0x3ff)

      } else if (addr < 0x3f00) {
        readPpu(addr - 0x1000)

      } else {
        var tmp = addr & 0x1f
        if (tmp >= 0x10 && ((tmp & 0x3) == 0)) {
          tmp -= 0x10
        }
        palette(tmp)
      }
    }
  }

  override def writePpu(addr: Int, d: Int): Unit = {
    val addrTmp = addr & 0x3fff
    if (addrTmp < 0x2000) {
      chrRom(addrTmp) = d
    } else {
      if (addr < 0x2400) {
        nt(0)(addr & 0x3ff) = d

      } else if (addr < 0x2800) {
        nt(1)(addr & 0x3ff) = d

      } else if (addr < 0x2c00) {
        nt(2)(addr & 0x3ff) = d

      } else if (addr < 0x3000) {
        nt(3)(addr & 0x3ff) = d

      } else if (addr < 0x3f00) {
        writePpu(addr - 0x1000, d)

      } else {
        var tmp = addr & 0x1f
        if (tmp >= 0x10 && ((tmp & 0x3) == 0)) {
          tmp -= 0x10
        }
        palette(tmp) = d
      }
    }
  }
}
