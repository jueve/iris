class CpuMemory(ram: Memory2,
                mapper: Mapper,
                ppu: Ppu,
                controllers: Controllers) {
  import CpuMemoryAddress._

  def read(addr: Int): Int = {
    cpuMemoryAccessPattern(addr) match {
      case Ram | RamMirror =>
        ram.read(addr & 0x07ff)

      case Ppu1 | Ppu1Mirror =>
        ppu.read(0x2000 + (addr % 8))

      case Ppu4014 =>
        ppu.read(addr)

      case Apu1 | Apu2 =>
        0x00

      case Reg4016 =>
        controllers.c1.read

      case Reg4017 =>
        controllers.c2.read

      case MapperRange =>
        mapper.readCartridge(addr)

      case IllegalAccessInCpuMemory =>
        addr >>> 8
    }
  }

  def write(addr: Int, d: Int): Unit = {
    cpuMemoryAccessPattern(addr) match {
      case Ram | RamMirror =>
        ram.write(addr & 0x07ff, d)

      case Ppu1 | Ppu1Mirror =>
        ppu.write(0x2000 + (addr % 8), d)

      case Ppu4014 =>
        ppu.write(addr, d)

      case Apu1 | Apu2 => // write to apu

      case Reg4016 =>
        controllers.c1.write(d)
        controllers.c2.write(d)

      case Reg4017 =>

      case MapperRange =>
        mapper.writeCartridge(addr, d)

      case _ =>
        mapper.writeCartridge(addr, d)
    }
  }

  private def cpuMemoryAccessPattern(addr: Int): CpuMemoryAddress = {
    if (addr < 0x0800) Ram
    else if (addr < 0x2000) RamMirror
    else if (addr <= 0x2007) Ppu1
    else if (addr >= 0x2008 && addr <= 0x3fff) Ppu1Mirror
    else if (addr >= 0x4000 && addr <= 0x4013) Apu1
    else if (addr == 0x4014) Ppu4014
    else if (addr == 0x4015) Apu2
    else if (addr == 0x4016) Reg4016
    else if (addr == 0x4017) Reg4017
    else if (addr >= 0x6000) MapperRange
    else IllegalAccessInCpuMemory
  }
}

object CpuMemory {
  def apply(ram: Memory2,
            mapper: Mapper,
            ppu: Ppu,
            controllers: Controllers) = new CpuMemory(ram, mapper, ppu, controllers)
}

class PpuMemory(mapper: Mapper) {
  import PpuMemoryAddress._

  def read(addr: Int): Int = {
    ppuMemoryAddressPattern(addr) match {
      case PatternTables | NameTables | PaletteTable =>
        mapper.readPpu(addr)

      case _ =>
        throw new RuntimeException(s"Illegal access on reading PPU memory: ${ppuMemoryAddressPattern(addr)} => ${addr.toHexString}")
    }
  }

  def write(addr: Int, d: Int): Unit = {
    ppuMemoryAddressPattern(addr) match {
      case PatternTables | NameTables | PaletteTable =>
        mapper.writePpu(addr, d)

      case _ =>
        throw new RuntimeException(s"Illegal access on writing PPU memory: ${ppuMemoryAddressPattern(addr)} => ${addr.toHexString}")
    }
  }

  private def ppuMemoryAddressPattern(addr: Int): PpuMemoryAddress = {
    if (addr < 0x2000) PatternTables
    else if (addr < 0x3f00) NameTables
    else PaletteTable
  }
}

object PpuMemory {
  def apply(mapper: Mapper) = new PpuMemory(mapper)
}

abstract class Memory {
  val arr: Array[Int]
  def read(ind: Int): Int
  def write(ind: Int, d: Int): Unit
}

class Memory1(size: Int) extends Memory {
  val arr: Array[Int] = new Array[Int](size)

  def read(ind: Int): Int = {
    arr(ind) & 0xff
  }

  def write(ind: Int, d: Int): Unit = {
    arr(ind) = d
  }
}

class Memory2(val arr: Array[Int]) extends Memory {
  def read(ind: Int): Int = {
    arr(ind) & 0xff
  }

  def write(ind: Int, d: Int): Unit = {
    arr(ind) = d
  }
}

object Memory {
  def apply(size: Int): Memory1 = new Memory1(size)
  def apply(arr: Array[Int]): Memory2 = new Memory2(arr)
}

sealed trait CpuMemoryAddress
object CpuMemoryAddress {
  case object Ram extends CpuMemoryAddress
  case object RamMirror extends CpuMemoryAddress
  case object Ppu1 extends CpuMemoryAddress
  case object Ppu4014 extends CpuMemoryAddress
  case object Ppu1Mirror extends CpuMemoryAddress
  case object Apu1 extends CpuMemoryAddress
  case object Apu2 extends CpuMemoryAddress
  case object Reg4016 extends CpuMemoryAddress
  case object Reg4017 extends CpuMemoryAddress
  case object MapperRange extends CpuMemoryAddress
  case object IllegalAccessInCpuMemory extends CpuMemoryAddress
}

sealed trait PpuMemoryAddress
object PpuMemoryAddress {
  case object PatternTables extends PpuMemoryAddress
  case object NameTables extends PpuMemoryAddress
  case object PaletteTable extends PpuMemoryAddress
  case object IllegalAccessInPpuMemory extends PpuMemoryAddress
}
