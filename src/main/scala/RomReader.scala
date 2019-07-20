import java.io.BufferedInputStream
import Util._

class RomReader(val data: BufferedInputStream) {
  private val header: Array[Byte] = {
    val h = new Array[Byte](16)
    data.read(h, 0, 16)
    h
  }

  private var loadedChr, loadedPrg = false
  private val prgBankSize: Int = header(4) & 0xff
  private val chrBankSize: Int = header(5) & 0xff
  private val flags6: Int = header(6) & 0xff
  private val flags7: Int = header(7) & 0xff
  private val flags9: Int = header(9) & 0xff

  // flags6
  val mirroring: Mirroring = {
    if (isBitSet(3, flags6)) FourScreen
    else if (isBitSet(0, flags6)) Vertical
    else Horizontal
  }

  val mapperNum: Int = (flags7 & 0xf0) | ((flags6 >>> 4) & 0xf)
  val tvSystem: TvSystem = {
    if (isBitSet(0, flags9)) Pal
    else Ntsc
  }

  val mapper: Mapper = generateMapper()

  private def loadPrg(): Array[Int] = {
    if (!loadedPrg) {
      loadedPrg = true
      if (prgBankSize == 1) {
        val prgRom1, prgRom2: Array[Byte] = new Array[Byte](16384)
        data.read(prgRom1, 0, 16384)
        prgRom2.indices.foreach(i => prgRom2(i) = prgRom1(i))
        (prgRom1 ++ prgRom2).map(x => x.wrapAround8)
      } else {
        val prgRom: Array[Byte] = new Array[Byte](16384 * prgBankSize)
        data.read(prgRom, 0, 16384 * prgBankSize)
        prgRom.map(x => x.wrapAround8)
      }
    } else {
      println("Empty in loadPrg")
      new Array[Int](0)
    }
  }

  private def loadChr(): Array[Int] = {
    if (!loadedChr) {
      loadedChr = true
      val chrRom: Array[Byte] = new Array[Byte](8192 * chrBankSize)
      data.read(chrRom, 0, 8192 * chrBankSize)
      chrRom.map(x => x.wrapAround8)
    } else {
      println("Empty in loadChr")
      new Array[Int](0)
    }
  }

  private def generateMapper(): Mapper = {
    mapperNum match {
      case 0 =>
        val prg = loadPrg()
        val chr = loadChr()
        new NromMapper(prg, chr, mirroring)

      case _ =>
        throw new RuntimeException("Invalid mapper.")
    }
  }

  println(s"PRG bank size is $prgBankSize")
  println(s"Mirroring is $mirroring")
  println(s"Contains trainer ${isBitSet(2, flags6)}")
  println(s"Contains PRG RAM ${isBitSet(1, flags6)}")
}

object RomReader {
  def apply(data: BufferedInputStream): RomReader = new RomReader(data)
}

sealed trait Mirroring
case object Horizontal extends Mirroring
case object Vertical extends Mirroring
case object FourScreen extends Mirroring

sealed trait TvSystem
case object Pal extends TvSystem
case object Ntsc extends TvSystem



