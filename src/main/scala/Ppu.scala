import Util._

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

class Ppu(ppuMemory: PpuMemory, ram: Memory2, openBus: OpenBus) {
  // previous PPU register value
  private var regPrevious = 0x0

  // $2000 PPUCTRL
  private var baseNtAddr = 0x0
  private var vRamAddrInc,
              sprPtAddr,
              bgPtAddr,
              sprSize,
              nmiOutput = false

  // $2001 PPUMASK
  private var showBgInLeftmost,
              showSprInLeftmost,
              showBg,
              showSpr = false

  // $2002 PPUSTATUS
  private var sprOverflow,
              sprZeroHit,
              nmiOccurred = false

  // $2003 OAM Address
  private var oamAddr = 0x00

  // $2007 PPU Data
  private var pDataBuffer = 0x00

  // Internal registers
  private var interV,
              interT,
              interX,
              interW = 0x00

  // 16 bit shift registers for background
  private var bgBitmapDataHighShift,
              bgBitmapDataLowShift = 0x00

  // 8 bit shift registers for background
  private var bgPaletteAttrShift = 0x0

  // 8 bit latches for background
  private var ntByteLatch,
              atByteLatch,
              bgTileBitmapLowLatch,
              bgTileBitmapHighLatch = 0x00

  // 8 bit shift registers for 8 sprites
  private val sprBitmapDataHighShift,
              sprBitmapDataLowShift = Array.fill(8)(0x00)

  // 8 bit latches for 8 sprites
  private val sprAttrLatch = Array.fill(8)(0x00)

  // 8 bit latches for 8 sprites
  private val sprXLatch = Array.fill(8)(0x00)

  private var bgRgb = (0, 0, 0)
  private var bgPixel = false

  private var sprRgb = (0, 0, 0)
  private var sprPixel = false
  private var sprPriority = false
  private var sprIndex = 0

  // oam
  private val oam = Array.fill(256)(0x0)
  private val secOam  = Array.fill(32)(0xff)
  private var oamFind = 0
  private var oamStart = 0x0
  private var findSpr0 = false

  // 0 to 340
  private var cycle = 0

  // 0 to 261
  private var scanline = 0
  private var isOddScanline = false

  val colorPalette: Vector[(Int, Int, Int)] = Vector(
    (84, 84, 84), (0, 30, 116), (8, 16, 144), (48, 0, 136),
    (68, 0, 100), (92, 0, 48), (84, 4, 0), (60, 24, 0),
    (32, 42, 0), (8, 58, 0), (0, 64, 0), (0, 60, 0),
    (0, 50, 60), (0, 0, 0), (0, 0, 0), (0, 0, 0),

    (152, 150, 152), (8, 76, 196), (48, 50, 236), (92, 30, 228),
    (136, 20, 176), (160, 20, 100), (152, 34, 32), (120, 60, 0),
    (84, 90, 0), (40, 114, 0), (8, 124, 0), (0, 118, 40),
    (0, 102, 120), (0, 0,  0), (0, 0, 0), (0, 0, 0),

    (236, 238, 236), (76, 154, 236), (120, 124, 236), (176, 98, 236),
    (228, 84, 236), (236, 88, 180), (236, 106, 100), (212, 136, 32),
    (160, 170, 0), (116, 196, 0), (76, 208, 32), (56, 204, 108),
    (56, 180, 204), (60, 60, 60), (0, 0, 0), (0, 0, 0),

    (236, 238, 236), (168, 204, 236), (188, 188, 236), (212, 178, 236),
    (236, 174, 236), (236, 174, 212), (236, 180, 176), (228, 196, 144),
    (204, 210, 120), (180, 222, 120), (168, 226, 144), (152, 226, 180),
    (160, 214, 228), (160, 162, 160), (0, 0, 0), (0, 0, 0))

  // buffer
  val lineBuffer: ArrayBuffer[Int] = ArrayBuffer()

  private def addrInc: Int = if (vRamAddrInc) 32 else 1

  def read(addr: Int): Int = {
    (addr - 0x2000) % 8 match {
      case 2 =>
        readStatus()

      case 4 =>
        readOamData()

      case 7 =>
        readData()

      case _ =>
        regPrevious
    }
  }

  def write(addr: Int, d: Int): Unit = {
    regPrevious = d
    if (addr == 0x4014) {
      val start = d << 8
      (0x0 to 0xff).foreach(i => oam(i) = ram.read(start | i))
      openBus.setCpuStall()
    } else {
      (addr - 0x2000) % 8 match {
        case 0 =>
          writeCtrl(d)

        case 1 =>
          writeMask(d)

        case 3 =>
          writeOamAddr(d)

        case 4 =>
          writeOamData(d)

        case 5 =>
          writeScroll(d)

        case 6 =>
          writeAddr(d)

        case 7 =>
          writeData(d)

        case _ =>

      }
    }
  }

  private def writeCtrl(d: Int): Unit = {
    nmiOutput = isBitSet(7, d)
    sprSize = isBitSet(5, d)
    bgPtAddr = isBitSet(4, d)
    sprPtAddr = isBitSet(3, d)
    vRamAddrInc = isBitSet(2, d)
    baseNtAddr = d & 0x3

    // t: ...BA.. ........ = d: ......BA
    val e = d & 0x3
    val initT = interT & ~0xc00
    interT = initT | (e << 10)
  }

  private def writeMask(d: Int): Unit = {
    showSpr = isBitSet(4, d)
    showBg = isBitSet(3, d)
    showSprInLeftmost = isBitSet(2, d)
    showBgInLeftmost = isBitSet(1, d)
  }

  private def readStatus(): Int = {
    val vb = if (nmiOccurred) 1 else 0
    val szh = if (sprZeroHit) 1 else 0
    val sov = if (sprOverflow) 1 else 0
    val rsl1 = vb << 7 | szh << 6 | sov << 5
    val rsl2 = regPrevious & 0x1f

    nmiOccurred = false

    // w:                  = 0
    interW = 0

    regPrevious = rsl1 | rsl2
    regPrevious
  }

  private def writeOamAddr(d: Int): Unit = {
    oamAddr = d & 0xff
  }

  private def readOamData(): Int = {
    regPrevious = oam(oamAddr)
    val renderingEnabled = showBg || showSpr
    if (renderingEnabled && scanline <= 240) {
      if (cycle < 64) 0xff
      else if (cycle <= 256) 0x00
      else oam(oamAddr)
    } else {
      regPrevious
    }
  }

  private def writeOamData(d: Int): Unit = {
    oam(oamAddr) = d
    oamAddr += 1
    oamAddr = oamAddr.wrapAround8
  }

  private def writeScroll(d: Int): Unit = {
    if (interW == 0) {
      // t: ....... ...HGFED = d: HGFED...
      // x:              CBA = d: .....CBA
      // w:                  = 1
      interT = interT & ~0x1f
      interT = interT | (d >>> 3)
      interX = d & 0x7
      interW = 1

    } else {
      // t: CBA..HG FED..... = d: HGFEDCBA
      // w:                  = 0
      interT = interT & ~0x7000
      interT = interT | ((d & 0x7) << 12)
      interT = interT & ~0x3e0
      interT = interT | (d & 0xf8) << 2
      interW = 0
    }
  }

  private def writeAddr(d: Int): Unit = {
    if (interW == 0) {
      // t: .FEDCBA ........ = d: ..FEDCBA
      // t: X...... ........ = 0
      // w:                  = 1
      interT = (interT & 0xc0ff) | ((d & 0x3f) << 8)
      interT = interT & 0x3fff
      interW = 1
    } else {
      // t: ....... HGFEDCBA = d: HGFEDCBA
      // v                   = t
      // w:                  = 0
      val init = interT & 0xfff00
      interT = init | d
      interV = interT
      interW = 0
    }
  }

  private def readData(): Int = {
    var tmp = 0x0
    val renderingEnabled = showBg || showSpr
    if ((interV & 0x3fff) < 0x3f00) {
      tmp = pDataBuffer
      pDataBuffer = ppuMemory.read(interV & 0x3fff)
    } else {
      pDataBuffer = ppuMemory.read((interV & 0x3fff) - 0x1000)
      tmp = ppuMemory.read(interV)
    }

    if (!renderingEnabled || (scanline > 240 && scanline <= 260)) {
      interV += addrInc
    }
    regPrevious = tmp
    regPrevious
  }

  private def writeData(d: Int): Unit = {
    val renderingEnabled = showBg || showSpr
    ppuMemory.write(interV & 0x3fff, d)
    interV += addrInc
  }

  def clock(): ArrayBuffer[Int] = scan()

  private def incrementX(): Unit = {
    var v = interV
    if ((v & 0x001f) == 31) {
      v = v & ~0x001f
      v = v ^ 0x0400
      interV = v
    } else {
      v += 1
      interV = v
    }
  }

  private def incrementY(): Unit = {
    var v = interV
    if ((v & 0x7000) != 0x7000) {
      interV = v + 0x1000
    } else {
      v = v & ~0x7000
      var y = (v & 0x03e0) >>> 5
      if (y == 29) {
        y = 0
        v = v ^ 0x0800
      } else if (y == 31) {
        y = 0
      } else {
        y += 1
      }
      interV = (v & ~0x03e0) | (y << 5)
    }
  }

  private def copyX(): Unit = {
    // v: ....F.. ...EDCBA = t: ....F.. ...EDCBA
    interV = (interV & ~0x41f) | (interT & 0x041f)
  }

  private def copyY(): Unit = {
    // v: .IHGF.ED CBA..... = t: .IHGF.ED CBA.....
    interV = (interV & ~0x7be0) | (interT & 0x7be0)
  }

  private def shift(): Unit = {
    bgBitmapDataHighShift = bgBitmapDataHighShift << 1
    bgBitmapDataLowShift = bgBitmapDataLowShift << 1
  }

  private def reloadBgData(): Unit = {
    // BG tile higher
    bgBitmapDataHighShift = bgBitmapDataHighShift | bgTileBitmapHighLatch

    // BG tile lower
    bgBitmapDataLowShift = bgBitmapDataLowShift | bgTileBitmapLowLatch

    // AT Byte
    bgPaletteAttrShift = (bgPaletteAttrShift << 8) | atByteLatch
  }

  private def fetchNtByte: Int = {
    ppuMemory.read(0x2000 | (interV & 0xfff))
  }

  private def fetchAtByte: Int = {
    val atBase = 0x23c0 | (interV & 0x0c00)
    val coarseXOffsetMul4 = (interV >>> 2) & 0x07
    val coarseYOffsetMul4 = (interV >>> 4) & 0x38
    val addr = atBase | coarseYOffsetMul4 | coarseXOffsetMul4
    ppuMemory.read(addr)
  }

  private def getAtBits(offset: Int): Int = {
    val atByte = bgPaletteAttrShift >> 8
    val y = scanline
    val x = cycle - 1

    val upper1Y = (y >>> 4) & 0x1
    val upper1X = (x >>> 4) & 0x1

    ((upper1X % 2) == 1, (upper1Y % 2) == 1) match {
      case (false, false) => atByte & 0x3
      case (true, false) => (atByte >>> 2) & 0x3
      case (false, true) => (atByte >>> 4) & 0x3
      case (true, true) => (atByte >>> 6) & 0x3
    }
  }

  private def fetchBgTile(isLow: Boolean): Int = {
    val leftOrRight = if (bgPtAddr) 0x1000 else 0x0
    val yOffset = (interV >>> 12) & 0x7

    if (isLow) {
      ppuMemory.read(leftOrRight | ntByteLatch << 4 | 0x0 << 3 | yOffset)
    } else {
      ppuMemory.read(leftOrRight | ntByteLatch << 4 | 0x1 << 3 | yOffset)
    }
  }

  private def createBgPixel(): Unit = {
    val offset = (scanline << 8) + (cycle - 1)
    if (!showBgInLeftmost && (offset & 0xff) < 8) {
      bgPixel = false
      bgRgb = colorPalette(0)

    } else {
      val palOffset = (bitAt(15 - interX, bgBitmapDataHighShift) << 1) | bitAt(15 - interX, bgBitmapDataLowShift)
      val attribute = getAtBits(15 - interX)
      val rgb = colorPalette(ppuMemory.read(0x3f00 | (attribute << 2) | palOffset))
      bgPixel = if (palOffset != 0) true else false
      bgRgb = rgb
    }
  }

  private def evaluateSpr(): Unit = {
    val b = new Breaks()
    val start = (0 to 255).filter(i => i % 4 == 0).map(i => i + oamStart).filter(i => i < 255)
    val size: Int = if (sprSize) 15 else 7
    val offset: Int => Int = yPos => scanline - yPos
    val isOutOfRange: Int => Boolean = yPos => {
      yPos > scanline || offset(yPos) > size
    }

    findSpr0 = false
    oamFind = 0
    (0 to 31).foreach(i => secOam(i) = 0xff)

    b.breakable {
      for (s <- start) {
        val yPos = oam(s)

        if (!isOutOfRange(yPos)) {
          if (s == 0) findSpr0 = true

          if (oamFind >= 8) {
            oamFind = 8
            sprOverflow = true
            b.break()
          } else {
            secOam(oamFind * 4) = oam(s)
            secOam(oamFind * 4 + 1) = oam(s + 1)
            secOam(oamFind * 4 + 2) = oam(s + 2)
            secOam(oamFind * 4 + 3) = oam(s + 3)

            oamFind += 1
          }
        }
      }
    }

    for (rest <- oamFind to 7) {
      sprXLatch(rest) = 0x0
      sprBitmapDataHighShift(rest) = 0x0
      sprBitmapDataLowShift(rest) = 0x0
    }
  }

  private def fetchSprTile(ind: Int, isLow: Boolean, tile: Int, offset: Int, attr: Int): Int = {
    val flipHori = isBitSet(6, attr)
    val flipVert = isBitSet(7, attr)
    val base = if (sprPtAddr) 0x1 << 12 else 0x0
    val tileNum = tile
    val size = if (sprSize) 15 else 7

    var o = offset
    if (flipVert) o = size - o

    val addrLower = base | (tileNum << 4) | (0x0 << 3) | o
    val addrHigher = base | (tileNum << 4) |  (0x1 << 3) | o

    (isLow, flipHori) match {
      case (false, false) => reverseBit(ppuMemory.read(addrHigher))
      case (false, true) => ppuMemory.read(addrHigher)
      case (true, false) => reverseBit(ppuMemory.read(addrLower))
      case (true, true) => ppuMemory.read(addrLower)
    }
  }

  private def createSprPixel(): Unit = {
    val startDraw = if (showSprInLeftmost) 0 else 8
    var noSpr = true

    for (y <- (0 until oamFind).reverse) {
      val offset = (cycle - 1) - sprXLatch(y)
      if (offset >= 0 && offset <= 8) {
        if (bitAt(0, sprBitmapDataHighShift(y)) + bitAt(0, sprBitmapDataLowShift(y)) != 0) {
          sprIndex = y
          val pixel = ((sprBitmapDataHighShift(y) & 0x1) << 1) | (sprBitmapDataLowShift(y) & 0x1)
          val attribute = (sprAttrLatch(y) & 0x3) + 4
          val rgb = colorPalette(ppuMemory.read(0x3f00 | (attribute << 2) | pixel))
          val priority = isBitSet(5, sprAttrLatch(y))

          sprPixel = if (pixel != 0) true else false
          sprRgb = rgb
          sprPriority = priority

          noSpr = false
        }

        sprBitmapDataHighShift(y) = sprBitmapDataHighShift(y) >>> 1
        sprBitmapDataLowShift(y) = sprBitmapDataLowShift(y) >>> 1
      }
    }

    if (noSpr || (cycle - 1) < startDraw || !showSpr) {
      sprPixel = false
      sprRgb = colorPalette(0)
      sprPriority = false
    }
  }

  private def createPixel(): (Int, Int, Int) = {
    (bgPixel, sprPixel, sprPriority) match {
      case (false, false, _) =>
        colorPalette(ppuMemory.read(0x3f00)) // BG ($3F00)

      case (false, true, _) =>
        sprRgb // Sprite

      case (true, false, _) =>
        bgRgb // BG

      case (true, true, false) =>
        if((showSpr && showBg) &&
          sprIndex == 0 &&
          (cycle - 1) < 255 &&
          findSpr0) {
          sprZeroHit = true
        }
        sprRgb // Sprite

      case (true, true, true) =>
        if((showSpr && showBg) &&
          sprIndex == 0 &&
          (cycle - 1) < 255 &&
          findSpr0) {
          sprZeroHit = true
        }
        bgRgb // BG
    }
  }

  private def manageCounter(): Unit = {
    val renderingEnabled = showBg || showSpr
    if (renderingEnabled && scanline == 261 && cycle == 339 && isOddScanline) {
      cycle = 0
      scanline = 0
      isOddScanline = !isOddScanline
      return
    }
    cycle += 1

    if (cycle > 340) {
      cycle = 0
      scanline += 1
      if (scanline > 261) {
        scanline = 0
        isOddScanline = !isOddScanline
      }
    }
  }

  private def scan(): ArrayBuffer[Int] = {
    manageCounter()

    if (nmiOccurred && nmiOutput) {
      openBus.setNmi()
    } else {
      openBus.clearNmi()
    }

    val renderingEnabled = showBg || showSpr
    val preRenderScanline = scanline == 261
    val visibleScanline = scanline < 240
    val renderScanline = preRenderScanline || visibleScanline
    val preFetchCycle = cycle >= 321 && cycle <= 336
    val visibleCycle = cycle >= 1 && cycle <= 256
    val fetchCycle = preFetchCycle || visibleCycle

    if (renderingEnabled) {
      if (visibleScanline & visibleCycle) {
          createBgPixel()
          createSprPixel()
          val p = createPixel()
          lineBuffer += (255 << 24 | p._1 << 16 | p._2 << 8 | p._3)
      }

      if (renderScanline && fetchCycle) {
        shift()
        cycle % 8 match {
          case 1 => ntByteLatch = fetchNtByte
          case 3 => atByteLatch = fetchAtByte
          case 5 => bgTileBitmapLowLatch = fetchBgTile(isLow = true)
          case 7 => bgTileBitmapHighLatch = fetchBgTile(isLow = false)
          case 0 => reloadBgData()
          case _ =>
        }
      }

      if (preRenderScanline && cycle >= 280 && cycle <= 304) {
        copyY()
      }

      if (renderScanline) {
        if (fetchCycle && cycle % 8 == 0) {
          incrementX()
        }

        if (cycle == 256) {
          incrementY()
        }

        if (cycle == 257) {
          copyX()
        }
      }
    }

    // sprites
    if (renderingEnabled && cycle == 65) {
      oamStart = oamAddr
    } else if (cycle > 257 && cycle < 341) {
      oamAddr = 0
    }

    if (renderingEnabled) {
      if (cycle == 260 && visibleScanline) {
          evaluateSpr()

          for (i <- 0 until oamFind) {
            val yPos = scanline - secOam(i * 4)
            val tile = secOam(i * 4 + 1)
            val attr = secOam(i * 4 + 2)

            sprAttrLatch(i) = attr
            sprXLatch(i) = secOam(i * 4 + 3)
            sprBitmapDataHighShift(i) = fetchSprTile(i, isLow = false, tile,  yPos, attr)
            sprBitmapDataLowShift(i) = fetchSprTile(i, isLow = true, tile,  yPos, attr)
          }
      }
    }

    // flags
    if (scanline == 241 && cycle == 1) {
      nmiOccurred = true
    }

    if (preRenderScanline && cycle == 0) {
      nmiOccurred = false
      sprZeroHit = false
      sprOverflow = false
    }

    // generate pixel line information to draw
    if (cycle == 257 && scanline == 239) {
      val lb = lineBuffer.reverse
      lineBuffer.clear()
      lb
    } else {
      ArrayBuffer()
    }
  }
}

object Ppu {
  def apply(ppuMemory: PpuMemory, ram: Memory2, openBus: OpenBus): Ppu = new Ppu(ppuMemory, ram, openBus)
}