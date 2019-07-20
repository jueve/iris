import Util._

class Cpu(memory: CpuMemory, openBus: OpenBus) {
  import Instruction._
  import AddressingMode._

  // register
  private var A, X, Y = 0x00
  private var S = 0xfd

  // program counter
  private var PC = memory.read(0xfffd) << 8 | memory.read(0xfffc)
  private var newPC = 0
  private var jump = false

  // flags
  private var negative = false
  private var overflow = false
  private var break = false
  private var decimal = false
  private var interruptDisable = true
  private var zero = false
  private var carry = false

  // cycles
  private var cycles = 0

  // nmi
  private var prevNmi, nmiNext = false
  private var isOddCycle = false

  // private var interrupt = 0
  private var interruptDelay = false
  private var previntflag = false

  private def flagsToByte: Int = {
    val n = if (negative) 0x1 else 0x0
    val v = if (overflow) 0x1 else 0x0
    val x = 0x1
    val b = if (break) 0x1 else 0x0
    val d = if (decimal) 0x1 else 0x0
    val i = if (interruptDisable) 0x1 else 0x0
    val z = if (zero) 0x1 else 0x0
    val c = if (carry) 0x1 else 0x0
    n << 7 | v << 6 | x << 5 | b << 4 | d << 3 | i << 2 | z << 1 | c
  }

  def parseP(d: Int): Unit = {
    if (isBitSet(7, d)) negative = true else negative = false
    if (isBitSet(6, d)) overflow = true else overflow = false
    if (isBitSet(4, d)) break = true else break = false
    if (isBitSet(3, d)) decimal = true else decimal = false
    if (isBitSet(2, d)) interruptDisable = true else interruptDisable = false
    if (isBitSet(1, d)) zero = true else zero = false
    if (isBitSet(0, d)) carry = true else carry = false
  }

  private def push(d: Int): Unit = {
    memory.write(0x0100 | S, d)
    S -= 1
  }

  private def pull(): Int = {
    val d = memory.read(0x0100 | S + 1)
    S += 1
    d
  }

  private def actNmi(): Unit = {
    push(PC >>> 8)
    push(PC & 0xff)
    push(flagsToByte & ~0x10)
    PC = memory.read(0xfffa) + (memory.read(0xfffb) << 8)
    interruptDisable = true
    cycles += 7
  }

//  private def actInterrupt(): Unit = {
//    push(PC >>> 8)
//    push(PC & 0xff)
//    push(flagsToByte & ~0x10)
//    PC = memory.read(0xfffe) + (memory.read(0xffff) << 8)
//    interruptDisable = true
//  }

  private def actBreakInterrupt(): Unit = {
    PC += 1
    push(PC >> 8)
    push(PC & 0xFF)
    push(flagsToByte | 0x10 | 0x20)
    PC = memory.read(0xFFFE) + (memory.read(0xFFFF) << 8)
    interruptDisable = true
  }

  private def delayInterrupt(): Unit = {
    interruptDelay = true
    previntflag = interruptDisable
  }

  private def fetchOperand(offset: Int): Int = {
    val operand = memory.read(PC + offset)
    Option(operand)
      .getOrElse(throw new RuntimeException(s"Couldn't get value from address ${PC + offset}"))
  }

  def clock(): Int = {
    cycles = 0
    jump = false

    if (openBus.cpuStallCondition && isOddCycle) {
      cycles += 514
      openBus.clearCpuStall()

    } else if (openBus.cpuStallCondition && !isOddCycle) {
      cycles += 513
      openBus.clearCpuStall()

    } else {
      // nmi
      if (nmiNext) {
        actNmi()
        nmiNext = false
      }
      if (openBus.nmiCondition && !prevNmi) {
        nmiNext = true
      }
      prevNmi = openBus.nmiCondition
      openBus.clearNmi()

      val opCode = memory.read(PC)
      val name = instrName(opCode)
      val len = instrLen(opCode)
      val mode = instrAddressingMode(opCode)

      // execution
      execute(name, mode)

      if (jump) {
        PC = newPC
      } else {
        PC += len
      }

      cycles += instrCycle(opCode)
    }

    if (cycles % 2 == 1) isOddCycle = true
    else isOddCycle = false

    cycles
  }

  private def calcAddr(mode: AddressingMode, addCyc: Boolean = false): Int = {
    mode match {
      case Zeropage =>
        fetchOperand(1)

      case ZeropageX =>
        (fetchOperand(1) + X).wrapAround8

      case ZeropageY =>
        (fetchOperand(1) + Y).wrapAround8

      case Absolute =>
        (fetchOperand(2) << 8) | fetchOperand(1)

      case AbsoluteX =>
        val abs = (fetchOperand(2) << 8) | fetchOperand(1)
        val added = (abs + X).wrapAround16

          if (((abs & ~0xff) != (added & ~0xff)) && addCyc) {
          cycles += 1
          added
        } else {
          added
        }

      case AbsoluteY =>
        val abs = (fetchOperand(2) << 8) | fetchOperand(1)
        val added = (abs + Y).wrapAround16

        if (((abs & ~0xff) != (added & ~0xff)) && addCyc) {
          cycles += 1
          added
        } else {
          added
        }

      case Indirect =>
        val tmp = (fetchOperand(2) << 8) | fetchOperand(1)
        if (((tmp + 1) & 0xff) == 0x00) {
          tmp + 1
        } else {
          (memory.read(tmp + 1) << 8) | memory.read(tmp)
        }

      case IndexedIndirect =>
        val indexed = (fetchOperand(1) + X).wrapAround8
        (memory.read((indexed + 0x1).wrapAround8) << 8) | memory.read(indexed)

      case IndirectIndexed =>
        val indirect = (memory.read((fetchOperand(1) + 0x1).wrapAround8) << 8) | memory.read(fetchOperand(1))
        val indexed = (indirect + Y).wrapAround16

        if (((indirect & ~0xff) != (indexed & ~0xff)) && addCyc) {
          cycles += 1
          indexed
        } else {
          indexed
        }

      case Implied | Immediate => 0 // not used

      case _ =>
        throw new RuntimeException(s"Invalid Addressing Mode $mode")
    }
  }

  private def branch(cond: Boolean): Unit = {
    if (cond) {
      val v = fetchOperand(1)
      val signed = v.toByte
      val prev = PC
      val dest = prev + 2 + signed

      newPC = dest
      jump = true

      if ((dest & ~0xff) == ((prev + 2) & ~0xff)) {
        cycles += 1
      } else {
        cycles += 2
      }
    }
  }

  private def execute(instr: Instruction, mode: AddressingMode): Unit = {
    instr match {
      case BRK() => brk()
      case ORA() => ora(mode)
      case KIL() => // do nothing
      case SLO() => slo(mode)
      case NOP() => nop(mode)
      case ASL() => asl(mode)
      case PHP() => php()
      case ANC() => anc()
      case BPL() => bpl()
      case CLC() => clc()
      case JSR() => jsr()
      case AND() => and(mode)
      case RLA() => rla(mode)
      case BIT() => bit(mode)
      case ROL() => rol(mode)
      case PLP() => plp()
      case BMI() => bmi()
      case SEC() => sec()
      case RTI() => rti()
      case EOR() => eor(mode)
      case SRE() => sre(mode)
      case LSR() => lsr(mode)
      case PHA() => pha()
      case JMP() => jmp(mode)
      case ASR() => // not defined
      case BVC() => bvc()
      case CLI() => cli()
      case RTS() => rts()
      case ADC() => adc(mode)
      case RRA() => rra(mode)
      case ROR() => ror(mode)
      case PLA() => pla()
      case ARR() => // not defined
      case BVS() => bvs()
      case SEI() => sei()
      case STA() => sta(mode)
      case SAX() => sax(mode)
      case STY() => sty(mode)
      case STX() => stx(mode)
      case DEY() => dey()
      case TXA() => txa()
      case XAA() => // not defined
      case BCC() => bcc()
      case AXA() => // not defined
      case TYA() => tya()
      case XAS() => // not defined
      case TXS() => txs(mode)
      case SYA() => // not defined
      case SXA() => // not defined
      case LDY() => ldy(mode)
      case LDA() => lda(mode)
      case LDX() => ldx(mode)
      case LAX() => lax(mode)
      case TAY() => tay()
      case TAX() => tax()
      case ATX() => // not defined
      case BCS() => bcs()
      case CLV() => clv()
      case TSX() => tsx()
      case LAR() => // not defined
      case CPY() => cpy(mode)
      case CMP() => cmp(mode)
      case DEC() => dec(mode)
      case DCP() => dcp(mode)
      case INY() => iny()
      case DEX() => dex()
      case AXS() => // not defined
      case BNE() => bne()
      case CLD() => cld()
      case CPX() => cpx(mode)
      case SBC() => sbc(mode)
      case ISB() => isb(mode)
      case INC() => inc(mode)
      case INX() => inx()
      case BEQ() => beq()
      case SED() => sed()
    }
  }

  private def brk(): Unit = {
    actBreakInterrupt()
  }

  private def ora(mode: AddressingMode): Unit = {
    if (mode == Immediate) {
      val prev = A
      val rsl = prev | fetchOperand(1)

      A = rsl

      if (rsl == 0x0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false

    } else {
      val prev = A
      val addr = calcAddr(mode)
      val rsl = prev | memory.read(addr)

      A = rsl

      if (rsl == 0x0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false
    }
  }

  private def slo(mode: AddressingMode): Unit = {
    val addr = calcAddr(mode)
    val prev = memory.read(addr)
    val rsl = (prev << 1) & 0xff

    memory.write(addr, rsl)
    A = A | rsl

    if (isBitSet(7, prev)) carry = true else carry = false
    if (A == 0x00) zero = true else zero = false
    if (rsl == 0x00) zero = true else zero = false
    if (isBitSet(7, rsl)) negative = true else negative = false
    if (isBitSet(7, A)) negative = true else negative = false
  }

  private def nop(mode: AddressingMode): Unit = {
    calcAddr(mode, true)
  }

  private def asl(mode: AddressingMode): Unit = {
    if (mode == Accumulator) {
      val prev = A
      val rsl = (prev << 1).wrapAround8

      A = rsl

      if (isBitSet(7, prev)) carry = true else carry = false
      if (isBitSet(7, rsl)) negative = true else negative = false
      if (rsl == 0x00) zero = true else zero = false

    } else {
      val addr= calcAddr(mode)
      val prev = memory.read(addr)
      val rsl = (prev << 1).wrapAround8

      memory.write(addr, rsl)

      if (isBitSet(7, prev)) carry = true else carry = false
      if (isBitSet(7, rsl)) negative = true else negative = false
      if (rsl == 0x00) zero = true else zero = false
    }
  }

  private def php(): Unit = {
    break = true
    push(flagsToByte)
    break = false
  }

  private def anc(): Unit = {
    val rsl = A & fetchOperand(1)

    A = rsl

    if (isBitSet(7, rsl)) carry = true else carry = false
    if (rsl == 0x0) zero = true else zero = false
    if (isBitSet(7, rsl)) negative = true else negative = false
  }

  private def bpl(): Unit = {
    branch(!negative)
  }

  private def clc(): Unit = {
    carry = false
  }

  private def jsr(): Unit = {
    val returnPoint = (PC + 3) - 1

    push((returnPoint >>> 8) & 0xff) // PCH
    push(returnPoint & 0xff) // PCL

    newPC = calcAddr(Absolute)
    jump = true
  }

  private def and(mode: AddressingMode): Unit = {
    if (mode == Immediate) {
      val prev = A
      val rsl = prev & fetchOperand(1)

      A = rsl

      if (rsl == 0x0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false

    } else {
      val prev = A
      val addr = calcAddr(mode)
      val rsl = prev & memory.read(addr)

      A = rsl

      if (rsl == 0x0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false
    }
  }

  private def rla(mode: AddressingMode): Unit = {
    val addr = calcAddr(mode)
    val prev = memory.read(addr)
    val c = if (carry) 0x1 else 0x0
    val rsl = ((prev << 1) | c).wrapAround8

    memory.write(addr, rsl)
    A = A & rsl

    if (isBitSet(7, prev)) carry = true else carry = false
    if (A == 0x0) zero = true else zero = false
    if (rsl == 0x0) zero = true else zero = false
    if (isBitSet(7, rsl)) negative = true else negative = false
    if (isBitSet(7, A)) negative = true else negative = false
  }

  private def bit(mode: AddressingMode): Unit = {
    val m = memory.read(calcAddr(mode))

    if ((A & m) == 0x0) zero = true else zero = false
    if (isBitSet(6, m)) overflow = true else overflow = false
    if (isBitSet(7, m)) negative = true else negative = false
  }

  private def rol(mode: AddressingMode): Unit = {
    if (mode == Accumulator) {
      val prev = A
      val c = if (carry) 0x1 else 0x0
      val rsl = ((prev << 1) | c).wrapAround8

      A = rsl

      if (isBitSet(7, prev)) carry = true else carry = false
      if (A == 0x0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false
    } else {
      val addr = calcAddr(mode)
      val prev = memory.read(addr)
      val c = if (carry) 0x1 else 0x0
      val rsl = ((prev << 1) | c).wrapAround8

      memory.write(addr, rsl)

      if (isBitSet(7, prev)) carry = true else carry = false
      if (A == 0x0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false
    }
  }

  private def plp(): Unit = {
    parseP(pull())
    break = false
  }

  private def bmi(): Unit = {
    branch(negative)
  }

  private def sec(): Unit = {
    carry = true
  }

  private def rti(): Unit = {
    break = true
    parseP(pull())
    break = false
    val l = pull()
    val h = pull() << 8

    newPC = h | l

    jump = true
  }

  private def eor(mode: AddressingMode): Unit = {
    if (mode == Immediate) {
      val prev = A
      val rsl = prev ^ fetchOperand(1)

      A = rsl

      if (rsl == 0x0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false

    } else {
      val prev = A
      val addr = calcAddr(mode)
      val rsl = prev ^ memory.read(addr)

      A = rsl

      if (rsl == 0x0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false
    }
  }

  private def sre(mode: AddressingMode): Unit = {
    val addr = calcAddr(mode)
    val prev = memory.read(addr)
    val rsl = (prev >>> 1) & 0xff

    memory.write(addr, rsl)
    A = A ^ rsl

    if (isBitSet(0, prev)) carry = true else carry = false
    if (A == 0x00) zero = true else zero = false
    if (rsl == 0x00) zero = true else zero = false
    if (isBitSet(7, rsl)) negative = true else negative = false
    if (isBitSet(7, A)) negative = true else negative = false
  }

  private def lsr(mode: AddressingMode): Unit = {
    if (mode == Accumulator) {
      val prev = A
      val rsl = (prev >>> 1).wrapAround8

      A = rsl

      if (isBitSet(0, prev)) carry = true else carry = false
      if (isBitSet(7, rsl)) negative = true else negative = false
      if (rsl == 0x00) zero = true else zero = false

    } else {
      val addr= calcAddr(mode)
      val prev = memory.read(addr)
      val rsl = (prev >>> 1).wrapAround8

      memory.write(addr, rsl)

      if (isBitSet(0, prev)) carry = true else carry = false
      if (isBitSet(7, rsl)) negative = true else negative = false
      if (rsl == 0x00) zero = true else zero = false
    }
  }


  private def pha(): Unit = {
    push(A)
  }

  private def jmp(mode: AddressingMode): Unit = {
    newPC = calcAddr(mode)

    jump = true
  }

  private def bvc(): Unit = {
    branch(!overflow)
  }

  private def cli(): Unit = {
    delayInterrupt()

    interruptDisable = false
  }

  private def rts(): Unit = {
    val l = pull()
    val h = pull() << 8

    newPC = (h | l) + 1

    jump = true
  }

  private def adc(mode: AddressingMode): Unit = {
    if (mode == Immediate) {
      val prev = A
      val c = if (carry) 1 else 0
      val unsigned = prev + fetchOperand(1) + c
      val signed = prev.toByte + fetchOperand(1).toByte + c.toByte

      A = unsigned.wrapAround8

      if (unsigned > 0xff || signed == 0x00) carry = true else carry = false
      if (signed == 0x00) zero = true else zero = false
      if (signed > 127) overflow = true else overflow = false
      if (isBitSet(7, unsigned)) negative = true else negative = false

    } else {
      val prev = A
      val addr = calcAddr(mode)
      val c = if (carry) 1 else 0
      val unsigned = prev + memory.read(addr) + c
      val signed = prev.toByte + memory.read(addr).toByte + c.toByte

      A = unsigned.wrapAround8

      if (unsigned > 0xff || signed == 0x00) carry = true else carry = false
      if (signed == 0x00) zero = true else zero = false
      if (signed > 127) overflow = true else overflow = false
      if (isBitSet(7, unsigned)) negative = true else negative = false
    }
  }

  private def rra(mode: AddressingMode): Unit = {
    val addr = calcAddr(mode)
    val prev = memory.read(addr)
    val cr = if (carry) 0x1 else 0x0
    val rotate = ((prev >>> 1) | (cr << 7)).wrapAround8

    // flag operation "ROR"
    if (isBitSet(0, prev)) carry = true else carry = false
    if (A == 0x0) zero = true else zero = false
    if (isBitSet(7, rotate)) negative = true else negative = false

    // arithmetic
    val c = if (carry) 0x1 else 0x0
    val unsigned = A + rotate + c
    val signed = A.toByte + rotate.toByte + c.toByte

    // write back
    memory.write(addr, rotate)
    A = unsigned.wrapAround8

    // flag operation "ADC"
    if (unsigned > 0xff | signed == 0x00) carry = true else carry = false
    if (signed == 0x00) zero = true else zero = false
    if (signed > 127) overflow = true else overflow = false
    if (isBitSet(7, unsigned)) negative = true else negative = false
  }

  private def ror(mode: AddressingMode): Unit = {
    if (mode == Accumulator) {
      val prev = A
      val c = if (carry) 0x1 else 0x0
      val rsl = ((prev >>> 1) | (c << 7)).wrapAround8

      A = rsl

      if (isBitSet(0, prev)) carry = true else carry = false
      if (A == 0x0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false

    } else {
      val addr = calcAddr(mode)
      val prev = memory.read(addr)
      val c = if (carry) 0x1 else 0x0
      val rsl = ((prev >>> 1) | (c << 7)).wrapAround8

      memory.write(addr, rsl)

      if (isBitSet(0, prev)) carry = true else carry = false
      if (A == 0x0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false
    }
  }

  private def pla(): Unit = {
    val rsl = pull()

    A = rsl

    if (rsl == 0x0) zero = true else zero = false
    if (isBitSet(7, rsl)) negative = true else negative = false
  }

  private def bvs(): Unit = {
    branch(overflow)
  }

  private def sei(): Unit = {
    delayInterrupt()
    interruptDisable = true
  }

  private def sta(mode: AddressingMode): Unit = {
    memory.write(calcAddr(mode), A)
  }

  private def sax(mode: AddressingMode): Unit = {
    memory.write(calcAddr(mode), A & X)
  }

  private def sty(mode: AddressingMode): Unit = {
    memory.write(calcAddr(mode), Y)
  }

  private def stx(mode: AddressingMode): Unit = {
    memory.write(calcAddr(mode), X)
  }

  private def dey(): Unit = {
    val rsl = (Y - 1).wrapAround8

    Y = rsl

    if (rsl == 0x0) zero = true else zero = false
    if (isBitSet(7, rsl)) negative = true else negative = false
  }

  private def txa(): Unit = {
    A = X

    if (A == 0x0) zero = true else zero = false
    if (isBitSet(7, A)) negative = true else negative = false
  }

  private def bcc(): Unit = {
    branch(!carry)
  }

  private def tya(): Unit = {
    A = Y

    if (A == 0x0) zero = true else zero = false
    if (isBitSet(7, A)) negative = true else negative = false
  }

  private def txs(mode: AddressingMode): Unit = {
    S = X
  }

  private def ldy(mode: AddressingMode): Unit = {
    if (mode == Immediate) {
      val rsl = fetchOperand(1)

      Y = rsl

      if (rsl == 0x0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false

    } else {
      val rsl = memory.read(calcAddr(mode, true))

      Y = rsl

      if (rsl == 0x0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false
    }
  }

  private def lda(mode: AddressingMode): Unit = {
    if (mode == Immediate) {
      val rsl = fetchOperand(1)

      A = rsl

      if (rsl == 0x0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false

    } else {
      val rsl = memory.read(calcAddr(mode, true))

      A = rsl

      if (rsl == 0x0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false
    }
  }

  private def ldx(mode: AddressingMode): Unit = {
    if (mode == Immediate) {
      val rsl = fetchOperand(1)

      X = rsl

      if (rsl == 0x0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false

    } else {
      val rsl = memory.read(calcAddr(mode, true))

      X = rsl

      if (rsl == 0x0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false
    }
  }

  private def lax(mode: AddressingMode): Unit = {
    val d = memory.read(calcAddr(mode, true))

    A = d
    X = d

    if (isBitSet(7, d)) negative = true else negative = false
    if (d == 0x0) zero = true else zero = false
  }

  private def tay(): Unit = {
    Y = A

    if (Y == 0x0) zero = true else zero = false
    if (isBitSet(7, Y)) negative = true else negative = false
  }

  private def tax(): Unit = {
    X = A

    if (X == 0x0) zero = true else zero = false
    if (isBitSet(7, X)) negative = true else negative = false
  }

  private def bcs(): Unit = {
    branch(carry)
  }

  private def clv(): Unit = {
    overflow = false
  }

  private def tsx(): Unit = {
    X = S

    if (X == 0x0) zero = true else zero = false
    if (isBitSet(7, X)) negative = true else negative = false
  }

  private def cpy(mode: AddressingMode): Unit = {
    if (mode == Immediate) {
      val rsl = Y - fetchOperand(1)

      if (rsl >= 0) carry = true else carry = false
      if (rsl == 0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false

    } else {
      val rsl = Y - memory.read(calcAddr(mode))

      if (rsl >= 0) carry = true else carry = false
      if (rsl == 0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false
    }
  }

  private def cmp(mode: AddressingMode): Unit = {
    if (mode == Immediate) {
      val rsl = A - fetchOperand(1)

      if (rsl >= 0) carry = true else carry = false
      if (rsl == 0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false

    } else {
      val rsl = A - memory.read(calcAddr(mode))

      if (rsl >= 0) carry = true else carry = false
      if (rsl == 0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false
    }
  }

  private def dec(mode: AddressingMode): Unit = {
    val rsl = memory.read(calcAddr(mode)) - 1

    memory.write(calcAddr(mode), rsl.wrapAround8)

    if (rsl == 0x0) zero = true else zero = false
    if (isBitSet(7, rsl)) negative = true else negative = false
  }

  private def dcp(mode: AddressingMode): Unit = {
    val addr = calcAddr(mode)
    val prev = memory.read(addr)
    val rsl = (prev - 1).wrapAround8

    memory.write(addr, rsl)
    val rsl2 = A - memory.read(addr)

    if (rsl2 >= 0x0) carry = true else carry = false
    if (rsl2 == 0x0) zero = true else zero = false
    if (isBitSet(7 , rsl2)) negative = true else negative = false
  }

  private def iny(): Unit = {
    val rsl = (Y + 1).wrapAround8

    Y = rsl

    if (rsl == 0x0) zero = true else zero = false
    if (isBitSet(7, rsl)) negative = true else negative = false
  }

  private def dex(): Unit = {
    val rsl = X - 1

    X = rsl.wrapAround8

    if (rsl == 0x0) zero = true else zero = false
    if (isBitSet(7, rsl)) negative = true else negative = false
  }

  private def bne(): Unit = {
    branch(!zero)
  }

  private def cld(): Unit = {
    decimal = false
  }

  private def cpx(mode: AddressingMode): Unit = {
    if (mode == Immediate) {
      val rsl = X - fetchOperand(1)

      if (rsl >= 0) carry = true else carry = false
      if (rsl == 0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false

    } else {
      val rsl = X - memory.read(calcAddr(mode))

      if (rsl >= 0) carry = true else carry = false
      if (rsl == 0) zero = true else zero = false
      if (isBitSet(7, rsl)) negative = true else negative = false
    }
  }

  private def sbc(mode: AddressingMode): Unit = {
    if (mode == Immediate) {
      val prev = A
      val c = if (carry) 1 else 0
      val unsigned = prev - fetchOperand(1) - (1 - c)
      val signed = prev.toByte - fetchOperand(1) - (1 - c).toByte

      A = unsigned.wrapAround8

      if (unsigned >= 0x00) carry = true else carry = false
      if (signed == 0x00) zero = true else zero = false
      if (signed < -128) overflow = true else overflow = false
      if (isBitSet(7, unsigned)) negative = true else negative = false

    } else {
      val prev = A
      val addr = calcAddr(mode)
      val c = if (carry) 1 else 0
      val unsigned = prev - memory.read(addr) - (1 - c)
      val signed = prev.toByte - memory.read(addr).toByte - (1 - c).toByte

      A = unsigned.wrapAround8

      if (unsigned >= 0x00) carry = true else carry = false
      if (signed == 0x00) zero = true else zero = false
      if (signed < -128) overflow = true else overflow = false
      if (isBitSet(7, unsigned)) negative = true else negative = false
    }
  }

  private def isb(mode: AddressingMode): Unit = {
    val addr = calcAddr(mode)
    val inc = (memory.read(addr) + 1).wrapAround8
    memory.write(addr, inc)

    val i = memory.read(addr)
    val c = if(carry) 0x1 else 0x0
    val unsigned = A - i - (1 - c)
    val signed = A.toByte - i.toByte - (1 - c).toByte

    A = unsigned.wrapAround8

    if (unsigned >= 0x00) carry = true else carry = false
    if (signed == 0x00) zero = true else zero = false
    if (signed < -128) overflow = true else overflow = false
    if (isBitSet(7, unsigned)) negative = true else negative = false
  }

  private def inc(mode: AddressingMode): Unit = {
    val addr = calcAddr(mode)
    val rsl = (memory.read(addr) + 1).wrapAround8

    memory.write(addr, rsl)

    if (rsl == 0x0) zero = true else zero = false
    if (isBitSet(7, rsl)) negative = true else negative = false
  }

  private def inx(): Unit = {
    val rsl = (X + 1).wrapAround8

    X = rsl

    if (rsl == 0x0) zero = true else zero = false
    if (isBitSet(7, rsl)) negative = true else negative = false
  }

  private def beq(): Unit = {
    branch(zero)
  }

  private def sed(): Unit = {
    decimal = true
  }

  private def kil(): Unit = {}

  private def asr(): Unit = {}

  private def arr(): Unit = {}

  private def xaa(): Unit = {}

  private def sxa(): Unit = {}

  private def axa(): Unit = {}

  private def xas(): Unit = {}

  private def sya(): Unit = {}

  private def lar(): Unit = {}

  private def axs(): Unit = {}

  val instrName: Vector[Instruction] = Vector(
    // 0x00-0x0f
    BRK(), ORA(), KIL(), SLO(),
    NOP(), ORA(), ASL(), SLO(),
    PHP(), ORA(), ASL(), ANC(),
    NOP(), ORA(), ASL(), SLO(),

    // 0x10-0x1f
    BPL(), ORA(), KIL(), SLO(),
    NOP(), ORA(), ASL(), SLO(),
    CLC(), ORA(), NOP(), SLO(),
    NOP(), ORA(), ASL(), SLO(),

    // 0x20-0x2f
    JSR(), AND(), KIL(), RLA(),
    BIT(), AND(), ROL(), RLA(),
    PLP(), AND(), ROL(), ANC(),
    BIT(), AND(), ROL(), RLA(),

    // 0x30-0x3f
    BMI(), AND(), KIL(), RLA(),
    NOP(), AND(), ROL(), RLA(),
    SEC(), AND(), NOP(), RLA(),
    NOP(), AND(), ROL(), RLA(),

    // 0x40-0x4f
    RTI(), EOR(), KIL(), SRE(),
    NOP(), EOR(), LSR(), SRE(),
    PHA(), EOR(), LSR(), ASR(),
    JMP(), EOR(), LSR(), SRE(),

    // 0x50-0x5f
    BVC(), EOR(), KIL(), SRE(),
    NOP(), EOR(), LSR(), SRE(),
    CLI(), EOR(), NOP(), SRE(),
    NOP(), EOR(), LSR(), SRE(),

    // 0x60-0x6f
    RTS(), ADC(), KIL(), RRA(),
    NOP(), ADC(), ROR(), RRA(),
    PLA(), ADC(), ROR(), ARR(),
    JMP(), ADC(), ROR(), RRA(),

    // 0x70-0x7f
    BVS(), ADC(), KIL(), RRA(),
    NOP(), ADC(), ROR(), RRA(),
    SEI(), ADC(), NOP(), RRA(),
    NOP(), ADC(), ROR(), RRA(),

    // 0x80-0x8f
    NOP(), STA(), NOP(), SAX(),
    STY(), STA(), STX(), SAX(),
    DEY(), NOP(), TXA(), XAA(),
    STY(), STA(), STX(), SAX(),

    // 0x90-0x9f
    BCC(), STA(), KIL(), AXA(),
    STY(), STA(), STX(), SAX(),
    TYA(), STA(), TXS(), XAS(),
    SYA(), STA(), SXA(), AXA(),

    // 0xa0-0xaf
    LDY(), LDA(), LDX(), LAX(),
    LDY(), LDA(), LDX(), LAX(),
    TAY(), LDA(), TAX(), XAS(),
    LDY(), LDA(), LDX(), LAX(),

    // 0xb0-0xbf
    BCS(), LDA(), KIL(), LAX(),
    LDY(), LDA(), LDX(), LAX(),
    CLV(), LDA(), TSX(), LAR(),
    LDY(), LDA(), LDX(), LAX(),

    // 0xc0-0xcf
    CPY(), CMP(), NOP(), DCP(),
    CPY(), CMP(), DEC(), DCP(),
    INY(), CMP(), DEX(), AXS(),
    CPY(), CMP(), DEC(), DCP(),

    // 0xd0-0xdf
    BNE(), CMP(), KIL(), DCP(),
    NOP(), CMP(), DEC(), DCP(),
    CLD(), CMP(), NOP(), DCP(),
    NOP(), CMP(), DEC(), DCP(),

    // 0xe0-0xef
    CPX(), SBC(), NOP(), ISB(),
    CPX(), SBC(), INC(), ISB(),
    INX(), SBC(), NOP(), SBC(),
    CPX(), SBC(), INC(), ISB(),

    // 0xf-0xff
    BEQ(), SBC(), KIL(), ISB(),
    NOP(), SBC(), INC(), ISB(),
    SED(), SBC(), NOP(), ISB(),
    NOP(), SBC(), INC(), ISB(),
  )

  val instrAddressingMode: Vector[AddressingMode] = Vector(
    // 0x00-0x0f
    Implied, IndexedIndirect, Implied, IndexedIndirect,
    Zeropage, Zeropage, Zeropage, Zeropage,
    Implied, Immediate, Accumulator, Immediate,
    Absolute, Absolute, Absolute, Absolute,

    // 0x10 - 0x1f
    Relative, IndirectIndexed, Implied, IndirectIndexed,
    ZeropageX, ZeropageX, ZeropageX, ZeropageX,
    Implied, AbsoluteY, Implied, AbsoluteY,
    AbsoluteX, AbsoluteX, AbsoluteX, AbsoluteX,

    // 0x20-0x2f
    Absolute, IndexedIndirect, Implied, IndexedIndirect,
    Zeropage, Zeropage, Zeropage, Zeropage,
    Implied, Immediate, Accumulator, Immediate,
    Absolute, Absolute, Absolute, Absolute,

    // 0x30-0x3f
    Relative, IndirectIndexed, Implied, IndirectIndexed,
    ZeropageX, ZeropageX, ZeropageX, ZeropageX,
    Implied, AbsoluteY, Implied, AbsoluteY,
    AbsoluteX, AbsoluteX, AbsoluteX, AbsoluteX,

    // 0x40-0x4f
    Implied, IndexedIndirect, Implied, IndexedIndirect,
    Zeropage, Zeropage, Zeropage, Zeropage,
    Implied, Immediate, Accumulator, Immediate,
    Absolute, Absolute, Absolute, Absolute,

    // 0x50-0x5f
    Relative, IndirectIndexed, Implied, IndirectIndexed,
    ZeropageX, ZeropageX, ZeropageX, ZeropageX,
    Implied, AbsoluteY, Implied, AbsoluteY,
    AbsoluteX, AbsoluteX, AbsoluteX, AbsoluteX,

    // 0x60-0x6f
    Implied, IndexedIndirect, Implied, IndexedIndirect,
    Zeropage, Zeropage, Zeropage, Zeropage,
    Implied, Immediate, Accumulator, Immediate,
    Indirect, Absolute, Absolute, Absolute,

    // 0x70-0x7f
    Relative, IndirectIndexed, Implied, IndirectIndexed,
    ZeropageX, ZeropageX, ZeropageX, ZeropageX,
    Implied, AbsoluteY, Implied, AbsoluteY,
    AbsoluteX, AbsoluteX, AbsoluteX, AbsoluteX,

    // 0x80-0x8f
    Immediate, IndexedIndirect, Immediate, IndexedIndirect,
    Zeropage, Zeropage, Zeropage, Zeropage,
    Implied, Immediate, Implied, Immediate,
    Absolute, Absolute, Absolute, Absolute,

    // 0x90-0x9f
    Relative, IndirectIndexed, Implied, IndirectIndexed,
    ZeropageX, ZeropageX, ZeropageY, ZeropageY,
    Implied, AbsoluteY, Implied, AbsoluteY,
    AbsoluteX, AbsoluteX, AbsoluteY, AbsoluteY,

    // 0xa0-0xaf
    Immediate, IndexedIndirect, Immediate, IndexedIndirect,
    Zeropage, Zeropage, Zeropage, Zeropage,
    Implied, Immediate, Implied, Immediate,
    Absolute, Absolute, Absolute, Absolute,

    // 0xb0-0xbf
    Relative, IndirectIndexed, Implied, IndirectIndexed,
    ZeropageX, ZeropageX, ZeropageY, ZeropageY,
    Implied, AbsoluteY, Implied, AbsoluteY,
    AbsoluteX, AbsoluteX, AbsoluteY, AbsoluteY,

    // 0xc0-0xcf
    Immediate, IndexedIndirect, Immediate, IndexedIndirect,
    Zeropage, Zeropage, Zeropage, Zeropage,
    Implied, Immediate, Implied, Immediate,
    Absolute, Absolute, Absolute, Absolute,

    // 0xd0-0xdf
    Relative, IndirectIndexed, Implied, IndirectIndexed,
    ZeropageX, ZeropageX, ZeropageX, ZeropageX,
    Implied, AbsoluteY, Implied, AbsoluteY,
    AbsoluteX, AbsoluteX, AbsoluteX, AbsoluteX,

    // 0xe0-0xef
    Immediate, IndexedIndirect, Immediate, IndexedIndirect,
    Zeropage, Zeropage, Zeropage, Zeropage,
    Implied, Immediate, Implied, Immediate,
    Absolute, Absolute, Absolute, Absolute,

    // 0xf0-0xff
    Relative, IndirectIndexed, Implied, IndirectIndexed,
    ZeropageX, ZeropageX, ZeropageX, ZeropageX,
    Implied, AbsoluteY, Implied, AbsoluteY,
    AbsoluteX, AbsoluteX, AbsoluteX, AbsoluteX

  )

  val instrCycle: Vector[Int] = Vector(
    // 0x00-0x0f
    7, 6, 0, 8,
    3, 3, 5, 5,
    3, 2, 2, 2,
    4, 4, 6, 6,

    // 0x10-0x1f
    2, 5, 0, 8,
    4, 4, 6, 6,
    2, 4, 2, 7,
    4, 4, 7, 7,

    // 0x20-0x2f
    6, 6, 0, 8,
    3, 3, 5, 5,
    4, 2, 2, 2,
    4, 4, 6, 6,

    // 0x30-0x3f
    2, 5, 0, 8,
    4, 4, 6, 6,
    2, 4, 2, 7,
    4, 4, 7, 7,

    // 0x40-0x4f
    6, 6, 0, 8,
    3, 3, 5, 5,
    3, 2, 2, 2,
    3, 4, 6, 6,

    // 0x50-0x5f
    2, 5, 0, 8,
    4, 4, 6, 6,
    2, 4, 2, 7,
    4, 4, 7, 7,

    // 0x60-0x6f
    6, 6, 0, 8,
    3, 3, 5, 5,
    4, 2, 2, 2,
    5, 4, 6, 6,

    // 0x70-0x7f
    2, 5, 0, 8,
    4, 4, 6, 6,
    2, 4, 2, 7,
    4, 4, 7, 7,

    // 0x80-0x8f
    2, 6, 2, 6,
    3, 3, 3, 3,
    2, 2, 2, 2,
    4, 4, 4, 4,

    // 0x90-0x9f
    2, 6, 0, 6,
    4, 4, 4, 4,
    2, 5, 2, 5,
    5, 5, 5, 5,

    // 0xa0-0xaf
    2, 6, 2, 6,
    3, 3, 3, 3,
    2, 2, 2, 2,
    4, 4, 4, 4,

    // 0xb0-0xbf
    2, 5, 0, 5,
    4, 4, 4, 4,
    2, 4, 2, 4,
    4, 4, 4, 4,

    // 0xc0-0xcf
    2, 6, 2, 8,
    3, 3, 5, 5,
    2, 2, 2, 2,
    4, 4, 6, 6,

    // 0xd0-0xdf
    2, 5, 0, 8,
    4, 4, 6, 6,
    2, 4, 2, 7,
    4, 4, 7, 7,

    // 0xe0-0xef
    2, 6, 2, 8,
    3, 3, 5, 5,
    2, 2, 2, 2,
    4, 4, 6, 6,

    // 0xf0-0xff
    2, 5, 0, 8,
    4, 4, 6, 6,
    2, 4, 2, 7,
    4, 4, 7, 7,
  )


  val instrLen: Vector[Int] = Vector(
    // 0x00-0x0f
    1, 2, 1, 2,
    2, 2, 2, 2,
    1, 2, 1, 2,
    3, 3, 3, 3,

    // 0x10-0x1f
    2, 2, 1, 2,
    2, 2, 2, 2,
    1, 3, 1, 3,
    3, 3, 3, 3,

    // 0x20-0x2f
    3, 2, 2, 2,
    2, 2, 2, 2,
    1, 2, 1, 2,
    3, 3, 3, 3,

    // 0x30-0x3f
    2, 2, 1, 2,
    2, 2, 2, 2,
    1, 3, 1, 3,
    3, 3, 3, 3,

    // 0x40-0x4f
    1, 2, 1, 2,
    2, 2, 2, 2,
    1, 2, 1, 2,
    3, 3, 3, 3,

    // 0x50-0x5f
    2, 2, 1, 2,
    2, 2, 2, 2,
    1, 3, 1, 3,
    3, 3, 3, 3,

    // 0x60-0x6f
    1, 2, 1, 2,
    2, 2, 2, 2,
    1, 2, 1, 2,
    3, 3, 3, 3,

    // 0x70-0x7f
    2, 2, 1, 2,
    2, 2, 2, 2,
    1, 3, 1, 3,
    3, 3, 3, 3,

    // 0x80-0x8f
    2, 2, 2, 2,
    2, 2, 2, 2,
    1, 2, 1, 2,
    3, 3, 3, 3,

    // 0x90-0x9f
    2, 2, 1, 2,
    2, 2, 2, 2,
    1, 3, 1, 3,
    3, 3, 3, 3,

    // 0xa0-0xaf
    2, 2, 2, 2,
    2, 2, 2, 2,
    1, 2, 1, 2,
    3, 3, 3, 3,

    // 0xb0-0xbf
    2, 2, 1, 2,
    2, 2, 2, 2,
    1, 3, 1, 3,
    3, 3, 3, 3,

    // 0xc0-0xcf
    2, 2, 2, 2,
    2, 2, 2, 2,
    1, 2, 1, 2,
    3, 3, 3, 3,

    // 0xd0-0xdf
    2, 2, 1, 2,
    2, 2, 2, 2,
    1, 3, 1, 3,
    3, 3, 3, 3,

    // 0xe0-0xef
    2, 2, 2, 2,
    2, 2, 2, 2,
    1, 2, 1, 2,
    3, 3, 3, 3,

    // 0xf0-0xff
    2, 2, 1, 2,
    2, 2, 2, 2,
    1, 3, 1, 3,
    3, 3, 3, 3,
  )
}

object Instruction {
  sealed trait Instruction
  case class BRK() extends Instruction
  case class ORA() extends Instruction
  case class KIL() extends Instruction
  case class SLO() extends Instruction
  case class NOP() extends Instruction
  case class ASL() extends Instruction
  case class PHP() extends Instruction
  case class ANC() extends Instruction
  case class BPL() extends Instruction
  case class CLC() extends Instruction
  case class JSR() extends Instruction
  case class AND() extends Instruction
  case class RLA() extends Instruction
  case class BIT() extends Instruction
  case class ROL() extends Instruction
  case class PLP() extends Instruction
  case class BMI() extends Instruction
  case class SEC() extends Instruction
  case class RTI() extends Instruction
  case class EOR() extends Instruction
  case class SRE() extends Instruction
  case class LSR() extends Instruction
  case class PHA() extends Instruction
  case class JMP() extends Instruction
  case class ASR() extends Instruction
  case class BVC() extends Instruction
  case class CLI() extends Instruction
  case class RTS() extends Instruction
  case class ADC() extends Instruction
  case class RRA() extends Instruction
  case class ROR() extends Instruction
  case class PLA() extends Instruction
  case class ARR() extends Instruction
  case class BVS() extends Instruction
  case class SEI() extends Instruction
  case class STA() extends Instruction
  case class SAX() extends Instruction
  case class STY() extends Instruction
  case class STX() extends Instruction
  case class DEY() extends Instruction
  case class TXA() extends Instruction
  case class XAA() extends Instruction
  case class BCC() extends Instruction
  case class AXA() extends Instruction
  case class TYA() extends Instruction
  case class XAS() extends Instruction
  case class TXS() extends Instruction
  case class SYA() extends Instruction
  case class SXA() extends Instruction
  case class LDY() extends Instruction
  case class LDA() extends Instruction
  case class LDX() extends Instruction
  case class LAX() extends Instruction
  case class TAY() extends Instruction
  case class TAX() extends Instruction
  case class ATX() extends Instruction
  case class BCS() extends Instruction
  case class CLV() extends Instruction
  case class TSX() extends Instruction
  case class LAR() extends Instruction
  case class CPY() extends Instruction
  case class CMP() extends Instruction
  case class DEC() extends Instruction
  case class DCP() extends Instruction
  case class INY() extends Instruction
  case class DEX() extends Instruction
  case class AXS() extends Instruction
  case class BNE() extends Instruction
  case class CLD() extends Instruction
  case class CPX() extends Instruction
  case class SBC() extends Instruction
  case class ISB() extends Instruction
  case class INC() extends Instruction
  case class INX() extends Instruction
  case class BEQ() extends Instruction
  case class SED() extends Instruction
}

object AddressingMode {
  sealed trait AddressingMode
  case object Implied extends AddressingMode
  case object Accumulator extends AddressingMode
  case object Immediate extends AddressingMode
  case object Zeropage extends AddressingMode
  case object ZeropageX extends AddressingMode
  case object ZeropageY extends AddressingMode
  case object Relative extends AddressingMode
  case object Absolute extends AddressingMode
  case object AbsoluteX extends AddressingMode
  case object AbsoluteY extends AddressingMode
  case object Indirect extends AddressingMode
  case object IndexedIndirect extends AddressingMode
  case object IndirectIndexed extends AddressingMode
}

object Cpu {
  def apply(memory: CpuMemory, openBus: OpenBus) = new Cpu(memory, openBus)
}