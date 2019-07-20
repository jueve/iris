object Util {
  def isBitSet(n: Int, ofD: Int): Boolean = {
    ((ofD >>> n) & 0x1) == 1
  }

  def bitAt(n: Int, ofD: Int): Int = {
    (ofD >>> n) & 0x1
  }

  def reverseBit(d: Int): Int = {
    val b = d.toBinaryString
    val zeroComp = "0" * (8 - b.length)
    BigInt((zeroComp + b).reverse, 2).toInt
  }

  implicit class WrapAroundNumber(self: Int) {
    def wrapAround8: Int = self & 0xff
    def wrapAround16: Int = self & 0xffff
  }
}
