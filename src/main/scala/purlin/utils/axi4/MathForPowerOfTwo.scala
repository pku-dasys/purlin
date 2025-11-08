package purlin.utils.axi4

import chisel3._
import chisel3.util._

object MathForPowerOfTwo {
  def isPowerOfTwo(x: Int): Boolean = (x != 0 && (x & (x - 1)) == 0)

  def mask(x: Int): UInt = ~0.U(log2Ceil(x).W)

  implicit class forPowerOfTwoMultiplication(x: UInt) {
    /** Multiplication operator
     */
    def #*(y: Int): UInt = {
      assert(isPowerOfTwo(y))
      x << log2Ceil(y)
    }
  }

  implicit class forPowerOfTwoDivision(x: UInt) {
    /** Division operator
     */
    def #/(y: Int): UInt = {
      assert(isPowerOfTwo(y))
      x >> log2Ceil(y)
    }

    /** Modulo operator
     *
     * bit width reduced to log(y)
     */
    def #%(y: Int): UInt = {
      assert(isPowerOfTwo(y))
      (x & mask(y)) (log2Ceil(y) - 1, 0)
    }

    /** Compute the greatest multiple of y before x
     *
     * x #^ y === floor(x / y) * y
     **/
    def #^(y: Int): UInt = {
      assert(isPowerOfTwo(y))
      x & ~(mask(y) | 0.U(x.getWidth.W))
    }

    /** Compute the least multiple of y after x
     *
     * x #^^ y === ceil(x / y) * y
     */
    def #^^(y: Int): UInt = {
      assert(isPowerOfTwo(y))
      (x | mask(y)) + 1.U
    }
  }

}
