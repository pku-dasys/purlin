package tetriski.purlin.NoC

import chisel3._
import chisel3.util._
import tetriski.purlin.utils.{FunctionType, Parameters}

/** A test data bundle.
 */
class DataBundle extends Bundle {
  val a = UInt(32.W)
  val b = UInt(32.W)
}

/** A FIFO buffer.
 *
 * @param gen  the rule of data bundle
 * @param n    the buffer depth
 * @param name the module name
 */
class FIFO[T <: Data](gen: T, n: Int, name: String, betterFrequency: Boolean = false) extends Module {
  def incHelper(input: UInt): UInt ={
    Mux(input + 1.U === n.U, 0.U, input + 1.U)
  }

  val stressWidth = if (Parameters.functionType != FunctionType.XY || Parameters.USE_VC_HBT) {
    log2Ceil(Parameters.fifoDep + 1) + 2
  } else {
    0
  }

  val io = IO(new Bundle {
    val enq = Flipped(new DecoupledIO(gen))
    val deq = new DecoupledIO(gen)

    val stressOut = Output(UInt(stressWidth.W))
  })

  override def desiredName = name

  val enqDat = io.enq.bits
  val enqVal = io.enq.valid
  val enqRdy = io.enq.ready
  val deqDat = io.deq.bits
  val deqVal = io.deq.valid
  val deqRdy = io.deq.ready


  val enqPtr = RegInit(0.asUInt(log2Ceil(n + 1).W))
  val deqPtr = RegInit(0.asUInt(log2Ceil(n + 1).W))
  val isFull = RegInit(false.B)
  var doEnq = enqRdy && enqVal
  val doDeq = deqRdy && deqVal
  val isEmpty = !isFull && (enqPtr === deqPtr)
  //  val deqPtrInc = deqPtr + 1.U
  //  val enqPtrInc = enqPtr + 1.U
  val deqPtrInc = incHelper(deqPtr)
  val enqPtrInc = incHelper(enqPtr)
  if (betterFrequency) {
    //We set "enqRdy" false when (enqPtr + 2 === deqPtr) to prevent packet lose.
    //This implementation is not very good due to a waste of one register.
    val enqRdyReal = WireInit(false.B)
    enqRdyReal := !isFull
    val isFullNextNext = incHelper(enqPtrInc) === deqPtr
    val isFullNext = Mux(doEnq && !doDeq && (enqPtrInc === deqPtr),
      true.B, Mux(doDeq && isFull, false.B,
        isFull))

//    val isFullNext = RegNext(isFullNextNext)
    doEnq = enqRdyReal && enqVal
    enqPtr := Mux(doEnq, enqPtrInc, enqPtr)
    deqPtr := Mux(doDeq, deqPtrInc, deqPtr)
    isFull := isFullNext
    enqRdy := !isFullNextNext

  } else {
    val isFullNext = Mux(doEnq && !doDeq && (enqPtrInc === deqPtr),
      true.B, Mux(doDeq && isFull, false.B,
        isFull))
    enqPtr := Mux(doEnq, enqPtrInc, enqPtr)
    deqPtr := Mux(doDeq, deqPtrInc, deqPtr)
    isFull := isFullNext
    enqRdy := !isFull
  }

  val ram = Mem(n, gen)
  when(doEnq) {
    ram(enqPtr) := enqDat
  }

  deqVal := !isEmpty
  ram(deqPtr) <> deqDat

  when(isFull) {
    io.stressOut := n.U
  }.otherwise {
    io.stressOut := enqPtr - deqPtr
  }

}

object FIFOTest extends App {
  val fifo = () => new FIFO(new DataBundle, 8, "testFIFO")
  chisel3.Driver.execute(Array("-td", "tutorial/RTL/", "--full-stacktrace"), fifo)
}