package tetriski.purlin.NoC

import chisel3._
import chisel3.util._

class DataBundle extends Bundle {
  val a = UInt(32.W)
  val b = UInt(32.W)
}

class FIFO[T <: Data](gen: T, n: Int, name: String, betterFrequency: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(new DecoupledIO(gen))
    val deq = new DecoupledIO(gen)
  })

  override def desiredName = name

  val enqDat = io.enq.bits
  val enqVal = io.enq.valid
  val enqRdy = io.enq.ready
  val deqDat = io.deq.bits
  val deqVal = io.deq.valid
  val deqRdy = io.deq.ready


  val enqPtr = RegInit(0.asUInt(log2Ceil(n).W))
  val deqPtr = RegInit(0.asUInt(log2Ceil(n).W))
  val isFull = RegInit(false.B)
  val doEnq = enqRdy && enqVal
  val doDeq = deqRdy && deqVal
  val isEmpty = !isFull && (enqPtr === deqPtr)
  val deqPtrInc = deqPtr + 1.U
  val enqPtrInc = enqPtr + 1.U
  if(betterFrequency){
    val isFullNextNext = Mux(doEnq && !doDeq && ((enqPtrInc + 1.U) === deqPtr),
      true.B, Mux(doDeq && isFull, false.B,
        isFull))
    val isFullNext = RegNext(isFullNextNext)
    enqPtr := Mux(doEnq, enqPtrInc, enqPtr)
    deqPtr := Mux(doDeq, deqPtrInc, deqPtr)
    isFull := isFullNext
  }else{
    val isFullNext = Mux(doEnq && !doDeq && (enqPtrInc === deqPtr),
      true.B, Mux(doDeq && isFull, false.B,
        isFull))
    enqPtr := Mux(doEnq, enqPtrInc, enqPtr)
    deqPtr := Mux(doDeq, deqPtrInc, deqPtr)
    isFull := isFullNext
  }

  val ram = Mem(n, gen)
  when(doEnq) {
    ram(enqPtr) := enqDat
  }
  enqRdy := !isFull
  deqVal := !isEmpty
  ram(deqPtr) <> deqDat
}

object FIFOTest extends App {
  val fifo = () => new FIFO(new DataBundle, 8, "testFIFO")
  chisel3.Driver.execute(Array("-td", "tutorial/RTL/", "--full-stacktrace"), fifo)
}