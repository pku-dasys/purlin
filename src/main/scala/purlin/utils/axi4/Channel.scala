package purlin.utils.axi4

import chisel3._
import chisel3.util._

object Handshake {
  def apply[T <: Data](gen: T): Handshake[T] = {
    val self = Module(new Handshake(gen))
    val io = self.io
    io.enq.noenq()
    io.deq.nodeq()
    return self
  }
}

class Handshake[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(EnqIO(gen))
    val deq = Flipped(DeqIO(gen))
  })
  io.enq <> io.deq
}

object Channel {
  def apply(out_width: Int, in_width: Int) = new {
    val self = Module(new Channel(out_width, in_width))

    self.io.in.noenq()
    self.io.out.nodeq()

    def in = self.io.in

    def out = self.io.out

    def reset() {
      self.reset := true.B
    }

    def run() {}
  }
}

class Channel(out_width: Int, in_width: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(EnqIO(UInt(in_width.W)))
    val out = Flipped(DeqIO(UInt(out_width.W)))
  })

  val s_fetch :: s_exec :: Nil = Enum(2)
  val state = RegInit(s_fetch)

  val manip = SplitOrConcat(out_width, in_width)

  val data_in = Reg(Vec(manip.factor, UInt(out_width.W))) // to split
  val data_out = Reg(Vec(manip.factor, UInt(in_width.W))) // to concat
  val index = RegInit(0.U(log2Ceil(manip.factor + 1).W))

  io.in.nodeq()
  io.out.noenq()

  manip.mode match {
    case SplitOrConcat.Normal =>
      io.out <> io.in

    case SplitOrConcat.Split =>
      when(state === s_fetch) {
        val data = io.in.deq().asTypeOf(data_in)
        when(io.in.fire) {
          state := s_exec
          data_in := data
          assert(index === 0.U)
          pushData(data(index))
        }
      }

      when(state === s_exec) {
        pushData(data_in(index))
      }

      def pushData(data: UInt) {
        io.out.enq(data)
        when(io.out.fire) {
          val next_index = index + 1.U
          index := next_index
          when(next_index === manip.factor.U) {
            state := s_fetch
            index := 0.U
          }
        }
      }

    case SplitOrConcat.Concat =>
      when(state === s_fetch) {
        pullData()
      }

      when(state === s_exec) {
        io.out.enq(data_out.asUInt)
        when(io.out.fire) {
          state := s_fetch
          assert(index === 0.U)
          pullData()
        }
      }

      def pullData() {
        val data = io.in.deq()
        when(io.in.fire) {
          data_out(index) := data
          val next_index = index + 1.U
          index := next_index
          when(next_index === manip.factor.U) {
            state := s_exec
            index := 0.U
          }
        }
      }
  }
}

object SplitOrConcat extends Enumeration {
  type Mode = Value
  val Normal, Split, Concat = Value

  def apply(dest_width: Int, src_width: Int) = new {

    val mode = math.min(1, math.max(-1, src_width - dest_width)) match {
      case 1 => Split
      case 0 => Normal
      case -1 => Concat
    }
    val factor = mode match {
      case Split =>
        val fac = src_width / dest_width
        assert(fac * dest_width == src_width, s"$fac * $dest_width != $src_width")
        fac
      case Concat =>
        val fac = dest_width / src_width
        assert(fac * src_width == dest_width, s"$fac * $src_width != $dest_width")
        fac
      case Normal =>
        assert(src_width == dest_width, s"$src_width != $dest_width")
        1
    }
  }
}
