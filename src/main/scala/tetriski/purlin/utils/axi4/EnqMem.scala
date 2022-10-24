package tetriski.purlin.utils.axi4

import chisel3._
import chisel3.util._

/** A memory-writing process model for the EnqMem module.
 *
 * It accepts the input data from the handle `in` in the valid-ready protocol
 * and generates the control signals for on-chip memory using the `start` and `run` methods.
 */
abstract class EnqMemProcess {
  /** The input handle in the valid-ready protocol.
   */
  def in: DecoupledIO[UInt]

  /** Detect the idleness of the memory-writing process.
   */
  def idle(): Bool

  /** Start the memory-writing process.
   *
   * @param base the base address
   * @param len  the number of transfers, where #bytes == len * mem.din.getWidth/8
   * @param drop drops a few data at the beginning
   */
  def start(base: UInt, len: UInt, drop: UInt = 0.U): Unit

  /** Keep the memory-writing process alive.
   */
  def run(): Unit
}

object EnqMem {
  def apply(mem: MemWriteIOTrait): EnqMemProcess = new EnqMemProcess {
    val self = Module(new EnqMem(mem))

    self.io.en := false.B
    self.io.start := false.B
    self.io.in.noenq()
    self.io.base := 0.U
    self.io.len := 0.U
    self.io.drop := 0.U

    def in = self.io.in

    def idle() = self.io.idle

    def start(base: UInt, len: UInt, drop: UInt) {
      connect()
      self.io.en := true.B
      self.io.start := true.B
      self.io.base := base
      self.io.len := len
      self.io.drop := drop
    }

    def run() {
      connect()
      self.io.en := true.B
    }

    def connect() {
      self.io.mem.en <> mem.en
      self.io.mem.we <> mem.we
      self.io.mem.index <> mem.index
      self.io.mem.din <> mem.din
    }
  }
}

class EnqMem(mem_io: MemWriteIOTrait) extends Module {
  val mem_depth = mem_io.mem_depth
  val data_width = mem_io.din.getWidth
  val addr_width = mem_io.index.getWidth

  val io = IO(new Bundle {
    val in = Flipped(EnqIO(UInt(data_width.W)))
    val mem = new MemWriteIO(mem_depth, data_width)
    val base = Input(UInt(addr_width.W))
    val len = Input(UInt(addr_width.W))
    val drop = Input(UInt(addr_width.W))

    val en = Input(Bool())
    val start = Input(Bool())
    val idle = Output(Bool())
  })

  val s_idle :: s_wait :: s_fetch :: s_exec :: Nil = Enum(4)
  val state = RegInit(s_idle)

  val data = Reg(UInt(data_width.W))
  val index = Reg(UInt(addr_width.W))
  val len = Reg(UInt(addr_width.W)) // remained #transfers
  val drop = Reg(UInt(addr_width.W)) // remained #drops

  io.in.nodeq()
  io.mem.disable()

  io.idle := (state === s_idle)

  when(io.en) {
    when(io.idle && io.start) {
      when(io.drop === 0.U) {
        state := s_fetch
      }.otherwise {
        state := s_wait
      }
      index := io.base
      len := io.len
      drop := io.drop
      //printf("[EnqMem] start index=0x%x len=%d drop=%d\n", io.base, io.len, io.drop)
    }

    when(state === s_wait) {
      when(drop === 0.U) {
        state := s_fetch
      }.otherwise {
        data := io.in.deq()
        when(io.in.fire()) {
          drop := drop - 1.U
          //printf("[EnqMem] drop %d => %d\n", drop, drop - 1.U)
        }
      }
    }

    when(state === s_fetch) {
      //printf("[EnqMem] s_fetch index=0x%x len=%d\n", index, len)
      pullData()
    }

    when(state === s_exec) {
      //printf("[EnqMem] s_exec mem[0x%x] = 0x%x\n", index, data)
      io.mem.enable()
      io.mem.index := index
      io.mem.din := data
      index := index + 1.U
      pullData()
    }

    def pullData() {
      when(len === 0.U) {
        state := s_idle
      }.otherwise {
        data := io.in.deq()
        when(io.in.fire()) {
          state := s_exec
          len := len - 1.U
        }.otherwise {
          state := s_fetch
        }
      }
    }
  }
}
