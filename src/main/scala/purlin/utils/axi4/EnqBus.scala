package purlin.utils.axi4

import chisel3._
import chisel3.util._
import purlin.utils.axi4.{Axi4Config, AxiMasterWriteIOTrait, AxiMasterWriteIO}
import purlin.utils.axi4.Constants._
import purlin.utils.axi4.MathForPowerOfTwo._

/** A bus-writing process model for the EnqBus module.
 *
 * It wraps up the bus-reading protocol (e.g., AXI4) using the `start` and `run` methods
 * and generates an output handle `out` in the valid-ready protocol.
 *
 * For unaligned writes, assume the `base` has x bytes after an aligned address, the first x bytes
 * in the `in` handle will be dropped, and the strobe for the last transfer should be given.
 */
abstract class EnqBusProcess {
  /** The input handle in the valid-ready protocol.
   */
  def in: DecoupledIO[UInt]

  /** Detect the idleness of the bus-writing process.
   */
  def idle(): Bool

  /** Start the bus-writing process.
   *
   * @param base the base address
   * @param size the bytes to write, where (#bytes == #transfers * bus_size) for aligned writes
   */
  def start(base: UInt, size: UInt): Unit

  /** Keep the bus-writing process alive.
   */
  def run(): Unit
}

object EnqBus {
  def apply(bus: AxiMasterWriteIOTrait): EnqBusProcess = new EnqBusProcess {
    val self = Module(new EnqBus(bus))

    self.io.en := false.B
    self.io.start := false.B
    self.io.in.noenq()
    self.io.bus.sInit()
    self.io.base := 0.U
    self.io.size := 0.U

    def in = self.io.in

    def idle() = self.io.idle

    def start(base: UInt, size: UInt) {
      connect()
      self.io.en := true.B
      self.io.start := true.B
      self.io.base := base
      self.io.size := size
      //printf("[EnqBus] base=%x size=%d\n", base, size)
    }

    def run() {
      connect()
      self.io.en := true.B
    }

    def connect() {
      self.io.bus <> bus
    }
  }
}

class EnqBus(addr_width: Int, data_width: Int) extends Module {
  val data_size = data_width / BYTE_WIDTH

  def this(bus_io: AxiMasterWriteIOTrait) {
    this(bus_io.write_addr.bits.getWidth, bus_io.write_data.bits.getWidth)
  }

  val io = IO(new Bundle {
    val in = Flipped(EnqIO(UInt(data_width.W)))
    val bus = new AxiMasterWriteIO(addr_width, data_width)
    val base = Input(UInt(addr_width.W))
    val size = Input(UInt(addr_width.W))

    val en = Input(Bool())
    val start = Input(Bool())
    val idle = Output(Bool())
  })

  val s_fetch :: s_exec :: s_wait :: s_idle :: Nil = Enum(4)
  val state = RegInit(s_idle)

  val addr = Reg(UInt(addr_width.W))
  val len_count = Reg(UInt(addr_width.W))
  val is_first = Reg(Bool())
  val first_strobe = Reg(UInt(data_size.W))
  val last_strobe = Reg(UInt(data_size.W))

  val burst_max = 1.U << Axi4Config.Len.width
  val burst_count = Reg(UInt((Axi4Config.Len.width + 1).W))

  io.in.nodeq()
  io.bus.mInit()

  io.idle := (state === s_idle)

  when(io.en) {
    when(io.idle && io.start) {
      state := s_fetch
      addr := io.base

      // if (data_size == 1)
      len_count := io.size
      val default_strobe = ~0.U(data_size.W)
      is_first := true.B
      first_strobe := default_strobe
      last_strobe := default_strobe
      // else
      if (data_size > 1) {
        val prepended_size = io.base #% data_size
        val total_size = prepended_size + io.size
        val last_partial_size = total_size #% data_size
        val has_last_partial = (last_partial_size =/= 0.U)

        len_count := (total_size #/ data_size) + has_last_partial
        first_strobe := default_strobe << prepended_size
        when(has_last_partial) {
          last_strobe := ~(default_strobe << last_partial_size)
        }
      }
    }

    when(state === s_fetch) {
      when(len_count > 0.U) {
        val len = len_count.min(burst_max)
        io.bus.write_addr.setLen(len)
        io.bus.write_addr.setSize(data_size)
        io.bus.write_addr.enq(addr)
        when(io.bus.write_addr.fire) {
          //printf("[EnqBus] addr 0x%x fire curr_len=%d tot_len=%d\n", io.bus.write_addr.bits, len, len_count)
          state := s_exec
          addr := addr + (len #* data_size)
          len_count := len_count - len
          burst_count := len
        }
      }.otherwise {
        state := s_idle
      }
    }

    when(state === s_exec) {
      connect()
      val next_burst_count = burst_count - 1.U
      val is_last = WireDefault(false.B)
      when(next_burst_count === 0.U) {
        io.bus.write_data.last := true.B
        when(len_count === 0.U) {
          is_last := true.B
        }
      }
      when(is_first && is_last) {
        io.bus.write_data.strb := first_strobe & last_strobe
      }.elsewhen(is_first) {
        io.bus.write_data.strb := first_strobe
      }.elsewhen(is_last) {
        io.bus.write_data.strb := last_strobe
      }
      when(io.bus.write_data.fire) {
        //printf("[EnqBus] strobe 0x%x\n", io.bus.write_data.strb)
        //printf("[EnqBus] data fire 0x%x\n", io.bus.write_data.bits)
        is_first := false.B
        burst_count := next_burst_count
        when(next_burst_count === 0.U) {
          state := s_wait
        }
      }
    }

    when(state === s_wait) {
      val resp = io.bus.write_resp.deq()
      when(io.bus.write_resp.fire) {
        //printf("[EnqBus] write_resp fire len_count=%d\n", len_count)
        when(len_count === 0.U) {
          state := s_idle
        }.otherwise {
          state := s_fetch
        }
        assert(burst_count === 0.U)
        assert(resp === Axi4Config.Resp.OKAY)
      }
    }
  }

  def connect() {
    io.in.valid <> io.bus.write_data.valid
    io.in.ready <> io.bus.write_data.ready
    io.in.bits <> io.bus.write_data.bits
  }
}
