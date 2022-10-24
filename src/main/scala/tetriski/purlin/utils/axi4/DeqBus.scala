package tetriski.purlin.utils.axi4

import chisel3._
import chisel3.util._
import tetriski.purlin.utils.axi4.{Axi4Config, AxiMasterReadIO, AxiMasterReadIOTrait}
import tetriski.purlin.utils.axi4.Constants._
import tetriski.purlin.utils.axi4.MathForPowerOfTwo._

/** A bus-reading process model for the DeqBus module.
 *
 * It wraps up the bus-reading protocol (e.g., AXI4) using the `start` and `run` methods
 * and generates an output handle `out` in the valid-ready protocol.
 */
abstract class DeqBusProcess {
  /** The output handle in the valid-ready protocol.
   */
  def out: DecoupledIO[UInt]

  /** Detect the idleness of the bus-reading process.
   */
  def idle(): Bool

  /** Start the bus-reading process.
   *
   * @param base the base address
   * @param len  the number of transfers, where #bytes == len * bus.read_data.bits.getWidth / BYTE_WIDTH
   */
  def start(base: UInt, len: UInt): Unit

  /** Keep the bus-reading process alive.
   */
  def run(): Unit
}

object DeqBus {
  def apply(bus: AxiMasterReadIOTrait): DeqBusProcess = new DeqBusProcess {
    val self = Module(new DeqBus(bus))

    self.io.en := false.B
    self.io.start := false.B
    self.io.bus.sInit()
    self.io.out.nodeq()
    self.io.base := 0.U
    self.io.len := 0.U

    def out = self.io.out

    def idle() = self.io.idle

    def start(base: UInt, len: UInt) {
      connect()
      self.io.en := true.B
      self.io.start := true.B
      self.io.base := base
      self.io.len := len
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

class DeqBus(addr_width: Int, data_width: Int) extends Module {
  val data_size = data_width / BYTE_WIDTH // #bytes per data item

  def this(bus_io: AxiMasterReadIOTrait) {
    this(bus_io.read_addr.bits.getWidth, bus_io.read_data.bits.getWidth)
  }

  val io = IO(new Bundle {
    val bus = new AxiMasterReadIO(addr_width, data_width)
    val out = Flipped(DeqIO(UInt(data_width.W)))
    val base = Input(UInt(addr_width.W))
    val len = Input(UInt(addr_width.W))

    val en = Input(Bool())
    val start = Input(Bool())
    val idle = Output(Bool())
  })

  val s_fetch :: s_exec :: Nil = Enum(2)
  val state = RegInit(s_fetch)

  val addr = Reg(UInt(addr_width.W))
  val remain = RegInit(0.U(addr_width.W)) // remained #transfers that may initiate multiple bursts

  val burst_len = 1.U << Axi4Config.Len.width // maximum #transfers in a single burst
  val burst_remain = Reg(UInt((Axi4Config.Len.width + 1).W)) // remained #transfers in the current burst

  io.bus.mInit()
  io.out.noenq()

  io.idle := (state === s_fetch) && (remain === 0.U)

  when(io.en) {
    when(io.idle && io.start) {
      state := s_fetch
      addr := io.base
      remain := io.len
      //printf("[DeqBus] start 0x%x %d\n", io.base, io.len)
    }

    when(state === s_fetch) {
      when(remain > 0.U) {
        val len = remain.min(burst_len)
        io.bus.read_addr.setLen(len)
        io.bus.read_addr.setSize(data_size)
        io.bus.read_addr.enq(addr)
        when(io.bus.read_addr.fire) {
          state := s_exec
          addr := addr + (len #* data_size)
          remain := remain - len
          burst_remain := len
          //printf("[DeqBus] read_addr fire len=%d data_size=%d addr=0x%x\n", len, (data_size).U, io.bus.read_addr.bits)
        }
      }
    }

    when(state === s_exec) {
      connect()
      when(io.bus.read_data.fire) {
        //printf("[DeqBus] data fire 0x%x\n", io.bus.read_data.bits)
        val next_burst_remain = burst_remain - 1.U
        burst_remain := next_burst_remain
        when(io.bus.read_data.last) {
          state := s_fetch
          assert(next_burst_remain === 0.U)
        }
      }
    }
  }

  def connect() {
    io.bus.read_data.ready <> io.out.ready
    io.bus.read_data.valid <> io.out.valid
    io.bus.read_data.bits <> io.out.bits
  }
}
