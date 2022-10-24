package tetriski.purlin.utils.axi4

import chisel3._
import chisel3.util._
import tetriski.purlin.utils.axi4.Constants._

/** A memory-reading process model for the DeqMem module.
 *
 * It fetches data from on-chip memory and generate an outpu handle `out`
 */
abstract class DeqMemProcess {
  /** The output handle in the valid-ready protocol.
   */
  def out: DecoupledIO[UInt]

  /** Detect the idleness of the memory-reading process.
   */
  def idle(): Bool

  /** Start the memory-reading process.
   *
   * @param base    the base address
   * @param len     the number of transfers, where #bytes == len * mem.din.getWidth/8
   * @param prepend prepends a few dummy data at the beginning
   */
  def start(base: UInt, len: UInt, prepend: UInt = 0.U): Unit

  /** Keep the memory-writing process alive.
   */
  def run(): Unit
}

object DeqMem {
  def apply(mem: MemReadIOTrait): DeqMemProcess = new DeqMemProcess {
    val self = Module(new DeqMem(mem))

    self.io.out.nodeq()
    self.io.mem.dout := DontCare
    self.io.en := false.B
    self.io.start := false.B
    self.io.base := 0.U
    self.io.len := 0.U
    self.io.prepend := 0.U

    def out = self.io.out

    def idle() = self.io.idle

    def start(base: UInt, len: UInt, prepend: UInt) {
      connect()
      self.io.en := true.B
      self.io.start := true.B
      self.io.base := base
      self.io.len := len
      self.io.prepend := prepend
    }

    def run() {
      connect()
      self.io.en := true.B
    }

    def connect() {
      self.io.mem.en <> mem.en
      self.io.mem.index <> mem.index
      self.io.mem.dout <> mem.dout
    }
  }
}

object EnqAddrDeqMem {
  def apply(enq_io: => DecoupledIO[UInt], mem_io: => MemReadIOTrait, deq_io: => DecoupledIO[UInt]) = new {
    val self = Module(new EnqAddrDeqMem(mem_io))

    self.io.iaddr.noenq()
    self.io.mem.dout := DontCare
    self.io.odata.nodeq()

    def idle() = self.io.idle

    def run() {
      connect()
    }

    def connect() {
      self.io.iaddr <> enq_io
      self.io.mem.en <> mem_io.en
      self.io.mem.index <> mem_io.index
      self.io.mem.dout <> mem_io.dout
      self.io.odata <> deq_io
    }
  }
}

class DeqMem(mem_io: MemReadIOTrait) extends Module {
  val mem_depth = mem_io.mem_depth
  val data_width = mem_io.dout.getWidth
  val addr_width = mem_io.index.getWidth

  val io = IO(new Bundle {
    val out = Flipped(DeqIO(UInt(data_width.W)))
    val mem = new MemReadIO(mem_depth, data_width)
    val base = Input(UInt(addr_width.W))
    val len = Input(UInt(addr_width.W))
    val prepend = Input(UInt(addr_width.W))

    val en = Input(Bool())
    val start = Input(Bool())
    val idle = Output(Bool())
  })

  val s_idle :: s_wait :: s_fetch :: s_exec :: Nil = Enum(4)
  val state = RegInit(s_idle)

  val data = Reg(UInt(data_width.W))
  val index = Reg(UInt(addr_width.W))
  val len = Reg(UInt(addr_width.W))
  val prepend = Reg(UInt(addr_width.W))

  val iaddr_hs = Handshake(UInt(addr_width.W))

  def iaddr: DecoupledIO[UInt] = iaddr_hs.io.enq

  val odata_hs = Handshake(UInt(data_width.W))

  def odata: DecoupledIO[UInt] = odata_hs.io.deq

  val imo = EnqAddrDeqMem(iaddr_hs.io.deq, io.mem, odata_hs.io.enq)

  io.mem.disable()
  io.out.noenq()

  io.idle := (state === s_idle)

  when(io.en) {
    imo.run()

    when(io.idle && io.start) {
      when(io.prepend === 0.U) {
        state := s_fetch
      }.otherwise {
        state := s_wait
      }
      index := io.base
      len := io.len
      prepend := io.prepend
      //printf("[DeqMem] start base=0x%x len=%d\n", io.base, io.len)
    }

    when(state === s_wait) {
      when(prepend === 0.U) {
        state := s_fetch
      }.otherwise {
        io.out.bits := BigInt("cc" * (io.out.bits.getWidth / BYTE_WIDTH), 16).U
        io.out.valid := true.B
        when(io.out.fire) {
          //printf("[DeqMem] prepend 0x%x\n", io.out.bits)
          prepend := prepend - 1.U
        }
      }
    }

    when(state === s_fetch) {
      fetch()
    }

    when(state === s_exec) {
      io.out.enq(data)
      when(io.out.fire) {
        //printf("[DeqMem] data 0x%x\n", io.out.bits)
        fetch()
      }
    }
  }

  def fetch() {
    // enq addr
    when(len > 0.U) {
      iaddr.enq(index)
      when(iaddr.fire) {
        index := index + 1.U
        len := len - 1.U
      }
    }
    // deq data
    data := odata.deq()
    when(odata.fire) {
      state := s_exec
    }.elsewhen(imo.idle() === false.B) {
      state := s_fetch
    }.otherwise {
      state := s_idle
    }
  }
}

class EnqAddrDeqMem(mem_io: MemReadIOTrait) extends Module {
  val io = IO(new Bundle {
    val iaddr = Flipped(EnqIO(UInt(mem_io.index.getWidth.W)))
    val mem = new MemReadIO(mem_io.mem_depth, mem_io.mem_width)
    val odata = Flipped(DeqIO(UInt(mem_io.dout.getWidth.W)))
    val idle = Output(Bool())
  })

  val token = RegInit(false.B)
  val next_token = Wire(Bool())

  io.iaddr.nodeq()
  io.mem.disable()
  io.odata.noenq()

  io.idle := (token === false.B) && (io.iaddr.valid === false.B)

  next_token := token

  when(token) {
    io.odata.enq(io.mem.dout)
    when(io.odata.fire()) {
      token := false.B
      next_token := false.B
    }
  }

  when(next_token === false.B) {
    val addr = io.iaddr.deq()
    when(io.iaddr.fire()) {
      token := true.B
      io.mem.enable()
      io.mem.index := addr
    }
  }
}

