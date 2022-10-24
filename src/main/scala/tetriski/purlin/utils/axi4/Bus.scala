package tetriski.purlin.utils.axi4

import chisel3._
import chisel3.util._
import tetriski.purlin.utils.axi4.{Axi4Config, AxiLiteSlaveIO, AxiMasterIO, AxiMasterReadIO, AxiMasterWriteIO, AxiMasterReadIOTrait}
import tetriski.purlin.utils.axi4.Constants._
import tetriski.purlin.utils.axi4.MathForPowerOfTwo._

class WriteRequest[T <: Data](addr_gen: T, data_gen: T) extends Bundle {
  val addr = addr_gen
  val data = data_gen
  require(data.getWidth % BYTE_WIDTH == 0)
  val strb = UInt((data.getWidth / BYTE_WIDTH).W)

  override def cloneType: this.type = new WriteRequest(addr_gen, data_gen).asInstanceOf[this.type]
}

class ReadArbiter(n: Int, bus: AxiMasterReadIOTrait) extends Module {
  val io = IO(new Bundle {
    val server = new AxiMasterReadIO(bus.addr_width, bus.data_width)
    val client = Vec(n, Flipped(new AxiMasterReadIO(bus.addr_width, bus.data_width)))
  })
  io.server.mInit()
  io.client.foreach(_.sInit())

  val s_idle = io.client.length.U
  val state = RegInit(s_idle)

  when(state === s_idle) {
    nextState()
  }

  for (i <- 0 until io.client.length) {
    when(state === i.U) {
      io.client(i) <> io.server
      when(io.server.read_data.last && io.server.read_data.fire) {
        nextState()
      }
    }
  }

  def nextState() {
    val onehot = PriorityEncoderOH(Cat((0 until io.client.length).map(i => io.client(i).read_addr.valid).reverse))
    when(onehot === 0.U) {
      state := s_idle
    }.otherwise {
      state := OHToUInt(onehot)
    }
  }
}

object BusReader {
  def apply(bus_io: AxiMasterReadIOTrait, mem_io: MemWriteIOTrait) = new {
    val bus_width = bus_io.read_data.bits.getWidth
    val mem_width = mem_io.din.getWidth

    val enq_mem = EnqMem(mem_io)
    val channel = Channel(mem_width, bus_width)
    val deq_bus = DeqBus(bus_io)

    def idle() = deq_bus.idle && enq_mem.idle // may drop some bytes from the bus read

    /** Start the bus to mem data movement
     *
     * @param dst_mem_base the base address on mem
     * @param src_bus_base the base address on bus
     * @param size         the number of bytes to transfer
     */
    def start(dst_mem_base: UInt, src_bus_base: UInt, size: UInt) {
      val bus_size = bus_width / BYTE_WIDTH
      val mem_size = mem_width / BYTE_WIDTH

      val drop = (src_bus_base #% bus_size) #/ mem_size
      val extra = !(size #% bus_size === 0.U && drop === 0.U)

      val mem_len = size #/ mem_size
      val bus_len = (size #/ bus_size) + extra

      connect()
      enq_mem.start(dst_mem_base, mem_len, drop)
      deq_bus.start(src_bus_base, bus_len)
    }

    def run(): Unit = {
      connect()
      enq_mem.run()
      deq_bus.run()
      when(idle) {
        channel.reset()
      }
    }

    def connect(): Unit = {
      enq_mem.in <> channel.out
      channel.in <> deq_bus.out
    }
  }
}

/*
object BusWriter {
  def apply(bus_io: AxiMasterWriteIO, mem_io: MemReadIO) = new {

    val mem = mem_io
    val bus = bus_io

    val mem_width = mem.dout.getWidth
    val bus_width = bus.write_data.data.getWidth

    protected val pipe = Module(new Queue(UInt(bus_width.W), 2))
    protected val deq_mem = DeqMem(mem, bus_width)
    protected val pipe_to_bus = DecoupledBus(pipe.io.deq, bus)
    pipe.io.enq <> deq_mem.out
    pipe.io.deq.nodeq()

    val s_fetch :: s_exec :: s_idle :: Nil = Enum(3)
    val state = RegInit(s_idle)

    def start() {
      state := s_fetch
    }

    def idle() = (state === s_idle)

    def memcpy(bus_base: UInt, mem_base: UInt, bytes: UInt) {

      when (state === s_fetch) {
        state := s_exec
        val read_len = 8.U * bytes / mem_width.U
        deq_mem.start(mem_base, read_len)
      }

      when (state === s_exec) {
        deq_mem.run()
        val write_len = 8.U * bytes / bus_width.U
        pipe_to_bus.write(bus_base, write_len)
        when (deq_mem.idle() && pipe_to_bus.idle()) {
          state := s_idle
        }
      }
    }
  }
}
*/

object BusLiteWriteSlave {
  def apply(bus_io: AxiLiteSlaveIO, buffer_depth: Int) = new {
    val addr_width = bus_io.write_addr.addr.getWidth
    val data_width = bus_io.write_data.data.getWidth
    val strb_width = bus_io.write_data.strb.getWidth

    protected val request_gen = new WriteRequest(UInt(addr_width.W), UInt(data_width.W))
    protected val obuf = Module(new Queue(UInt(request_gen.getWidth.W), buffer_depth))

    protected val s_fetch :: s_load :: s_resp :: s_exec :: Nil = Enum(4)
    protected val state = RegInit(s_fetch)
    protected val addr = RegInit(0.U(addr_width.W))
    protected val data = RegInit(0.U(data_width.W))
    protected val strb = RegInit(1.U(strb_width.W))

    obuf.io.enq.noenq()
    obuf.io.deq.nodeq()

    def out = obuf.io.deq

    def idle() = (state === s_fetch && !bus_io.write_addr.valid)

    def run() {
      when(state === s_fetch) {
        pullAddr()
      }

      when(state === s_load) {
        data := bus_io.write_data.deq()
        when(bus_io.write_data.fire) {
          strb := bus_io.write_data.strb
          state := s_resp
        }
      }

      when(state === s_resp) {
        bus_io.write_resp.enq(Axi4Config.Resp.OKAY)
        when(bus_io.write_resp.fire) {
          state := s_exec
          pushRequest()
        }
      }

      when(state === s_exec) {
        pushRequest()
      }
    }

    protected def pullAddr() {
      addr := bus_io.write_addr.deq()
      when(bus_io.write_addr.fire) {
        state := s_load
        addr := bus_io.write_addr.bits
      }.otherwise {
        state := s_fetch
      }
    }

    protected def pushRequest() {
      obuf.io.enq.enq(Cat(addr, data, strb))
      when(obuf.io.enq.fire) {
        state := s_fetch
        pullAddr()
      }.otherwise {
        state := s_exec
      }
    }
  }
}

object BusLiteReadSlave {
  def apply(bus_io: AxiLiteSlaveIO, buffer_depth: Int) = new {
    val addr_width = bus_io.write_addr.addr.getWidth
    val data_width = bus_io.write_data.data.getWidth

    protected val obuf = Module(new Queue(UInt(addr_width.W), buffer_depth))
    protected val ibuf = Module(new Queue(UInt(data_width.W), buffer_depth))

    obuf.io.enq.noenq()
    obuf.io.deq.nodeq()
    ibuf.io.enq.noenq()
    ibuf.io.deq.nodeq()

    def addr = obuf.io.deq

    def data = ibuf.io.enq

    def idle() = (!bus_io.read_addr.valid && !bus_io.read_data.valid)

    def run() {
      //
      // (addr)          (data)
      //   +---> caller ----+
      //   |                V
      // obuf <- bus_io <- ibuf
      //

      bus_io.read_addr.valid <> obuf.io.enq.valid
      bus_io.read_addr.ready <> obuf.io.enq.ready
      bus_io.read_addr.bits <> obuf.io.enq.bits

      ibuf.io.deq.valid <> bus_io.read_data.valid
      ibuf.io.deq.ready <> bus_io.read_data.ready
      ibuf.io.deq.bits <> bus_io.read_data.bits
    }
  }
}
