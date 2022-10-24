package tetriski.purlin.utils.axi4

import chisel3._
import chisel3.util._
import tetriski.purlin.utils.axi4.{Axi4Config, AxiMasterIO}
import tetriski.purlin.utils.axi4.Constants._
import tetriski.purlin.utils.axi4.MathForPowerOfTwo._

class DramSimulator(ram_io: MemIO, ports: Seq[AxiMasterIO]) extends Module {
  def this(port: AxiMasterIO) { // single bus
    this(new MemIO(math.pow(2, port.addr_width).toInt, port.data_width), Seq(port))
  }

  val io = IO(new Bundle {
    val ram = Flipped(new MemIO(ram_io.mem_depth, ram_io.mem_width))
    val buses = MixedVec(ports.map(p => Flipped(new AxiMasterIO(p.addr_width, p.data_width))))

    def bus = buses(0) // shortcut for single bus
  })

  val mem_width = BYTE_WIDTH
  val ram_factor = io.ram.mem_width / mem_width
  val mem_depth = io.ram.mem_depth * ram_factor
  val mem = Mem(mem_depth, UInt(mem_width.W))

  io.ram.dout := DontCare

  io.buses.foreach(bus => bus.sInit())

  when(io.ram.en) {
    when(io.ram.we) {
      val data = Wire(Vec(ram_factor, UInt(mem_width.W)))
      data := io.ram.din.asTypeOf(data)
      for (i <- 0 until ram_factor) {
        mem((io.ram.index << Log2(ram_factor.U)) | i.U) := data(i)
      }
    }.otherwise {
      io.ram.dout := Cat((0 until ram_factor).reverse.map(i =>
        mem((io.ram.index << Log2(ram_factor.U)) | i.U)))
    }
  }.otherwise {
    io.buses.foreach(bus => {
      send(bus)
      recv(bus)
    })
  }

  def send(bus: AxiMasterIO) {
    assert(bus.data_width % mem_width == 0)
    val factor = bus.data_width / mem_width

    val s_fetch :: s_exec :: Nil = Enum(2)
    val state = RegInit(s_fetch)

    val addr = Reg(UInt(bus.addr_width.W)) // byte address
    val remain = Reg(UInt(bus.addr_width.W)) // remained #transfers

    when(state === s_fetch) {
      val unaligned_addr = bus.read_addr.deq()
      when(bus.read_addr.fire) {
        //printf("[DramSim] addr=0x%x fire=%d\n", bus.read_addr.bits, bus.read_addr.fire)
        state := s_exec
        if (bus.data_width == BYTE_WIDTH) {
          addr := unaligned_addr
        } else { // enforced address alignment
          val mask = ~0.U(bus.addr_width.W) << Log2((bus.data_width / BYTE_WIDTH).U)
          addr := unaligned_addr & mask
          //printf("[DramSim] addr 0x%x := 0x%x & 0x%x\n", (unaligned_addr & mask), unaligned_addr, mask)
        }
        remain := bus.read_addr.getLen()
        val size = bus.read_addr.getSize()
        assert(size * BYTE_WIDTH.U === bus.data_width.U)
        val burst = bus.read_addr.burst
        assert(burst === Axi4Config.Burst.INCR)
      }
    }

    when(state === s_exec) {
      bus.read_data.last := (remain === 1.U)
      val index = addr / (mem_width / BYTE_WIDTH).U
      bus.read_data.enq(
        Cat((0 until factor).reverse.map(i => mem(index + i.U)))
      )
      when(bus.read_data.fire) {
        //printf("[DramSim] data=0x%x\n", bus.read_data.bits)
        addr := addr + (bus.data_width / BYTE_WIDTH).U
        remain := remain - 1.U
        when(bus.read_data.last.asBool) {
          state := s_fetch
        }
      }
    }
  }

  def recv(bus: AxiMasterIO) {
    assert(bus.data_width % mem_width == 0)
    val factor = bus.data_width / mem_width
    val bus_size = bus.data_width / BYTE_WIDTH

    val s_fetch :: s_exec :: s_notify :: Nil = Enum(3)
    val state = RegInit(s_fetch)

    val addr = Reg(UInt(bus.addr_width.W)) // byte address
    val len_count = Reg(UInt(bus.addr_width.W)) // remaining #transfers

    val strobe = Reg(UInt(bus_size.W))
    val default_strobe = ~0.U(bus_size.W)

    when(state === s_fetch) {
      val unaligned_addr = bus.write_addr.deq()
      when(bus.write_addr.fire) {
        state := s_exec
        if (bus.data_width == BYTE_WIDTH) {
          addr := unaligned_addr
        } else { // enforced address alignment
          addr := unaligned_addr #^ bus_size
          val offset = unaligned_addr #% bus_size
          strobe := default_strobe << offset
        }
        len_count := bus.write_addr.getLen()
        val size = bus.write_addr.getSize()
        assert(size * BYTE_WIDTH.U === bus.data_width.U)
        val burst = bus.read_addr.burst
        assert(burst === Axi4Config.Burst.INCR)
      }
    }

    when(state === s_exec) {
      val mem_size = mem_width / BYTE_WIDTH
      val data = bus.write_data.deq()
      when(bus.write_data.fire) {
        val index = addr >> log2Ceil(mem_size).U
        val strb = strobe & bus.write_data.strb
        strobe := default_strobe
        (0 until factor).foreach(i => {
          val piece = data((i + 1) * mem_width - 1, i * mem_width)
          val sub_strb = strb((i + 1) * mem_size - 1, i * mem_size)
          val mem_word = Wire(Vec(mem_size, UInt(BYTE_WIDTH.W)))
          val bus_word = Wire(Vec(mem_size, UInt(BYTE_WIDTH.W)))
          mem_word := mem(index + i.U).asTypeOf(mem_word)
          bus_word := piece.asTypeOf(bus_word)
          (0 until mem_size).foreach(j => {
            when(sub_strb(j).asBool) {
              mem_word(j) := bus_word(j)
            }
          })

          mem(index + i.U) := mem_word.asUInt
        })
        addr := addr + (bus.data_width / BYTE_WIDTH).U
        len_count := len_count - 1.U
        when(bus.write_data.last.asBool) {
          state := s_notify
        }
      }
    }

    when(state === s_notify) {
      bus.write_resp.enq(Axi4Config.Resp.OKAY)
      when(bus.write_resp.fire) {
        state := s_fetch
        assert(len_count === 0.U)
      }
    }
  }
}
