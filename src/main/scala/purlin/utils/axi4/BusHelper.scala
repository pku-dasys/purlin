package purlin.utils.axi4

import chisel3._
import chisel3.util._
import purlin.utils.axi4.{Axi4Config, AxiMasterIO, AxiMasterReadIO, AxiMasterWriteIO}

@deprecated("use DeqBus or EnqBus instead", "")
object DecoupledBus {

  def apply(bus: AxiMasterReadIO, enq: DecoupledIO[UInt]) = new {

    val len_width = bus.read_addr.len.getWidth
    val data_width = bus.read_data.bits.getWidth

    protected val s_start :: s_read :: s_idle :: Nil = Enum(3)
    protected val state = RegInit(s_idle)

    def idle() = (state === s_idle)

    def start() {
      state := s_start
    }

    def read(base: UInt, len: UInt) {

      val counter = RegInit(0.U(len_width))
      when(state === s_start) {

        bus.read_addr.setLen(len)
        bus.read_addr.setSize(data_width / 8)
        bus.read_addr.enq(base)

        when(bus.read_addr.fire()) {

          state := s_read
          counter := len - 1.U
          assert(bus.read_addr.len === len - 1.U)
        }
      }

      when(state === s_read) {
        connect()
        when(bus.read_data.fire()) {
          counter := counter - 1.U
          when(bus.read_data.last) {
            state := s_idle
            assert(counter === 0.U)
          }
        }
      }

      when(state === s_idle) {
      }
    }

    def connect() {
      bus.read_data.ready <> enq.ready
      bus.read_data.valid <> enq.valid
      bus.read_data.bits <> enq.bits
    }
  }


  def apply(deq: DecoupledIO[UInt], bus: AxiMasterWriteIO) = new {

    val len_width = bus.write_addr.len.getWidth
    val data_width = bus.write_data.bits.getWidth

    protected val s_start :: s_write :: s_resp :: s_idle :: Nil = Enum(4)
    protected val state = RegInit(s_start)

    def idle() = (state === s_idle)

    def start() {
      state := s_start
    }

    def write(base: UInt, len: UInt) {
      val counter = RegInit(0.U(len_width.W))

      when(state === s_start) {
        bus.write_addr.setLen(len)
        bus.write_addr.setSize(data_width / 8)
        bus.write_addr.enq(base)
        when(bus.write_addr.fire()) {
          state := s_write
          counter := bus.write_addr.len
          assert(bus.write_addr.len === len - 1.U)
        }
      }

      when(state === s_write) {
        connect()
        when(bus.write_data.fire()) {
          when(counter === 0.U) {
            state := s_resp
            bus.write_data.last := true.B
          }.otherwise {
            counter := counter - 1.U
          }
        }
      }

      when(state === s_resp) {
        val resp = bus.write_resp.deq()
        when(bus.write_resp.fire()) {
          state := s_idle
          assert(counter === 0.U)
          assert(resp === Axi4Config.Resp.OKAY)
        }
      }

      when(state === idle) {
      }
    }

    def connect() {
      deq.ready <> bus.write_data.ready
      deq.valid <> bus.write_data.valid
      deq.bits <> bus.write_data.bits
      bus.write_resp.ready := true.B
    }
  }
}

