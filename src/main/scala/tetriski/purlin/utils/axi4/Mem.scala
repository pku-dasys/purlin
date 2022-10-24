package tetriski.purlin.utils.axi4

import chisel3._
import chisel3.util._

trait MemIOTrait extends MemWriteIOTrait with MemReadIOTrait {
  override def enable() {
    super[MemWriteIOTrait].disable()
    super[MemReadIOTrait].enable()
  }

  def wenable() {
    super[MemReadIOTrait].disable()
    super[MemWriteIOTrait].enable()
  }

  override def disable() {
    super[MemReadIOTrait].disable()
    super[MemWriteIOTrait].disable()
  }

  override def <>(that: MemIOTrait) {
    super[MemReadIOTrait].<>(that)
    super[MemWriteIOTrait].<>(that)
  }
}

trait MemReadIOTrait extends Bundle {
  val mem_depth: Int
  val mem_width: Int

  val en: Bool
  val index: UInt
  val dout: UInt

  def enable() {
    en := true.B
  }

  def disable() {
    en := false.B
    index := DontCare
  }

  def <>(that: MemIOTrait) {
    this.en <> that.en
    this.index <> that.index
    this.dout <> that.dout
  }

  def <>(that: MemReadIOTrait) {
    this.en <> that.en
    this.index <> that.index
    this.dout <> that.dout
  }
}

trait MemWriteIOTrait extends Bundle {
  val mem_depth: Int
  val mem_width: Int

  val en: Bool
  val we: Bool
  val index: UInt
  val din: UInt

  def enable() {
    en := true.B
    we := true.B
  }

  def disable() {
    en := false.B
    we := false.B
    index := DontCare
    din := DontCare
  }

  def write(addr: UInt, data: UInt) {
    this.index := addr
    this.din := data
  }

  def <>(that: MemIOTrait) {
    this.en <> that.en
    this.we <> that.we
    this.index <> that.index
    this.din <> that.din
  }

  def <>(that: MemWriteIOTrait) {
    this.en <> that.en
    this.we <> that.we
    this.index <> that.index
    this.din <> that.din
  }
}


class MemReadIO(val mem_depth: Int, val mem_width: Int) extends MemReadIOTrait {
  val en = Output(Bool())
  val index = Output(UInt(log2Ceil(mem_depth).W))
  val dout = Input(UInt(mem_width.W))

  override def cloneType: this.type = new MemReadIO(mem_depth, mem_width).asInstanceOf[this.type]
}

class MemWriteIO(val mem_depth: Int, val mem_width: Int) extends MemWriteIOTrait {
  val en = Output(Bool())
  val we = Output(Bool())
  val index = Output(UInt(log2Ceil(mem_depth).W))
  val din = Output(UInt(mem_width.W))

  override def cloneType: this.type = new MemWriteIO(mem_depth, mem_width).asInstanceOf[this.type]
}

class MemIO(val mem_depth: Int, val mem_width: Int) extends MemIOTrait {
  val en = Output(Bool())
  val we = Output(Bool())
  val index = Output(UInt(log2Ceil(mem_depth).W))
  val din = Output(UInt(mem_width.W))
  val dout = Input(UInt(mem_width.W))

  override def cloneType: this.type = new MemIO(mem_depth, mem_width).asInstanceOf[this.type]
}

// exclusive read or write
class SinglePortMem(mem_depth: Int, mem_width: Int) extends Module {
  val io = IO(
    Flipped(new MemIO(mem_depth, mem_width))
  )

  val mem = Mem(mem_depth, UInt(mem_width.W))
  val dout = Reg(UInt(io.dout.getWidth.W))

  io.dout := dout

  when(io.en) {
    when(io.we) {
      mem.write(io.index, io.din)
    }.otherwise {
      dout := mem.read(io.index) // buffered; io.dout available in next cycle
    }
  }

  //printf("[SinglePortMem] %d %d %x %x %x\n", io.en, io.we, io.addr, io.din, io.dout)
}

// write port A + read port B
class SimpleDualPortMem(mem_depth: Int, mem_width: Int) extends Module {
  val io = IO(new Bundle {
    val a = Flipped(new MemWriteIO(mem_depth, mem_width))
    val b = Flipped(new MemReadIO(mem_depth, mem_width))
  })

  val mem = Mem(mem_depth, UInt(mem_width.W))
  val dout = Reg(UInt(io.b.dout.getWidth.W))

  io.b.dout := dout

  when(io.a.en && io.a.we) {
    mem.write(io.a.index, io.a.din)
  }

  when(io.b.en) {
    dout := mem.read(io.b.index) // buffered; io.b.dout available in next cycle
  }
}

