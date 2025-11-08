package purlin.utils.axi4

/** Provides classes for dealing with AXI4 protocol.
 *
 * See ARM IHI 0022F.b (ID122117)
 * AMBA® AXI and ACE Protocol Specification (AXI3, AXI4, AXI5, ACE and ACE5)
 * at https://developer.arm.com/docs/ihi0022/fb
 *
 * Some encodings are implemented in Axi4Config.
 *
 * When handling unaligned read/write in AXI4,
 * see the following example:
 * 4-byte burst size, data represented using
 * hex numbers, little endian, and "-" for don't-care.
 *
 * 0x4      0x7    0x8      0xb    0xc      0xf
 * initial     |  cc cc cc cc  |  cc cc cc cc  |  cc cc cc cc  |
 * target      |  cc cc cc 0d  |  10 12 14 18  |  1a 1c cc cc  |
 *
 * To write the target content in memory,
 * we need three write transfers in a burst:
 * data 0x0d------ for the 1st transfer starting at address 0x7;
 * data 0x18141210 for the 2nd transfer in the burst;
 * data 0x----1c1a for the last transfer with strobe 0x0011.
 *
 * To read the target content from memory,
 * we need three read transfers in a burst:
 * data 0x0d------ from the 1st transfer starting at address 0x7
 * (or data 0x0dcc----, 0x0dcccc--, 0x0dcccccc for addresses 0x6, 0x5, 0x4, respectively);
 * data 0x18141210 from the 2nd transfer in the burst;
 * data 0xcccc1c1a from the last transfer (and drop the leading two cc's yourself).
 */

import chisel3._
import chisel3.util._

class AxiMasterIO(val addr_width: Int, val data_width: Int) extends AxiMasterIOTrait {
  val read_addr = new AxiAddressIO(UInt(addr_width.W))
  val read_data = Flipped(new AxiReadDataIO(UInt(data_width.W)))
  val write_addr = new AxiAddressIO(UInt(addr_width.W))
  val write_data = new AxiWriteDataIO(UInt(data_width.W))
  val write_resp = Flipped(new AxiWriteResponseIO(UInt(Axi4Config.Resp.width.W)))
}

class AxiMasterReadIO(val addr_width: Int, val data_width: Int) extends AxiMasterReadIOTrait {
  val read_addr = new AxiAddressIO(UInt(addr_width.W))
  val read_data = Flipped(new AxiReadDataIO(UInt(data_width.W)))

  def this(parent: AxiMasterIO) {
    this(parent.addr_width, parent.data_width)
  }

  override def cloneType: this.type = new AxiMasterReadIO(addr_width, data_width).asInstanceOf[this.type]
}

class AxiMasterWriteIO(val addr_width: Int, val data_width: Int) extends AxiMasterWriteIOTrait {
  val write_addr = new AxiAddressIO(UInt(addr_width.W))
  val write_data = new AxiWriteDataIO(UInt(data_width.W))
  val write_resp = Flipped(new AxiWriteResponseIO(UInt(Axi4Config.Resp.width.W)))

  def this(parent: AxiMasterIO) {
    this(parent.addr_width, parent.data_width)
  }
}

trait AxiMasterIOTrait extends Bundle with AxiMasterWriteIOTrait with AxiMasterReadIOTrait {
  override def mInit() {
    super[AxiMasterReadIOTrait].mInit()
    super[AxiMasterWriteIOTrait].mInit()
  }

  override def sInit() {
    super[AxiMasterReadIOTrait].sInit()
    super[AxiMasterWriteIOTrait].sInit()
  }

  override def <>(that: AxiMasterIOTrait) {
    super[AxiMasterReadIOTrait].<>(that)
    super[AxiMasterWriteIOTrait].<>(that)
  }
}

trait AxiMasterReadIOTrait extends Bundle {
  val addr_width: Int
  val data_width: Int
  val read_addr: AxiAddressIO
  val read_data: AxiReadDataIO

  def mInit() {
    read_addr.mInit()
    read_data.mInit()
  }

  def sInit() {
    read_addr.sInit()
    read_data.sInit()
  }

  def <>(that: AxiMasterIOTrait) {
    this.read_addr <> that.read_addr
    this.read_data <> that.read_data
  }

  def <>(that: AxiMasterReadIOTrait) {
    this.read_addr <> that.read_addr
    this.read_data <> that.read_data
  }
}

trait AxiMasterWriteIOTrait extends Bundle {
  val addr_width: Int
  val data_width: Int
  val write_addr: AxiAddressIO
  val write_data: AxiWriteDataIO
  val write_resp: AxiWriteResponseIO

  def mInit() {
    write_addr.mInit()
    write_data.mInit()
    write_resp.mInit()
  }

  def sInit() {
    write_addr.sInit()
    write_data.sInit()
    write_resp.sInit()
  }

  def <>(that: AxiMasterIOTrait) {
    this.write_addr <> that.write_addr
    this.write_data <> that.write_data
    this.write_resp <> that.write_resp
  }

  def <>(that: AxiMasterWriteIOTrait) {
    this.write_addr <> that.write_addr
    this.write_data <> that.write_data
    this.write_resp <> that.write_resp
  }
}

class AxiLiteSlaveIO(val addr_width: Int, val data_width: Int) extends Bundle {
  val read_addr = Flipped(new AxiLiteAddressIO(UInt(addr_width.W)))
  val read_data = new AxiLiteReadDataIO(UInt(data_width.W))
  val write_addr = Flipped(new AxiLiteAddressIO(UInt(addr_width.W)))
  val write_data = Flipped(new AxiLiteWriteDataIO(UInt(data_width.W)))
  val write_resp = new AxiLiteWriteResponseIO(UInt(Axi4Config.Resp.width.W))

  def sInit() {
    read_addr.sInit()
    read_data.sInit()
    write_addr.sInit()
    write_data.sInit()
    write_resp.sInit()
  }

  def mInit() {
    read_addr.mInit()
    read_data.mInit()
    write_addr.mInit()
    write_data.mInit()
    write_resp.mInit()
  }
}

// Encoding in ARM IHI 0022F.b (ID122117) //////////////////////////////////////
//   AMBA® AXI and ACE Protocol Specification (AXI3, AXI4, AXI5, ACE and ACE5)
//   at https://developer.arm.com/docs/ihi0022/fb
object Axi4Config {

  object Id {
    val width = 1 // Warning: should be configurable
  }

  object User {
    val width = 1 // Warning: should be configurable
  }

  object Qos {
    val width = 4
  }

  object Region {
    val width = 4
  }

  object Len {
    // A3.4.1 Address structure: Burst length
    val width = 8

    def apply(length: UInt): UInt = {
      val code = WireDefault(0.U(width.W))
      code := length - 1.U
      return code
    }
  }

  object Size {
    // Table A3-2 Burst size encoding
    val width = 3

    def apply(bytes: Int): UInt = bytes match {
      case 1 => "b000".U(3.W)
      case 2 => "b001".U(3.W)
      case 4 => "b010".U(3.W)
      case 8 => "b011".U(3.W)
      case 16 => "b100".U(3.W)
      case 32 => "b101".U(3.W)
      case 64 => "b110".U(3.W)
      case 128 => "b111".U(3.W)
    }
  }

  object Burst {
    // Table A3-3 Burst type encoding
    val width = 2
    val FIXED = "b00".U(2.W)
    val INCR = "b01".U(2.W)
    val WRAP = "b10".U(2.W)
    val RESERVED = "b11".U(2.W)
  }

  object Resp {
    // Table A3-4 RRESP and BRESP encoding
    val width = 2
    val OKAY = "b00".U(2.W)
    val EXOKAY = "b01".U(2.W)
    val SLVERR = "b10".U(2.W)
    val DECERR = "b11".U(2.W)
  }

  trait CacheAx {
    // Table A4-5 Memory type encoding
    val width = 4
    val DEVICE_NON_BUFFERABLE = "b0000".U(4.W)
    val DEVICE_BUFFERABLE = "b0001".U(4.W)
    val NORMAL_NON_CACHEABLE_NON_BUFFERABLE = "b0010".U(4.W)
    val NORMAL_NON_CACHEABLE_BUFFERABLE = "b0011".U(4.W)
  }

  object Cache extends CacheAx {
  }

  object CacheAr extends CacheAx {
    val WRITE_THROUGH_NO_ALLOCATE = "b1010".U(4.W)
    val WRITE_THROUGH_READ_ALLOCATE = "b1110".U(4.W)
    val WRITE_THROUGH_WRITE_ALLOCATE = "b1010".U(4.W)
    val WRITE_THROUGH_READ_AND_WRITE_ALLOCATE = "b1110".U(4.W)
    val WRITE_BACK_NO_ALLOCATE = "b1011".U(4.W)
    val WRITE_BACK_READ_ALLOCATE = "b1111".U(4.W)
    val WRITE_BACK_WRITE_ALLOCATE = "b1011".U(4.W)
    val WRITE_BACK_READ_AND_WRITE_ALLOCATE = "b1111".U(4.W)
  }

  object cacheAw extends CacheAx {
    val WRITE_THROUGH_NO_ALLOCATE = "b0110".U(4.W)
    val WRITE_THROUGH_READ_ALLOCATE = "b0110".U(4.W)
    val WRITE_THROUGH_WRITE_ALLOCATE = "b1110".U(4.W)
    val WRITE_THROUGH_READ_AND_WRITE_ALLOCATE = "b1110".U(4.W)
    val WRITE_BACK_NO_ALLOCATE = "b0111".U(4.W)
    val WRITE_BACK_READ_ALLOCATE = "b0111".U(4.W)
    val WRITE_BACK_WRITE_ALLOCATE = "b1111".U(4.W)
    val WRITE_BACK_READ_AND_WRITE_ALLOCATE = "b1111".U(4.W)
  }

  object Prot {
    val width = 3
    // Table A4-6 Protection encoding
    // bit mask for prot[0]
    val UNPRIVILEGED = "b0".U(3.W)
    val PRIVILEGED = "b1".U(3.W)
    // bit mask for prot[1]
    val SECURE = "b00".U(3.W)
    val NONSECURE = "b10".U(3.W)
    // bit mask for prot[2]
    val DATA = "b000".U(3.W)
    val INSTRUCTION = "b100".U(3.W)
  }

  object Lock {
    // Table A7-2 AXI4 atomic access encoding
    val width = 1
    val NORMAL = false.B
    val EXECLUSIVE = true.B

    object Axi3 {
      // Table A7-1 AXI3 atomic access encoding
      val width = 2
      val NORMAL = "b00".U(2.W)
      val EXECLUSIVE = "b01".U(2.W)
      val LOCKDED = "b10".U(2.W)
      val RESERVED = "b11".U(2.W)
    }

  }

}

// Table B1-1 shows the required signals on an AXI4-Lite interface. ////////////

trait AxiLiteAddressIOTrait extends DecoupledIO[UInt] {
  // directions in the perspective of master (address source)
  def addr: UInt = bits

  val prot = Output(UInt(Axi4Config.Prot.width.W))

  def mInit() {
    this.noenq()
    prot := {
      import Axi4Config.Prot._; UNPRIVILEGED | SECURE | DATA
    }
  }

  def sInit() {
    this.nodeq()
  }
}

class AxiLiteAddressIO(gen: UInt) extends DecoupledIO(gen) with AxiLiteAddressIOTrait {
  override def cloneType: this.type = new AxiLiteAddressIO(gen).asInstanceOf[this.type]
}

trait AxiLiteReadDataIOTrait extends DecoupledIO[UInt] {
  // directions in the perspective of slave (read data source)
  def data: UInt = bits

  val resp = Output(UInt(Axi4Config.Resp.width.W))

  def mInit() {
    this.nodeq()
  }

  def sInit() {
    this.noenq()
    resp := DontCare
  }
}

class AxiLiteReadDataIO(gen: UInt) extends DecoupledIO(gen) with AxiLiteReadDataIOTrait {
  override def cloneType: this.type = new AxiLiteReadDataIO(gen).asInstanceOf[this.type]
}

trait AxiLiteWriteDataIOTrait extends DecoupledIO[UInt] {
  // directions in the perspective of master (write data source)
  private val strb_width = bits.getWidth / 8

  def data: UInt = bits

  val strb = Output(UInt(strb_width.W))

  def mInit() {
    this.noenq()
    strb := ~0.U(strb_width.W) // bit-wise not
  }

  def sInit() {
    this.nodeq()
  }
}

class AxiLiteWriteDataIO(gen: UInt) extends DecoupledIO(gen) with AxiLiteWriteDataIOTrait {
  override def cloneType: this.type = new AxiLiteWriteDataIO(gen).asInstanceOf[this.type]
}

trait AxiLiteWriteResponseIOTrait extends DecoupledIO[UInt] {
  // directions in the perspective of slave (write response source)
  def resp: UInt = bits

  def mInit() {
    this.nodeq()
  }

  def sInit() {
    this.noenq()
  }
}

class AxiLiteWriteResponseIO(gen: UInt) extends DecoupledIO(gen) with AxiLiteWriteResponseIOTrait {
  override def cloneType: this.type = new AxiLiteWriteResponseIO(gen).asInstanceOf[this.type]
}

// Implementations of the AXI IOs //////////////////////////////////////////////

trait AxiIdentifier {
  // directions in the perspective of message source
  val id = Output(UInt(Axi4Config.Id.width.W))
  val user = Output(UInt(Axi4Config.User.width.W))

  def init() {
    id := 0.U
    user := 0.U
  }
}

class AxiAddressIO(gen: UInt) extends DecoupledIO(gen) with AxiIdentifier with AxiLiteAddressIOTrait {
  // Table A2-2 Write address channel signals
  // Table A2-5 Read address channel signals
  val len = Output(UInt(Axi4Config.Len.width.W)) // === #transfers minus one
  val size = Output(UInt(Axi4Config.Size.width.W)) // === log(bytes) per transfer
  val burst = Output(UInt(Axi4Config.Burst.width.W))
  val lock = Output(UInt(Axi4Config.Lock.Axi3.width.W))
  val cache = Output(UInt(Axi4Config.Cache.width.W))

  val qos = Output(UInt(Axi4Config.Qos.width.W))
  val region = Output(UInt(Axi4Config.Region.width.W))

  override def mInit() {
    super[AxiIdentifier].init()
    super[AxiLiteAddressIOTrait].mInit()

    len := 0.U
    size := Axi4Config.Size(2)
    burst := Axi4Config.Burst.INCR
    lock := Axi4Config.Lock.Axi3.NORMAL
    cache := Axi4Config.Cache.NORMAL_NON_CACHEABLE_BUFFERABLE

    qos := 0.U
    region := 0.U
  }

  // master mode
  def setLen(len: UInt) {
    this.len := len - 1.U
  }

  def setSize(size: Int) {
    this.size := Axi4Config.Size(size)
  }

  // slave mode
  def getLen(): UInt = (len + 1.U((Axi4Config.Len.width + 1).W))

  def getSize(): UInt = (1.U << size)

  override def cloneType: this.type = new AxiAddressIO(gen).asInstanceOf[this.type]
}

class AxiReadDataIO(gen: UInt) extends DecoupledIO(gen) with AxiIdentifier with AxiLiteReadDataIOTrait {
  // Table A2-6 Read data channel signals
  val last = Output(Bool())

  override def sInit() {
    super[AxiIdentifier].init()
    super[AxiLiteReadDataIOTrait].sInit()
    last := false.B
  }

  override def cloneType: this.type = new AxiReadDataIO(gen).asInstanceOf[this.type]
}

class AxiWriteDataIO(gen: UInt) extends DecoupledIO(gen) with AxiIdentifier with AxiLiteWriteDataIOTrait {
  // Table A2-3 Write data channel signals
  val last = Output(Bool())

  override def mInit() {
    super[AxiIdentifier].init()
    super[AxiLiteWriteDataIOTrait].mInit()
    last := false.B
  }

  override def cloneType: this.type = new AxiWriteDataIO(gen).asInstanceOf[this.type]
}

class AxiWriteResponseIO(gen: UInt) extends DecoupledIO(gen) with AxiIdentifier with AxiLiteWriteResponseIOTrait {
  // Table A2-4 Write response channel signals

  override def sInit() {
    super[AxiIdentifier].init()
    super[AxiLiteWriteResponseIOTrait].sInit()
  }

  override def cloneType: this.type = new AxiWriteResponseIO(gen).asInstanceOf[this.type]
}

