package tetriski.purlin.utils

import chisel3.util.log2Ceil
import chisel3.{Bool, Bundle, UInt, _}

object FunctionType extends Enumeration {
  type FunctionType = Value
  val  XY, DyXY, WestFirst, ModifiedWestFirst = Value
}

object TrafficType extends Enumeration {
  type TrafficType = Value
  val  UniformRandom, HotSpot, Transpose, BitReversal = Value
  var hotSpot: (Int, Int) = (0, 0)

  def initHotSpot(): Unit ={
    hotSpot = (scala.util.Random.nextInt(Parameters.xSize), scala.util.Random.nextInt(Parameters.ySize))
  }
}

object InterconnectType extends Enumeration {
  type InterconnectType = Value
  val  Mesh, Torus = Value
}

/** Global parameters.
 */
object Parameters {
  var xSize = 4
  var ySize = 4

  /** A factor for congestion-aware algorithm
   */
  var congestionFactor = 0.2
  /** A factor for latency estimation algorithm
   */
  var overlapPunishFactor = 1.0

  var tilePortSize = 1

  var channelSize = 2

  var useMultiChannelRouter = false

  var sourceRouting = true

  /* The Head-Body-Tail Packet and virtual channels*/
  var USE_VC_HBT = false

  var functionType = FunctionType.XY

  var trafficType = TrafficType.UniformRandom

  var interconnectType = InterconnectType.Mesh

  /** Do not use source routing in packet-switched on-chip networks.
   */
  def abandonSourceRouting(): Unit = {
    sourceRouting = false
  }

  def log2X = log2Ceil(xSize)

  def log2Y = log2Ceil(ySize)

  def log2TilePortSize = {
    var size = 0
    if (tilePortSize != 0) {
      size = log2Ceil(tilePortSize)
      //for CGRA Testing
      if (size == 0) {
        size = 1
      }
    } else {
      //abandon srcPort in Header
      size = 0
    }
    size
  }

  /** default: source routing max hops = xSize + ySize
   */
  def log2Routing = if (sourceRouting) {
    2 * (Parameters.xSize + Parameters.ySize)
  } else {
    0
  }

  //  def log2Routing = 2 * (Parameters.xSize * Parameters.ySize)

  def log2MemSize = log2Ceil(memSize)

  var fifoDep = 16

  var payloadSize = 32

  var memSize = 4

  val E = 0
  val W = 3
  val S = 1
  val N = 2
  val TILE = -1

  val Fault = 5


  /** For broadcast, a packet could be granted to at most 4 destination
   */
  var grantNumLimit = 4

  var grantRegion = 5 //E, W, S, N, Arrival

  var useBroadcast = true

  /** Retrench the packet-switched network by abandoning broadcast and srcPort in Header.
   */
  def retrench(): Unit = {
    //abandon broadcast
    grantNumLimit = 1
    useBroadcast = false

    //abandon srcPort in Header
    tilePortSize = 0
  }

  def getGrantNumWidth = log2Ceil(Parameters.grantNumLimit + 1)

  def getGrantWidth = log2Ceil(grantRegion)

  def getRoutingRegionWidth = log2Ceil(grantRegion - 1) //E, W, S, N

  def reverse(direction: Int) = 3 - direction

  def strDirection(direction: Int) = direction match {
    case E => "E"
    case W => "W"
    case S => "S"
    case N => "N"
  }

  def intDirection(direction: String) = direction match {
    case "E" => E
    case "W" => W
    case "S" => S
    case "N" => N
  }
}


/** A coordinate bundle.
 */
class Coordinate(val valid: Boolean = true)  extends Bundle {
  val xWidth = if(valid){Parameters.log2X}else{0}
  val yWidth = if(valid){Parameters.log2Y}else{0}
  val x = UInt(xWidth.W)
  val y = UInt(yWidth.W)

  def ===(that: Coordinate): Bool = {
    x === that.x && y === that.y
  }

}

/** A header bundle.
 */
class Header(val hasHead: Boolean = true)  extends Bundle {
  val src = new Coordinate(hasHead)
  val dst = new Coordinate(hasHead)
  val srcPortWidth = if(hasHead){Parameters.log2TilePortSize}else{0}
  val srcPort = UInt(srcPortWidth.W)
  val routingWidth = if(hasHead){Parameters.log2Routing}else{0}
  val routing = UInt(routingWidth.W)
}

/** A mini-packet (flit) bundle.
 */
class MiniPacket(val hasHead: Boolean = true) extends Bundle {
  val header = new Header(hasHead)
  val payloadWidth = if(Parameters.USE_VC_HBT && hasHead) {
    Parameters.payloadSize - header.getWidth
  }else{
    Parameters.payloadSize
  }
  val payload = UInt(payloadWidth.W)

  val typeWidth = if(Parameters.USE_VC_HBT){2}else{0}
  val flitType = UInt(typeWidth.W)

  val vcIdWidth = if(Parameters.USE_VC_HBT){log2Ceil(Parameters.channelSize)}else{0}
  val vcId = UInt(vcIdWidth.W)
}


/** A bundle containing a mini-packet and its grants.
 */
class AnalyzedPacket extends Bundle {
  val packet = new MiniPacket
  val grantNum = UInt(log2Ceil(Parameters.grantNumLimit + 1).W)
  val grants = Vec(Parameters.grantNumLimit, UInt(Parameters.getGrantWidth.W))
}

/** A multi-channel packet containing some mini-packets.
 * We consider a multi-channel packet composed of a valid number and some mini-packets
 * as the basic transmission unit between routers
 */
class MultiChannelPacket extends Bundle {
  val validNum = UInt(log2Ceil(Parameters.channelSize + 1).W)
  val packets = Vec(Parameters.channelSize, new MiniPacket)
}


/** A bundle containing some headers. (only used in CGRA testing)
 */
class DeliverCtrl extends Bundle {
  val validNum = UInt(log2Ceil(Parameters.tilePortSize + 1).W)
  val headers = Vec(Parameters.tilePortSize, new Header)
}

/** A bundle for checking after receiving a packet. (only used in CGRA testing)
 */
class ReceivePattern extends Bundle {
  val src = new Coordinate
  val srcPort = UInt(Parameters.log2TilePortSize.W)
  val dstPort = UInt(Parameters.log2TilePortSize.W)

  def check(header: Header) = (src === header.src) & (srcPort === header.srcPort)
}

/** A bundle for checking after receiving some packets. (only used in CGRA testing)
 */
class ReceiveCtrl extends Bundle {
  val validNum = UInt(log2Ceil(Parameters.tilePortSize + 1).W)
  val patterns = Vec(Parameters.tilePortSize, new ReceivePattern)
}