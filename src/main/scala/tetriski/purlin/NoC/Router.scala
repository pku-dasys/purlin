package tetriski.purlin.NoC

import chisel3.util._
import chisel3.{Bundle, Input, Module, Vec, _}
import tetriski.purlin.utils.{Coordinate, FunctionType, InterconnectType, Parameters}

import scala.collection.mutable.ArrayBuffer

/** The base class of router.
 */
class Router(y: Int, x: Int, packetRule: () => Bundle) extends Module {
  override def desiredName = "Router_" + x + "_" + y

  def getPacketRule = packetRule

  val connectArray = new ArrayBuffer[Int]()
  if (Parameters.interconnectType == InterconnectType.Mesh) {
    if (x < Parameters.xSize - 1) {
      connectArray.append(Parameters.E)
    }
    if (x > 0) {
      connectArray.append(Parameters.W)
    }
    if (y < Parameters.ySize - 1) {
      connectArray.append(Parameters.S)
    }
    if (y > 0) {
      connectArray.append(Parameters.N)
    }
  } else {
    //Torus
    connectArray.append(Parameters.E)
    connectArray.append(Parameters.W)
    connectArray.append(Parameters.S)
    connectArray.append(Parameters.N)
  }


  val connectSize = connectArray.size
  val size = connectArray.size + 1

  val xUInt = x.U(Parameters.log2X.W)
  val yUInt = y.U(Parameters.log2Y.W)

  val co = Cat(xUInt, yUInt).asTypeOf(new Coordinate)

  val defaultX = 0.U(Parameters.log2X.W)
  val defaultY = 0.U(Parameters.log2Y.W)
  val defaultP = 0.U
  val defaultRouting = 0.U(log2Ceil(4 * (Parameters.xSize + Parameters.ySize)).W)

  val stressWidth = if (Parameters.functionType != FunctionType.XY) {
    log2Ceil(Parameters.fifoDep + 1) + 2
  } else {
    0
  }


  val io = IO(new Bundle {
    val enqs = Vec(connectSize, Flipped(new DecoupledIO(packetRule.apply())))
    val deqs = Vec(connectSize, new DecoupledIO(packetRule.apply()))

    val en = Input(Bool())

    val enqFromTile = Flipped(new DecoupledIO(packetRule.apply()))
    val deqToTile = new DecoupledIO(packetRule.apply())

    val stressIn = Vec(connectSize, Input(UInt((stressWidth).W)))
    val stressOut = Output(UInt((stressWidth).W))
  })


  val enqs = (0 until size).map(_ => Wire(Flipped(new DecoupledIO(packetRule.apply()))))
  (0 until size - 1).foreach(i => enqs(i) <> io.enqs(i))
  enqs(size - 1) <> io.enqFromTile

  val deqs = (0 until size).map(_ => Wire(new DecoupledIO(packetRule.apply())))
  (0 until size - 1).foreach(i => deqs(i) <> io.deqs(i))
  deqs(size - 1) <> io.deqToTile
}
