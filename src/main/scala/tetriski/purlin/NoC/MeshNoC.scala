package tetriski.purlin.NoC

import chisel3.util._
import chisel3.{Bundle, Input, Module, Vec, _}
import tetriski.purlin.utils.Parameters
//import tetriski.pillars.Purlin.MultiChannelPacket


/** The packet-switched network module.
 * TODO: use the packet-switched model to initialize this module.
 * TODO: for some reasons in another project, the order of x-y in this module is reversed, and it should be corrected.
 *
 * @param routerRule the rule to initialize a router in (y, x)
 * @param packetRule the rule to distinguish simple routers or multi-channel routers.
 */
class MeshNoC(routerRule: (Int, Int) => Router, packetRule: () => Bundle) extends Module {
  override def desiredName = "MeshNoc_" + Parameters.channelSize + "_" + Parameters.xSize + "_" + Parameters.ySize

  val io = IO(new Bundle {
    val en = Input(Bool())

    //    val tileOutputValid = Vec(NoCParam.ySize, Vec(NoCParam.xSize, Input(Bool())))
    val enqFromTiles = Vec(Parameters.ySize, Vec(Parameters.xSize, Flipped(new DecoupledIO(packetRule.apply()))))
    //    val tileInputReady = Vec(NoCParam.ySize, Vec(NoCParam.xSize, Input(Bool())))
    val deqToTiles = Vec(Parameters.ySize, Vec(Parameters.xSize, new DecoupledIO(packetRule.apply())))
  })
  var routerMap = Map[(Int, Int), Router]()
  //  var tileMap = Map[(Int, Int), Data]()


  for (x <- 0 until Parameters.xSize) {
    for (y <- 0 until Parameters.ySize) {
      val router = Module(routerRule.apply(y, x))
      routerMap += (y, x) -> router
      router.io.en <> io.en
      router.io.enqFromTile <> io.enqFromTiles(y)(x)
      router.io.deqToTile <> io.deqToTiles(y)(x)

      //      val outputReg = RegInit(packetRule.apply())
      //      when(io.en && io.tileOutputValid(y)(x)) {
      //        outputReg := io.enqFromTiles(y)(x)
      //        router.io.enqFromTile.valid := true.B
      //      }
      //      router.io.enqFromTile.bits := outputReg
      //
      //      val inputReg = RegInit(packetRule.apply())
      //      tileMap += (y, x) -> inputReg
      //      io.deqToTiles(y)(x) := inputReg

      //      val headerMem = Mem(NoCParam.memSize, new Header)
      //      val header = io.headers(y)(x)

      //      val packet = Cat(header, outputReg).asTypeOf(new Packet)
      //      val packet = Wire(new Packet)
      //      packet


      //      router.io.deqToTile.ready := io.tileInputReady(y)(x)
      //      when(router.io.deqToTile.valid && io.tileInputReady(y)(x)) {
      ////        val packet = router.io.deqToTile.bits.asTypeOf(new Packet)
      //        inputReg := router.io.deqToTile.bits
      //      }.otherwise{
      //        inputReg := 0.U.asTypeOf(packetRule.apply())
      //      }

    }
  }


  for (x <- 0 until Parameters.xSize) {
    for (y <- 0 until Parameters.ySize) {
      val srcRouter = routerMap(y, x)
      val connectArray = srcRouter.connectArray
      for (i <- 0 until connectArray.size) {
        val direction = connectArray(i)
        val reDirection = Parameters.reverse(direction)

        val dstRouter = direction match {
          case Parameters.E => routerMap(y, (x + 1) % Parameters.xSize)
          case Parameters.W => routerMap(y, (x - 1 + Parameters.xSize) % Parameters.xSize)
          case Parameters.S => routerMap((y + 1) % Parameters.ySize, x)
          case Parameters.N => routerMap((y - 1 + Parameters.ySize) % Parameters.ySize, x)
        }

        val dstIndex = dstRouter.connectArray.indexOf(reDirection)
        dstRouter.io.enqs(dstIndex) <> srcRouter.io.deqs(i)
        dstRouter.io.stressIn(dstIndex) <> srcRouter.io.stressOut
        dstRouter.io.vcValidIn(dstIndex) <> srcRouter.io.vcValidOut(i)
      }
    }
  }

  //  io.test := tileMap(3, 2)
}


/** A wrapper for packet-switched network module.
 * It is used for evaluate the area of this module in FPGA where the IO bits is limited.
 */
class NoCWrapperForPR(routerRule: (Int, Int) => Router, packetRule: () => Bundle) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val enqX = Input(UInt(log2Ceil(Parameters.xSize).W))
    val enqY = Input(UInt(log2Ceil(Parameters.ySize).W))
    val deqX = Input(UInt(log2Ceil(Parameters.xSize).W))
    val deqY = Input(UInt(log2Ceil(Parameters.ySize).W))
    val enqFromTile = Flipped(new DecoupledIO(packetRule.apply()))
    val deqToTile = new DecoupledIO(packetRule.apply())
    //    val enqFromTile = Vec(Parameters.ySize, Vec(Parameters.xSize, Flipped(new DecoupledIO(packetRule.apply()))))
    //    val deqToTile = Vec(Parameters.ySize, Vec(Parameters.xSize, new DecoupledIO(packetRule.apply())))
  })


  val mesh = Module(new MeshNoC(routerRule, packetRule))
  mesh.io.en <> io.en
  for (x <- 0 until Parameters.xSize)
    for (y <- 0 until Parameters.ySize) {
      mesh.io.enqFromTiles(x)(y) <> DontCare
      mesh.io.deqToTiles(x)(y) <> DontCare
    }

  mesh.io.enqFromTiles(io.enqY)(io.enqX) <> io.enqFromTile
  mesh.io.deqToTiles(io.deqY)(io.deqX) <> io.deqToTile
}
