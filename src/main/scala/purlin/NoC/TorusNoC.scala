package purlin.NoC

import chisel3.util._
import chisel3.{Bundle, Input, Module, Vec, _}
import purlin.utils.Parameters


/** The packet-switched network module.
 * TODO: use the packet-switched model to initialize this module.
 * TODO: for some reasons in another project, the order of x-y in this module is reversed, and it should be corrected.
 *
 * @param routerRule the rule to initialize a router in (y, x)
 * @param packetRule the rule to distinguish simple routers or multi-channel routers.
 */
class TorusNoC(routerRule: (Int, Int) => Router, packetRule: () => Bundle) extends Module {
  override def desiredName = "TorusNoc_" + Parameters.channelSize + "_" + Parameters.xSize + "_" + Parameters.ySize

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
          case Parameters.W => routerMap(y, (x - 1) % Parameters.xSize)
          case Parameters.S => routerMap((y + 1) % Parameters.ySize, x)
          case Parameters.N => routerMap((y - 1) % Parameters.ySize, x)
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


