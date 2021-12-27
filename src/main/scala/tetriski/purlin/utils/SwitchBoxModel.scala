package tetriski.purlin.utils

import chisel3.util.log2Ceil

import scala.collection.mutable.ArrayBuffer

/** A bundle of config to help config generation.
 */
class ConfigBundle {
  var y = 0
  var x = 0
  var dIndex = 0
  var channel = 0
  var config = 0

  /** Initialize this bundle.
   *
   * @param x       the x index of a router
   * @param y       the y index of a router
   * @param dIndex  the port index (different from direction index)
   * @param channel the channel index
   * @param config  the configuration
   */
  def this(x: Int, y: Int, dIndex: Int, channel: Int, config: Int) = {
    this()
    this.y = y
    this.x = x
    this.dIndex = dIndex
    this.channel = channel
    this.config = config
  }
}

/** A network model of circuit-switched on-chip networks based on switch boxes.
 * We only generate one configuration for FPGA-like spatial computation.
 * We should choose accurate channel index in this model, different from the packet-switched model.
 */
class MeshSBModel extends MeshModel {

  var Fs = 0
  var configSize = 0
  var maxAdjacency = 0

  var topologyMap = Map[(SBModel, Int), (SBModel, Int)]()
  var topologyDirectionMap = Map[(SBModel, Int), (SBModel, Int)]()
  var routerModelMap = Map[(Int, Int), SBModel]()

  /** Go to next router from the "dst" direction of current router.
   *
   * @param router the current router
   * @param dst    (the direction connected to next router, the channel index)
   * @return (nextRouter, (direction from this router, the channel index))
   */
  def gotoNextRouter(router: SBModel, dst: (Int, Int)): (SBModel, (Int, Int)) = {
    //    println("!!!", router.x, router.y, dst._1)
    val sink = topologyDirectionMap((router, dst._1))
    val sinkRouter = sink._1
    val sinkPort = sink._2
    val sinkChannel = dst._2
    (sinkRouter, (sinkPort, sinkChannel))
  }

  /** Clear all allocated paths.
   */
  def clearPath: Unit = {
    routerModelMap.foreach(p => p._2.clearPath)
  }

  /** Get an array of config bundles of this network.
   */
  def getConfigArray: Array[ConfigBundle] = {
    var ret = new Array[ConfigBundle](0)
    for (y <- 0 until ySize) {
      for (x <- 0 until xSize) {
        val singleConfig = routerModelMap(x, y).getConfigArray
        ret ++= singleConfig
      }
    }
    ret
  }


  /** Allocate a path in router (x,y) at from "src" to "dst".
   *
   * @param x   the x index
   * @param y   the y index
   * @param src (the source direction, the source channel)
   * @param dst (the destination direction, the destination channel)
   *
   */
  def setPath(x: Int, y: Int, src: (Int, Int), dst: (Int, Int)): Unit = {
    val SBModel = routerModelMap((x, y))
    SBModel.setPath(src, dst)
  }

  /** Create this circuit-switched network model.
   *
   * @param channelSize the number of channels
   * @param xSize       the x size of this network
   * @param ySize       the y size of this network
   * @param Fs          the number of possible connections offered to each output channel
   *
   */
  def this(channelSize: Int, xSize: Int, ySize: Int, Fs: Int) {
    this()
    this.channelSize = channelSize
    this.xSize = xSize
    this.ySize = ySize
    this.Fs = Fs
    init()
  }

  /** Initialize this network model.
   * In fact, the topology can be diverse in this function.
   * For example, if router (1, 1) is abandoned, the east of router (0, 1) could be router (2, 1).
   * And the correct chisel module and configuration can also be generated correctly.
   *
   * TODO: introduce more topologies like fat-tree and ring
   */
  def init(): Unit = {
    for (x <- 0 until xSize) {
      for (y <- 0 until ySize) {
        val routerModel = new SBModel(Fs, x, y, channelSize, xSize, ySize)
        routerModelMap += (x, y) -> routerModel
      }
    }
    //mesh
    for (x <- 0 until xSize) {
      for (y <- 0 until ySize) {
        val routerModel = routerModelMap(x, y)
        val adjacency = routerModel.adjacency
        for (i <- 0 until adjacency.size - 1) {
          val direction = adjacency(i)
          val reDirection = Parameters.reverse(direction)

          val dstRouter = direction match {
            case Parameters.E => routerModelMap(x + 1, y)
            case Parameters.W => routerModelMap(x - 1, y)
            case Parameters.S => routerModelMap(x, y + 1)
            case Parameters.N => routerModelMap(x, y - 1)
          }

          topologyDirectionMap += (routerModel, direction) -> (dstRouter, reDirection)
          val dstIndex = dstRouter.adjacency.indexOf(reDirection)
          topologyMap += (routerModel, i) -> (dstRouter, dstIndex)
          //          println(routerModel.x, routerModel.y, direction)
        }
      }
    }
    maxAdjacency = 5

    configSize = routerModelMap.values.toList.map(model => model.configSize).max
  }
}


/** The model of a circuit-switched router.
 */
class SBModel {
  var x = 0
  var y = 0
  var Fs = 0
  //  var Fc = 0
  var adjacency = Array[Int]()
  var channelSize = 0
  var portConnectMap = Map[(Int, Int), Array[(Int, Int)]]()

  def tilePort = -1

  var configSize = 0
  var pathSelectMap = Map[(Int, Int), (Int, Int)]()

  var sinkFromSrc = Map[(Int, Int), Array[(Int, Int)]]()

  /** Clear all allocated path.
   */
  def clearPath: Unit = {
    pathSelectMap = Map[(Int, Int), (Int, Int)]()
  }

  /** Find feasible destinations for next hop.
   *
   * @return a set of (direction, channel) pairs
   */
  def findPath(src: (Int, Int)): Set[(Int, Int)] = {
    val dsts = sinkFromSrc(src)
    dsts.toSet.&~(pathSelectMap.keys.toSet)
  }

  /** Find the port index of a direction.
   *
   * @param direction the direction in utils.Parameters (e.g. E = 0)
   * @return the port index
   */
  def findPortIndex(direction: Int) = adjacency.indexOf(direction)


  /** Initialize this router model.
   *
   * @param Fs          the number of possible connections offered to each output channel
   * @param x           the x index
   * @param y           the y index
   * @param channelSize the channel number
   * @param xSize       the x size of the network
   * @param ySize       the y size of the network
   */
  def this(Fs: Int, x: Int, y: Int, channelSize: Int, xSize: Int = Parameters.xSize, ySize: Int = Parameters.ySize) = {
    this()
    this.Fs = Fs
    this.x = x
    this.y = y
    //    this.Fc = Fc

    val connectArray = new ArrayBuffer[Int]()
    if (x < xSize - 1) {
      connectArray.append(Parameters.E)
    }
    if (x > 0) {
      connectArray.append(Parameters.W)
    }
    if (y < ySize - 1) {
      connectArray.append(Parameters.S)
    }
    if (y > 0) {
      connectArray.append(Parameters.N)
    }

    this.adjacency = connectArray.toArray ++ Array(tilePort)
    this.channelSize = channelSize

    this.configSize = log2Ceil(Fs)

    for (dst <- adjacency) {
      for (dstC <- 0 until channelSize) {
        var count = 0
        val srcBuffer = new ArrayBuffer[(Int, Int)]()
        for (srcC <- dstC until (channelSize + dstC)) {
          for (src <- adjacency.sortBy(i => Math.abs(3 - i - dst))) {
            if (src != dst) {
              if (count < Fs) {
                srcBuffer.append((src, srcC % channelSize))
                count += 1
              }
            }
          }
        }
        this.portConnectMap += (dst, dstC) -> srcBuffer.toArray
      }
    }

    for (src <- adjacency) {
      for (srcC <- 0 until channelSize) {
        sinkFromSrc += (src, srcC) -> this.portConnectMap.filter(p => p._2.contains((src, srcC))).keys.toArray
      }
    }
  }

  /** Allocate a path in this router at from "src" to "dst".
   *
   * @param src (the source direction, the source channel)
   * @param dst (the destination direction, the destination channel)
   *
   */
  def setPath(src: (Int, Int), dst: (Int, Int)) = {
    var flag = false
    if (portConnectMap.contains(dst)) {
      if (portConnectMap(dst).contains(src)) {
        flag = true
        if (pathSelectMap.contains(dst)) {
          pathSelectMap -= dst
          pathSelectMap += dst -> src
        } else {
          pathSelectMap += dst -> src
        }
      }
    }
    if (!flag) {
      throw new Exception("Invalid path!" + src + " " + dst)
    }

  }

  /** Get an array of config bundles of this router.
   */
  def getConfigArray: Array[ConfigBundle] = {
    val ret = new ArrayBuffer[ConfigBundle]()

    for (d <- adjacency) {
      for (c <- 0 until channelSize) {
        val dIndex = findPortIndex(d)
        if (pathSelectMap.contains((d, c))) {
          val configBundle = new ConfigBundle(x, y, dIndex, c, portConnectMap((d, c)).indexOf(pathSelectMap((d, c))))
          ret.append(configBundle)
        }
      }
    }
    ret.toArray
  }
}
