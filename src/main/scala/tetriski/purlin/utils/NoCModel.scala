package tetriski.purlin.utils

import scala.collection.mutable.ArrayBuffer


/** A network model of packet-switched on-chip networks.
 * TODO: introduce more topologies like fat-tree and ring
 * TODO: realize a faster simulator to help improve algorithms
 */
class MeshNoCModel extends MeshModel {

  var maxAdjacency = 0
  var congestionLimit = 0.0

  var topologyMap = Map[(NoCRouterModel, Int), (NoCRouterModel, Int)]()
  var topologyDirectionMap = Map[(NoCRouterModel, Int), (NoCRouterModel, Int)]()
  var routerModelMap = Map[(Int, Int), NoCRouterModel]()

  var allocatedMessages = Map[Int, Message]()

  /** Estimate the average latency of messages allocated in this network model.
   */
  def estimateAll(): Double = {
    var latency = 0.0
    for (item <- allocatedMessages) {
      val messageIndex = item._1
      val message = item._2
      latency += estimate(message, messageIndex, true)
    }
    latency / allocatedMessages.size.toDouble
  }

  /** Estimate the latency of a message if it is allocated in this network model.
   *
   * @param message         the message to be allocated
   * @param messageIndex    the index of the message
   * @param allocated       indicate whether the message has been allocated
   * @param inputStrategies the routing path of the message
   * @return the estimated latency
   */
  def estimate(message: Message, messageIndex: Int = -1, allocated: Boolean = false,
               inputStrategies: List[RoutingStrategy] = List()): Double = {
    val begin = message.injectionCycle.getOrElse(0)
    val packetLength = message.packetLength.getOrElse(0)
    val end = packetLength + begin
    val strategies = if (inputStrategies.size > 0) {
      inputStrategies
    } else {
      message.routingStrategy.getOrElse(List())
    }
    val routingHops = strategies.length

    var congestionOverhead = 0.0

    for (i <- 0 until routingHops) {
      val strategy = strategies(i)
      val x = strategy.routerX.getOrElse(-1)
      val y = strategy.routerY.getOrElse(-1)
      val direction = strategy.dstDirection

      var congestionLevel = getCongestionLevel(x, y, direction, message, i)
      if (allocated) {
        congestionLevel -= 1
      }

      if (congestionLevel > 0) {
        val lBegin = begin + i
        val lEnd = end + i
        val router = routerModelMap((x, y))
        val congestionIndexes = router.routingInfo.filter(item => item._2 != messageIndex).map(item => item._3)
        var overlapNum = new Array[Int](packetLength)
        for (index <- congestionIndexes) {
          val congestionMessage = allocatedMessages(index)
          val ss = congestionMessage.routingStrategy.getOrElse(List())
          for (j <- 0 until ss.size) {
            val s = ss(j)
            if (s.routerX.getOrElse(-1) == x && s.routerY.getOrElse(-1) == y && s.dstDirection == direction) {
              val cBegin = congestionMessage.injectionCycle.getOrElse(0) + j
              val cEnd = cBegin + congestionMessage.packetLength.getOrElse(0)
              val overlap = ((lBegin until lEnd).toSet & (cBegin until cEnd).toSet)
              for (index <- overlap) {
                overlapNum(index - lBegin) += 1
              }
            }
          }
        }
        val overhead = overlapNum.map(i =>
          if (i >= channelSize) {
            i - channelSize + 1
          } else {
            0
          }).sum

        //        val overhead = overlapNum.sum
        congestionOverhead += overhead.toDouble * Parameters.overlapPunishFactor
      }

    }

    congestionOverhead + routingHops.toDouble
  }


  /** Allocate a message in this network model.
   *
   * @param message      the message to be allocated
   * @param messageIndex the index of the message
   */
  def allocateMessage(message: Message, messageIndex: Int): Unit = {
    allocatedMessages += messageIndex -> message
    val injectionCycle = message.injectionCycle.getOrElse(0)
    val packetLength = message.packetLength.getOrElse(0)
    val strategies = message.routingStrategy.getOrElse(List())
    var hopIndex = 0
    for (s <- strategies) {
      setPath(s.routerX.getOrElse(-1), s.routerY.getOrElse(-1),
        s.srcDirection.getOrElse(-1), s.dstDirection, messageIndex, injectionCycle + hopIndex, packetLength)
      hopIndex += 1
    }
  }

  /** Remove a message in this network model.
   *
   * @param message      the message to be allocated
   * @param messageIndex the index of the message
   */
  def ripUP(message: Message, messageIndex: Int): Unit = {
    allocatedMessages -= messageIndex
    val injectionCycle = message.injectionCycle.getOrElse(0)
    val packetLength = message.packetLength.getOrElse(0)
    val strategies = message.routingStrategy.getOrElse(List())
    var hopIndex = 0
    for (s <- strategies) {
      ripUp(s.routerX.getOrElse(-1), s.routerY.getOrElse(-1),
        s.srcDirection.getOrElse(-1), s.dstDirection, messageIndex, injectionCycle + hopIndex, packetLength)
      hopIndex += 1
    }
  }

  /** Get the congestion level to evaluate possible congestion when allocating the #hopIndex hop of a message
   * from router (x, y) to "direction" router.
   * Assuming no congestion happens, the congestion level is the average channel overuse for
   * the #hopIndex hop during the lifetime.
   *
   * @param x         the x index
   * @param y         the y index
   * @param direction the direction of next router
   * @param message   the message to be allocated
   * @param hopIndex  the hop index of the message
   */
  def getCongestionLevel(x: Int, y: Int, direction: Int, message: Message, hopIndex: Int = 0): Int = {
    val begin = message.injectionCycle.getOrElse(0) + hopIndex
    val packetLength = message.packetLength.getOrElse(0)
    val end = packetLength + begin
    val congestionLevel = (begin until end).map(c => getPresentlyUsed(x, y, direction, c)).reduce(_ + _) / packetLength
    if (congestionLevel >= channelSize) {
      congestionLevel - channelSize + 1
    } else {
      0
    }
  }

  /** Get presently used channels from router (x, y) to "direction" router at a certain cycle,
   * assuming no congestion happens.
   *
   * @param x         the x index
   * @param y         the y index
   * @param direction the direction of next router in utils.Parameters (e.g. E = 0)
   * @param cycle     the cycle after the simulation start
   */
  def getPresentlyUsed(x: Int, y: Int, direction: Int, cycle: Int): Int = {
    val router = routerModelMap((x, y))
    val congestionMap = router.directionCongestionMap(direction)
    val congestion = if (congestionMap.contains(cycle)) {
      congestionMap(cycle)
    } else {
      0.0
    }
    Math.round(congestion * channelSize).toInt
  }


  /** Go to next router from the "dst" direction of current router.
   *
   * @param router the current router
   * @param dst    the direction connected to next router
   * @return (nextRouter, direction from this router)
   */
  def gotoNextRouter(router: NoCRouterModel, dst: Int): (NoCRouterModel, Int) = {
    val sink = topologyDirectionMap((router, dst))
    val sinkRouter = sink._1
    val sinkDirection = sink._2
    (sinkRouter, sinkDirection)
  }


  /** Clear all routers.
   */
  def clear: Unit = {
    routerModelMap.foreach(p => p._2.clear)
  }


  /** Set a hop of a message (packet) in router (x,y) at from "src" direction to "dst" direction at "cycle" to ("cycle" + "packetLength").
   *
   * @param x            the x index
   * @param y            the y index
   * @param src          the source direction
   * @param dst          the destination direction
   * @param msgIndex     the index of the message
   * @param cycle        the cycle when the first flit of the packet go through this router
   * @param packetLength the length of the packet
   *
   */
  def setPath(x: Int, y: Int, src: Int, dst: Int, msgIndex: Int, cycle: Int, packetLength: Int): Unit = {
    val NoCRouterModel = routerModelMap((x, y))
    NoCRouterModel.setPath(src, dst, msgIndex, cycle, packetLength)
  }

  /** Remove a hop of a message (packet) in router (x,y) at from "src" direction to "dst" direction at "cycle" to ("cycle" + "packetLength").
   *
   * @param x            the x index
   * @param y            the y index
   * @param src          the source direction
   * @param dst          the destination direction
   * @param msgIndex     the index of the message
   * @param cycle        the cycle when the first flit of the packet go through this router
   * @param packetLength the length of the packet
   *
   */
  def ripUp(x: Int, y: Int, src: Int, dst: Int, msgIndex: Int, cycle: Int, packetLength: Int): Unit = {
    val NoCRouterModel = routerModelMap((x, y))
    NoCRouterModel.ripUp(src, dst, msgIndex, cycle, packetLength)
  }

  /** Create this packet-switched network model.
   *
   * @param channelSize     the number of channels
   * @param xSize           the x size of this network
   * @param ySize           the y size of this network
   * @param congestionLimit the maximum channel usage proportion in spite of the real injection cycle
   *
   */
  def this(channelSize: Int, xSize: Int, ySize: Int, congestionLimit: Double) {
    this()
    this.channelSize = channelSize
    this.xSize = xSize
    this.ySize = ySize
    this.congestionLimit = congestionLimit
    init()
  }

  /** Initialize this network model.
   *
   * TODO: introduce more topologies like fat-tree and ring
   */
  def init(): Unit = {
    for (x <- 0 until xSize) {
      for (y <- 0 until ySize) {
        val routerModel = new NoCRouterModel(x, y, channelSize, xSize, ySize, congestionLimit)
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
        }
      }
    }
    maxAdjacency = 5

  }
}


/** The model of a packet-switched router.
 */
class NoCRouterModel {
  var x = 0
  var y = 0
  var adjacency = Array[Int]()
  var channelSize = 0
  var congestionLimit = 0.0

  var deadlockPrevented = false

  def tilePort = -1

  var directionCongestionMap = scala.collection.mutable.Map[Int, scala.collection.mutable.Map[Int, Double]]()
  val routingInfo = new ArrayBuffer[(Int, Int, Int)]()


  /** Clear this router model.
   */
  def clear: Unit = {
    directionCongestionMap = scala.collection.mutable.Map[Int, scala.collection.mutable.Map[Int, Double]]()
    for (direction <- adjacency) {
      directionCongestionMap.put(direction, scala.collection.mutable.Map[Int, Double]())
    }
    routingInfo.clear()
  }

  /** Find feasible directions for next hop.
   * If "deadlockPrevented" is true, the odd-even turn will be applied.
   *
   * @param srcDirection   the source direction of current hop
   * @param cycle          the cycle of current hop
   * @param congestionRate the maximum channel usage proportion in spite of the real injection cycle
   */
  def findUnimpededDirection(srcDirection: Int, cycle: Int, congestionRate: Double = congestionLimit):
  scala.collection.mutable.Map[Int, scala.collection.mutable.Map[Int, Double]] = {
    var ret = directionCongestionMap.filter(i => i._1 != srcDirection)
      .filter(i => {
        if (i._2.contains(cycle)) {
          i._2(cycle)
        } else {
          0.0
        }
      } < congestionRate)
    if (deadlockPrevented) {
      //odd-even model
      if (y % 2 == 1) {
        if (srcDirection == Parameters.S || srcDirection == Parameters.N) {
          ret = ret.filter(item => item._1 != Parameters.W)
        }
      } else {
        if (srcDirection == Parameters.E) {
          ret = ret.filter(item => item._1 == Parameters.E)
        }
      }
    }
    ret
  }


  /** Find the port index of a direction. (not used in fact)
   *
   * @param direction the direction in utils.Parameters (e.g. E = 0)
   * @return the port index
   */
  def findPortIndex(direction: Int) = adjacency.indexOf(direction)


  /** Initialize this router model.
   *
   * @param x                 the x index
   * @param y                 the y index
   * @param channelSize       the channel number
   * @param xSize             the x size of the network
   * @param ySize             the y size of the network
   * @param congestionLimit   the maximum channel usage proportion in spite of the real injection cycle
   * @param deadlockPrevented indicate whether applying the odd-even turn to prevent dead lock
   */
  def this(x: Int, y: Int, channelSize: Int, xSize: Int = Parameters.xSize,
           ySize: Int = Parameters.ySize, congestionLimit: Double, deadlockPrevented: Boolean = false) = {
    this()
    this.x = x
    this.y = y
    this.congestionLimit = congestionLimit
    this.deadlockPrevented = deadlockPrevented

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

    for (direction <- adjacency) {
      directionCongestionMap.put(direction, scala.collection.mutable.Map[Int, Double]())
    }

    this.channelSize = channelSize
  }

  /** Set a hop of a message (packet) in this router at from "src" direction to "dst" direction at "cycle" to ("cycle" + "packetLength").
   *
   * @param srcDirection the source direction
   * @param dstDirection the destination direction
   * @param msgIndex     the index of the message
   * @param cycle        the cycle when the first flit of the packet go through this router
   * @param packetLength the length of the packet
   *
   */
  def setPath(srcDirection: Int, dstDirection: Int, msgIndex: Int, cycle: Int, packetLength: Int) = {
    for (c <- cycle until cycle + packetLength) {
      if (directionCongestionMap(dstDirection).contains(c)) {
        directionCongestionMap(dstDirection)(c) = 1.0 / channelSize + directionCongestionMap(dstDirection)(c)
        if (directionCongestionMap(dstDirection)(c) > congestionLimit) {
          throw new Exception("Exceed congestion limit!" + srcDirection + " " + dstDirection + " " + msgIndex)
        }
      } else {
        directionCongestionMap(dstDirection).put(c, 1.0 / channelSize)
      }
    }
    routingInfo.append((srcDirection, dstDirection, msgIndex))

  }

  /** Remove a hop of a message (packet) in this router at from "src" direction to "dst" direction at "cycle" to ("cycle" + "packetLength").
   *
   * @param srcDirection the source direction
   * @param dstDirection the destination direction
   * @param msgIndex     the index of the message
   * @param cycle        the cycle when the first flit of the packet go through this router
   * @param packetLength the length of the packet
   *
   */
  def ripUp(srcDirection: Int, dstDirection: Int, msgIndex: Int, cycle: Int, packetLength: Int): Unit = {
    if (routingInfo.contains((srcDirection, dstDirection, msgIndex))) {
      for (c <- cycle until cycle + packetLength) {
        directionCongestionMap(dstDirection)(c) = -1.0 / channelSize + directionCongestionMap(dstDirection)(c)
      }
      routingInfo.remove(routingInfo.indexOf((srcDirection, dstDirection, msgIndex)))
    } else {
      throw new Exception("Invalid path!" + srcDirection + " " + dstDirection + " " + msgIndex)
    }
  }

}
