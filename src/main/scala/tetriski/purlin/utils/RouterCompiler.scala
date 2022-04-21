package tetriski.purlin.utils

import java.io.{File, FileWriter, PrintWriter}
import java.util.Date

import chisel3.iotesters
import play.api.libs.json.{JsObject, JsValue, Json}
import tetriski.purlin.NoC.{MeshNoC, MeshNoCInjection, MultiChannelRouter}
import tetriski.purlin.SwitchBox.{MeshSwitchBox, RoutingResultTester}
import tetriski.purlin.utils.AlgorithmType.AlgorithmType

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.sys.process._

/** A enumeration of algorithms
 */
object AlgorithmType extends Enumeration {
  type AlgorithmType = Value
  val pathFinder, minimalDis, minimalCongestion, estimation, estimationRipUp, XY, random = Value
}

object RouterCompiler extends App {

  //    compareNetworks()
//  exploreInjectionRate()
  //  exploreParameters()


  /** Compare packet-switched and circuit-switched 2-channel 4x4 networks.
   * NOTE: "onceInjection" should be true when generate random tasks.
   */
  def compareNetworks(): Unit = {
    var array = Array.ofDim[Double](2, 5, 5)
    val packetLength = 8
    val packetNumForEachEndpoint = 64
    val onceInjection = true
    val model = new MeshModel(2, 4, 4)
    val underTestAlgorithm = Array(AlgorithmType.XY)

    for (i <- 0 until 5) {
      genRandomTask(0.5 + 0.1 * i, model, packetLength, onceInjection, packetNumForEachEndpoint, false)
      testNoCAlgorithm(0.5 + 0.1 * i, 2, 4, 4,
        packetLength, onceInjection, underTestAlgorithm, false)
      for (channelSize <- 2 until 4) {
        for (fs <- 4 until 9) {
          val ret = testSBAlgorithm(i, fs, channelSize)
          array(channelSize - 2)(fs - 4)(i) = ret
        }
      }
    }
    for (i <- 0 until 5) {
      val outputFile = new FileWriter("SBAvgResults.txt", true)
      for (channelSize <- 2 until 4) {
        for (fs <- 4 until 9) {
          val successNum = array(channelSize - 2)(fs - 4).filter(i => i > 0).size
          val avgLetency = array(channelSize - 2)(fs - 4).filter(i => i > 0).sum / successNum
          outputFile.write("injectionRate: " + (0.5 + 0.1 * i) + ", channel: " + channelSize + ", Fs: " + fs
            + ", successNum: " + successNum + ", avgLetency: " + avgLetency + "\n")
        }
      }
      outputFile.close()
    }


  }

  /** Explore a 2-channel 4x4 packet-switched distributed routing network under different injection rate.
   */
  def exploreInjectionRate(): Unit = {
    val packetLength = 3
    val packetNumForEachEndpoint = 64
    val size = 10
    val onceInjection = false
    val model = new MeshModel(2, size, size)
    val underTestAlgorithm = Array(AlgorithmType.XY)

    for (i <- 1 until 20) {
      val injectionRate = 0.01 * i
      genRandomTask(injectionRate, model, packetLength, onceInjection, packetNumForEachEndpoint, true)
      testNoCAlgorithm(injectionRate, 2, size, size, packetLength,
        onceInjection, underTestAlgorithm, false)
    }
  }

  /** Explore suitable parameters and injection rate where other algorithms perform better than XY-routing.
   */
  def exploreParameters(): Unit = {
    var flag = false
    var i = 2
    var count = 3

    val underTestAlgorithm = Array(AlgorithmType.XY,
      AlgorithmType.minimalCongestion, AlgorithmType.pathFinder,
      AlgorithmType.estimation)
    val packetLength = 8
    val packetNumForEachEndpoint = 64
    val onceInjection = false
    val model = new MeshModel(Parameters.channelSize, Parameters.xSize, Parameters.ySize)

    while (i <= 12) {
      flag = false
      val injectionRate = 0.01 * i
      Parameters.congestionFactor = count * 0.05
      genRandomTask(injectionRate, model, packetLength, onceInjection, packetNumForEachEndpoint, false)
      testNoCAlgorithm(injectionRate, 2, 4, 4, packetLength, onceInjection, underTestAlgorithm)
      val fileName = "PurlinTest/" + injectionRate + "-" +
        Parameters.xSize + "x" + Parameters.ySize + "-" + Parameters.channelSize + "-NoCTestingResults.txt"
      val result = io.Source.fromFile(fileName).getLines()
      val lines = result.toArray
      flag = false
      val res = lines(2).split(" ").filter(s => s != "")
      val xyLatency = res(res.size - 2).toDouble
      for (j <- 3 until 6) {
        val res = lines(j).split(" ").filter(s => s != "")
        val latency = res(res.size - 2).toDouble
        if (latency < xyLatency) {
          println("########### count = " + count + " #########")
          flag = true
        }
      }
      if (flag || count >= 100) {
        i += 1
        count = 0
      } else {
        count += 1
      }

    }
  }

  /** Run an algorithm for 4x4 circuit switched network and then simulate.
   *
   * @param i           injection rate offset
   * @param Fs          the number of possible connections offered to each output channel
   * @param channelSize the channel number
   */
  def testSBAlgorithm(i: Int, Fs: Int, channelSize: Int): Double = {
    //    val Fs = 8
    //    val channelSize = 2
    Parameters.channelSize = channelSize
    val model = new MeshSBModel(channelSize, 4, 4, Fs)
    //    genRandomTask(0.8, model)
    var start = new Date().getTime()
    val result = greedyRouting(readJson("PurlinTest/" + (i * 0.1 + 0.5).toString + "-4x4-2-globalRouting.json"), model)
    var end = new Date().getTime()
    writeJson(result, "globalRoutingResult.json")

    val avgLatency = if (result.messages(0).routingStrategy.getOrElse(List()).size == 0) {
      -1
    } else {
      result.messages.map(m => m.packetLength.getOrElse(0)
        + m.routingStrategy.getOrElse(List()).size - 1).sum / result.messages.size.toDouble
    }

    val outputFile = new FileWriter("SBTestingResults.txt", true)
    outputFile.write("xSize: " + Parameters.xSize + ", ySize: " + Parameters.ySize +
      ", channelSize: " + Parameters.channelSize + ", Fs: " + Fs + ", avgLatency: " +
      avgLatency + ", Time: " + (end - start).toString + ", injectionRate: " + (i * 0.1 + 0.5).toString + "\n")
    outputFile.flush()
    outputFile.close()

    val network = () => new MeshSwitchBox(model, 16)
    if (avgLatency == -1) {
      return avgLatency
    }
//    iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), network) {
//      c => new RoutingResultTester(c, model, result)
//    }

    avgLatency
  }

  /** Run algorithms for a packet switched network and then simulate.
   *
   * @param injectionRate      the injection rate
   * @param channelSize        the channel number
   * @param xSize              the x size
   * @param ySize              the y size of network
   * @param packetLength       the length of each packet
   * @param onceInjection      indicating whether only inject packets at cycle 0
   * @param underTestAlgorithm some under test algorithms
   * @param sourceRouting      indicate whether the network use source routing
   *
   */
  def testNoCAlgorithm(injectionRate: Double, channelSize: Int, xSize: Int, ySize: Int,
                       packetLength: Int, onceInjection: Boolean,
                       underTestAlgorithm: Array[AlgorithmType], sourceRouting: Boolean = true): Unit = {

    /** Simulate for the result (XY, Random only).
     *
     * @param injectionRate the injection rate
     * @param network       the network module
     * @param algorithm     an under test algorithm
     *
     */
    def runTester(network: () => MeshNoC, algorithm: AlgorithmType, injectionRate: Double,
                  onceInjection: Boolean): Unit = {
      val outputFile = new FileWriter("NoCTestingResults.txt", true)
      outputFile.write("%-20s%-16s%-16s".format(algorithm.toString, "-", "-"))
      outputFile.flush()
      outputFile.close()
//      iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), network) {
//        c => new MeshNoCInjection(c, algorithm, readJson(), injectionRate, onceInjection)
//      }
    }

    /** Simulate for the result of one algorithm.
     *
     * @param injectionRate the injection rate
     * @param model         the network model
     * @param network       the network module
     * @param algorithm     an under test algorithm
     *
     */
    def runTesterWithAlgorithm(model: MeshNoCModel, network: () => MeshNoC,
                               algorithm: AlgorithmType, injectionRate: Double,
                               onceInjection: Boolean): Unit = {
      val outputFile = new FileWriter("NoCTestingResults.txt", true)
      var start = new Date().getTime()
      val result = routingForNoC(readJson(), model, algorithm)
      var end = new Date().getTime()
      outputFile.write("%-20s%-16s%-16s".format(algorithm.toString,
        (end - start).toString, model.estimateAll().formatted("%.3f")))
      outputFile.flush()
      outputFile.close()
      writeJson(result, algorithm.toString + "-globalRoutingResult.json")
//      iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), network) {
//        c => new MeshNoCInjection(c, algorithm, result, injectionRate, onceInjection)
//      }
    }

    Parameters.channelSize = channelSize
    Parameters.xSize = xSize
    Parameters.ySize = ySize
    Parameters.retrench()
    if (!sourceRouting) {
      Parameters.abandonSourceRouting()
    }
    val model = new MeshNoCModel(Parameters.channelSize, Parameters.xSize, Parameters.ySize, 100.0)

    val network = () => new MeshNoC((y, x) => new MultiChannelRouter(y, x, false), () => new MultiChannelPacket)

    val outputFile = new FileWriter("NoCTestingResults.txt")
    outputFile.write("xSize: " + Parameters.xSize + ", ySize: " + Parameters.ySize +
      ", channelSize: " + Parameters.channelSize + ", injectionRate: " + injectionRate +
      ", packetLength: " + packetLength + "\n")
    outputFile.write("%-20s%-16s%-16s%-16s%-16s%-16s%-16s%-16s%-16s\n"
      .format("Algorithm", "Time", "Estimate", "Expected", "Received", "Minimal", "Average", "Network", "Packet"))
    outputFile.flush()
    outputFile.close()

    underTestAlgorithm.foreach(algorithm => algorithm match {
      case AlgorithmType.XY => runTester(network, algorithm, injectionRate, onceInjection)
      case AlgorithmType.random => runTester(network, algorithm, injectionRate, onceInjection)
      case AlgorithmType.minimalDis => runTesterWithAlgorithm(model, network, algorithm, injectionRate, onceInjection)
      case AlgorithmType.minimalCongestion =>
        runTesterWithAlgorithm(model, network, algorithm, injectionRate, onceInjection)
      case AlgorithmType.pathFinder =>
        runTesterWithAlgorithm(model, network, algorithm, injectionRate, onceInjection)
      case AlgorithmType.estimation =>
        runTesterWithAlgorithm(model, network, algorithm, injectionRate, onceInjection)
      case AlgorithmType.estimationRipUp =>
        runTesterWithAlgorithm(model, network, algorithm, injectionRate, onceInjection)
      case _ => None
    })

    val move = "cp NoCTestingResults.txt PurlinTest/" + injectionRate + "-" +
      Parameters.xSize + "x" + Parameters.ySize + "-" + Parameters.channelSize + "-NoCTestingResults.txt"
    val run = move !

    val move2 = "cp globalRouting.json PurlinTest/" + injectionRate + "-" +
      Parameters.xSize + "x" + Parameters.ySize + "-" + Parameters.channelSize + "-globalRouting.json"
    val run2 = move2 !

    val move3 = "cp latencyDistribution.txt PurlinTest/" + injectionRate + "-" +
      Parameters.xSize + "x" + Parameters.ySize + "-" + Parameters.channelSize + "-latencyDistribution.txt"
    val run3 = move3 !
  }

  /** Routing algorithm for packet-switched networks.
   *
   * @param globalRouting the routing tasks
   * @param model         the packet-switched network model
   * @param algorithm     the type of algorithm
   * @return the routing strategies
   *
   */
  def routingForNoC(globalRouting: GlobalRouting, model: MeshNoCModel, algorithm: AlgorithmType): GlobalRouting = {
    model.clear

    var priority = mutable.Map[List[RoutingStrategy], Double]()
    var history = Map[(Int, Int, Int), Double]()
    for (x <- 0 until model.xSize) {
      for (y <- 0 until model.ySize) {
        for (direction <- -1 until 4) {
          history += (x, y, direction) -> 0
        }
      }
    }
    /** A factor for historical congestion (pathFinder) algorithm
     */
    val historicalPunish = 0.1
    val congestionLowerBound = 1
    val messageBuffer = new ArrayBuffer[Message]()
    val maxHops = Parameters.log2Routing / 2

    /** Update priority of a new path into the queue.
     *
     * @param strategies    the original path
     * @param next          the next hop
     * @param newStrategies the new path
     * @param algorithm     the type of algorithm
     * @param message       the routing message (packet)
     */
    def updatePriority(strategies: List[RoutingStrategy], next: RoutingStrategy, newStrategies: List[RoutingStrategy],
                       algorithm: AlgorithmType, message: Message = Message(0, 0, 0, 0, None, None, None)): Unit = {
      val x = next.routerX.getOrElse(-1)
      val y = next.routerY.getOrElse(-1)
      val direction = next.dstDirection
      val hopNum = strategies.size
      val towards = (x, y, direction)
      priority.put(newStrategies, {
        algorithm match {
          case AlgorithmType.pathFinder =>
            -(hopNum + 1 +
              Parameters.congestionFactor * (1 + history(towards))
                * model.getCongestionLevel(x, y, direction, message, hopNum))
          case AlgorithmType.minimalDis => -(hopNum + 1)
          case AlgorithmType.minimalCongestion => -(2 * Parameters.congestionFactor *
            model.getCongestionLevel(x, y, direction, message)
            + Math.abs(priority(strategies)) + 1)
          case AlgorithmType.estimation => -model.estimate(message, inputStrategies = newStrategies)
          case AlgorithmType.estimationRipUp => -model.estimate(message, inputStrategies = newStrategies)
          case _ => 0.0
        }
      })
    }

    /** Find a feasible path for the routing message (packet).
     *
     * @param message the routing message (packet)
     */
    def findPathForMessage(message: Message): Unit = {
      val messageIndex = messageBuffer.size
      priority.clear()
      //      priority = Map[List[RoutingStrategy], Double]()
      val source = (message.srcX, message.srcY)
      val start = model.routerModelMap(source)

      val sink = (message.dstX, message.dstY)
      val end = model.routerModelMap(sink)

      //BFS
      val queue = mutable.PriorityQueue[List[RoutingStrategy]]()(Ordering.by(priority.apply))
      val visited = scala.collection.mutable.Set[(NoCRouterModel, Int)]()
      visited.add((start, Parameters.TILE))
      priority += List() -> 0.0

      val initDst = start.findUnimpededDirection(Parameters.TILE, message.injectionCycle.getOrElse(0))
      for (dst <- initDst) {
        val next = model.gotoNextRouter(start, dst._1)
        if (!visited.contains(next)) {
          val strategy = RoutingStrategy(Option(source._1), Option(source._2), Option(Parameters.TILE),
            None, dst._1, None)
          val strategies = List(strategy)
          updatePriority(List(), strategy, strategies, algorithm, message)
          queue.enqueue(strategies)
          visited.add(next)
        }
      }

      var flag = true
      while (!queue.isEmpty && flag) {
        val strategies = queue.dequeue()
        val lastStrategy = strategies.last
        val routerX = lastStrategy.routerX.getOrElse(-1)
        val routerY = lastStrategy.routerY.getOrElse(-1)
        val dstDirection = lastStrategy.dstDirection
        val sourceRouter = model.routerModelMap(routerX, routerY)

        val current = model.gotoNextRouter(sourceRouter, dstDirection)
        val currentRouter = current._1
        val currentPort = current._2

        val initDst = currentRouter.findUnimpededDirection(currentPort, message.injectionCycle.getOrElse(0) + strategies.size)
        for (dst <- initDst) {
          if (dst._1 == Parameters.TILE) {
            if (currentRouter == end) {
              flag = false
              val strategy = RoutingStrategy(Option(currentRouter.x), Option(currentRouter.y),
                Option(currentPort), None, dst._1, None)
              val newMessage = Message(message.srcX, message.srcY, message.dstX, message.dstY,
                message.injectionCycle, message.packetLength, Option(strategies ::: List(strategy)))
              messageBuffer.append(newMessage)
              println("Find path from: (" + source._1
                + ", " + source._2 + ") to (" + sink._1 + ", " + sink._2 + ").")
              model.allocateMessage(newMessage, messageIndex)
            }
          } else {
            val next = model.gotoNextRouter(currentRouter, dst._1)
            if (!visited.contains(next)) {
              val strategy = RoutingStrategy(Option(currentRouter.x), Option(currentRouter.y), Option(currentPort),
                None, dst._1, None)
              val newStrategies = strategies ::: List(strategy)
              if (newStrategies.size <= maxHops) {
                updatePriority(strategies, strategy, newStrategies, algorithm, message)
                queue.enqueue(newStrategies)
                visited.add(next)
              }
            }
          }
        }
      }

      if (queue.isEmpty) {
        throw new RuntimeException("Cannot find path from: (" + source._1
          + ", " + source._2 + ") to (" + sink._1 + ", " + sink._2 + ").")
      }
    }

    /** Update history of congestion.
     */
    def updateHistory(): Unit = {
      for (x <- 0 until model.xSize) {
        for (y <- 0 until model.ySize) {
          val NoCRouterModel = model.routerModelMap(x, y)
          for (item <- NoCRouterModel.directionCongestionMap) {
            val maxCongestion = if (item._2.size > 0) {
              item._2.map(c => c._2).max
            } else {
              0
            }
            history += (x, y, item._1) -> {
              if (maxCongestion > congestionLowerBound) {
                (maxCongestion - congestionLowerBound) * historicalPunish
              } else {
                0.0
              }
            }
          }
        }
      }
    }

    updateHistory()
    for (message <- globalRouting.messages) {
      findPathForMessage(message)
    }

    if (algorithm == AlgorithmType.pathFinder || algorithm == AlgorithmType.estimationRipUp) {
      val iteratorNum = 10
      for (i <- 0 until iteratorNum) {
        println("##############Rip-Up iteration: " + i + " ##############")
        updateHistory()
        val tmpMessageBuffer = messageBuffer.clone
        messageBuffer.clear()
        for (messageIndex <- 0 until tmpMessageBuffer.size) {
          val message = tmpMessageBuffer(messageIndex)
          model.ripUP(message, messageIndex)
          findPathForMessage(message)
        }
      }
    }

    GlobalRouting(messageBuffer.toList)
  }


  /** Routing algorithm (mininal distance) for circuit-switched networks.
   *
   * @param globalRouting the routing tasks
   * @param model         the circuit-switched network model
   * @return the routing strategies
   *
   */
  def greedyRouting(globalRouting: GlobalRouting, model: MeshSBModel): GlobalRouting = {
    model.clearPath
    var sourcePortUsedMap = Map[(Int, Int), Int]()
    var sinkPortUsedMap = Map[(Int, Int), Int]()

    val messageBuffer = new ArrayBuffer[Message]()
    for (message <- globalRouting.messages) {
      val source = (message.srcX, message.srcY)
      val sourceChannel = sourcePortUsedMap.getOrElse(source, 0)
      sourcePortUsedMap += source -> (sourceChannel + 1)
      val start = model.routerModelMap(source)

      val sink = (message.dstX, message.dstY)
      val sinkChannel = sinkPortUsedMap.getOrElse(sink, 0)
      sinkPortUsedMap += sink -> (sinkChannel + 1)
      val end = model.routerModelMap(sink)

      //BFS
      val queue = scala.collection.mutable.Queue[List[RoutingStrategy]]()
      val visited = scala.collection.mutable.Set[(SBModel, (Int, Int))]()
      visited.add(start, (Parameters.TILE, sourceChannel))

      val initDst = start.findPath((Parameters.TILE, sourceChannel))
      for (dst <- initDst) {
        val next = model.gotoNextRouter(start, dst)
        if (!visited.contains(next)) {
          val strategy = RoutingStrategy(Option(source._1), Option(source._2), Option(Parameters.TILE),
            Option(sourceChannel), dst._1, Option(dst._2))
          queue.enqueue(List(strategy))
          visited.add(next)
        }
      }


      var flag = true
      while (!queue.isEmpty && flag) {
        val strategies = queue.dequeue()
        val lastStrategy = strategies.last
        val routerX = lastStrategy.routerX.getOrElse(-1)
        val routerY = lastStrategy.routerY.getOrElse(-1)
        val dstChannel = lastStrategy.dstChannel.getOrElse(-1)
        val dstDirection = lastStrategy.dstDirection
        val sourceRouter = model.routerModelMap(routerX, routerY)

        val current = model.gotoNextRouter(sourceRouter, (dstDirection, dstChannel))
        val currentRouter = current._1
        val currentPort = current._2

        val initDst = currentRouter.findPath(currentPort)
        for (dst <- initDst) {
          if (dst._1 == Parameters.TILE) {
            if (currentRouter == end && dst._2 == sinkChannel) {
              flag = false
              val strategy = RoutingStrategy(Option(currentRouter.x), Option(currentRouter.y),
                Option(currentPort._1), Option(currentPort._2), dst._1, Option(dst._2))
              val newMessage = Message(message.srcX, message.srcY, message.dstX, message.dstY,
                message.injectionCycle, message.packetLength, Option(strategies ::: List(strategy)))
              messageBuffer.append(newMessage)
              println("Find path from: (" + source._1
                + ", " + source._2 + ") to (" + sink._1 + ", " + sink._2 + ").")
              for (s <- (strategies ::: List(strategy))) {
                model.setPath(s.routerX.getOrElse(-1), s.routerY.getOrElse(-1),
                  (s.srcDirection.getOrElse(-1), s.srcChannel.getOrElse(-1)),
                  (s.dstDirection, s.dstChannel.getOrElse(-1)))
              }
            }
          } else {
            val next = model.gotoNextRouter(currentRouter, dst)
            if (!visited.contains(next)) {
              val strategy = RoutingStrategy(Option(currentRouter.x), Option(currentRouter.y), Option(currentPort._1),
                Option(currentPort._2), dst._1, Option(dst._2))
              queue.enqueue(strategies ::: List(strategy))
              visited.add(next)
            }
          }
        }
      }

      if (queue.isEmpty) {
        return globalRouting
        throw new RuntimeException("Cannot find path from: (" + source._1
          + ", " + source._2 + ") to (" + sink._1 + ", " + sink._2 + ").")
      }
    }

    GlobalRouting(messageBuffer.toList)
  }

  /** Read the routing tasks or strategies from a JSON file.
   *
   * @param filename the name of th JSON file
   * */
  def readJson(filename: String = "globalRouting.json") = {
    val in = Source.fromFile(filename).getLines().reduce(_ + _)
    val json = Json.parse(in)
    GlobalRouting.read(json)
  }

  /** Write the routing tasks or strategies as a JSON file.
   *
   * @param globalRouting routing tasks or strategies
   * @param filename      the name of th JSON file
   * */
  def writeJson(globalRouting: GlobalRouting, filename: String = "globalRouting.json"): Unit = {
    val json = GlobalRouting.write(globalRouting)
    val out = Json.prettyPrint(json)
    val writer = new PrintWriter(new File(filename))
    writer.write(out)
    writer.close()
  }

  /** Generate tasks for random injection test and save as "globalRouting.json".
   *
   * @param injectionRatio the injection ratio from endpoint (tile) for each router
   * @param model          model of mesh network
   * @param packetLength   the length of generated packets
   * @param onceInjection  indicating whether only inject packets at cycle 0
   * @param packetNum      number of packets injected to each router, only used when onceInjection == false
   * @param randomPLength  indicating whether generating packets with different length
   * */
  def genRandomTask(injectionRatio: Double, model: MeshModel, packetLength: Int = 0,
                    onceInjection: Boolean = true, packetNum: Int = 0, randomPLength: Boolean = false) = {
    var messages = List[Message]()

    if (onceInjection) {
      //only inject packets at cycle 0
      //balanced injection for each endpoint
      val channelSize = model.channelSize
      var srcPortUsedMap = Map[(Int, Int), Int]()
      var dstPortUsedMap = Map[(Int, Int), Int]()
      val availablePortNum: Int = (model.xSize * model.ySize * injectionRatio).toInt
      for (i <- 0 until availablePortNum) {
        var srcX = 0
        var srcY = 0
        var srcPortUsed = channelSize
        while (srcPortUsed >= channelSize) {
          srcX = scala.util.Random.nextInt(model.xSize)
          srcY = scala.util.Random.nextInt(model.ySize)
          srcPortUsed = srcPortUsedMap.getOrElse((srcX, srcY), 0)
        }
        srcPortUsedMap += (srcX, srcY) -> (srcPortUsedMap.getOrElse((srcX, srcY), 0) + 1)

        var dstX = scala.util.Random.nextInt(model.xSize)
        var dstY = scala.util.Random.nextInt(model.ySize)
        var dstPortUsed = channelSize
        while (dstPortUsed >= channelSize || ((dstX == srcX) && (dstY == srcY))) {
          dstX = scala.util.Random.nextInt(model.xSize)
          dstY = scala.util.Random.nextInt(model.ySize)
          dstPortUsed = dstPortUsedMap.getOrElse((dstX, dstY), 0)
        }
        dstPortUsedMap += (dstX, dstY) -> (dstPortUsedMap.getOrElse((dstX, dstY), 0) + 1)

        val pLength = if (randomPLength && packetLength > 0) {
          scala.util.Random.nextInt(packetLength - 1) + 1
        } else {
          packetLength
        }
        val message = Message(srcX, srcY, dstX, dstY, Option(0), Option(pLength), None)
        messages ::= message
      }
    } else {
      //inject "packetNum" packets to each port
      for (i <- 0 until model.xSize) {
        for (j <- 0 until model.ySize) {
          var injectionCycle = 0
          for (p <- 0 until packetNum) {
            //at each cycle, each endpoint have "injectionRatio"% chances to inject packets
            while (scala.util.Random.nextInt(100) >= (injectionRatio * 100)) {
              injectionCycle += 1
            }

            var dstX = scala.util.Random.nextInt(model.xSize)
            var dstY = scala.util.Random.nextInt(model.ySize)
            while ((dstX == i) && (dstY == j)) {
              dstX = scala.util.Random.nextInt(model.xSize)
              dstY = scala.util.Random.nextInt(model.ySize)
            }

            val pLength = if (randomPLength && packetLength > 0) {
              scala.util.Random.nextInt(packetLength - 1) + 1
            } else {
              packetLength
            }

            val message = Message(i, j, dstX, dstY, Option(injectionCycle), Option(pLength), None)
            messages ::= message
          }
        }
      }
    }


    val globalRouting = GlobalRouting(messages)
    writeJson(globalRouting)
  }


}

/** Explore a 2-channel 4x4 packet-switched distributed routing network under different injection rate.
 */
object testInjectionRate extends App {
  RouterCompiler.exploreInjectionRate()
}

/** Compare packet-switched and circuit-switched 2-channel 4x4 networks.
 */
object testNetworkCompare extends App {
  RouterCompiler.compareNetworks()
}

/** Explore suitable parameters and injection rate where other algorithms perform better than XY-routing.
 */
object testAlgorithms extends App {
  RouterCompiler.exploreParameters()
}

/** Generate a default 4x4 2-channel distributed routing packet-switched network with 32-bit payload.
 */
object genDistributedRouting extends App {
  Parameters.retrench()
  Parameters.abandonSourceRouting()
  val network = () => new MeshNoC((y, x) => new MultiChannelRouter(y, x), () => new MultiChannelPacket)
//  chisel3.Driver.execute(Array("-td", "RTL/"), network)
}

object FVTester extends App {
  Parameters.retrench()
  Parameters.abandonSourceRouting()
  Parameters.payloadSize = 1
  Parameters.xSize = 2
  Parameters.ySize = 2
  val network = () => new MeshNoC((y, x) => new MultiChannelRouter(y, x), () => new MultiChannelPacket)
//  chisel3.Driver.execute(Array("-td", "RTL/"), network)
  val systemVerilog = chisel3.stage.ChiselStage.emitSystemVerilog(network.apply())

  val systemVerilogWriter = new FileWriter(new File(".", "inout.sv"))
  systemVerilogWriter write systemVerilog
  systemVerilogWriter.close()
}

/** Generate a default 4x4 2-channel source routing packet-switched network with 32-bit payload.
 */
object genSourceRouting extends App {
  Parameters.retrench()
  val network = () => new MeshNoC((y, x) => new MultiChannelRouter(y, x), () => new MultiChannelPacket)
//  chisel3.Driver.execute(Array("-td", "RTL/"), network)
}

/** Generate a default 4x4 2-channel circuit-switched network (Fs = 8) with 32-bit payload.
 */
object genCircuitSwitched extends App {
  val model = new MeshSBModel(2, 4, 4, 8)
  val network = () => new MeshSwitchBox(model, 32)
//  chisel3.Driver.execute(Array("-td", "RTL/"), network)
}

