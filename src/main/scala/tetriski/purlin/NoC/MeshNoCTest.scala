package tetriski.purlin.NoC

import java.io.{File, FileWriter}

import chisel3.iotesters.PeekPokeTester
import tetriski.purlin.utils.AlgorithmType.AlgorithmType
import tetriski.purlin.utils.{AlgorithmType, GlobalRouting, MiniPacket, MultiChannelPacket, Parameters}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/** A test object example of mesh packet-switched networks.
 */
object MeshNoCTest extends App {
  //  val router = () => new MeshNoC((y, x) => new SimpleRouter(y, x), () => new Packet)
  //  //  chisel3.Driver.execute(Array("-td", "tutorial/RTL/"), router)
  //
  //  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), router) {
  //    c => new MeshNoCTester(c)
  //  }

  Parameters.useMultiChannelRouter = true
  Parameters.retrench()
  //  Parameters.abandonSourceRouting()

  //  val router = () => new MultiChannelRouter(1, 1)
//  for (routersPerDim <- 2 until 7) {
//    Parameters.xSize = routersPerDim
//    Parameters.ySize = routersPerDim
//    val multiChannelRouter = () => new MeshNoC((y, x) =>
//      new MultiChannelRouter(y, x, true), () => new MultiChannelPacket)
////    chisel3.Driver.execute(Array("-td", "PurlinResult/RTL/NoC-sourceRouting-F/"), multiChannelRouter)
//  }

//  val multiChannelRouter = () => new MeshNoC((y, x) => new MultiChannelRouter(y, x, true),
//    () => new MultiChannelPacket)
  //  chisel3.Driver.execute(Array("-td", "tutorial/RTL/"),
  //    () => new NoCWrapperForPR((y, x) => new MultiChannelRouter(y, x), () => new MultiChannelPacket))
  //  chisel3.Driver.execute(Array("-td", "tutorial/RTL/"), router)


  Parameters.USE_VC_HBT = true
  val VCMesh = () => new MeshNoC((y,x) => new VCRouter(y, x), () => new MiniPacket())
  chisel3.Driver.execute(Array("-td", "tutorial/RTL/"), VCMesh)
  //  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), multiChannelRouter) {
  //    c => new MeshMCNoCTester(c)
  //  }
  //    iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), multiChannelRouter) {
  //      c => new MeshNoCInjection(c)
  //    }
}

/** A tester example of the simple mesh packet-switched networks.
 */
class MeshNoCTester(mesh: MeshNoC) extends PeekPokeTester(mesh) {
  poke(mesh.io.en, true)
  poke(mesh.io.enqFromTiles(1)(1).valid, true)
  poke(mesh.io.enqFromTiles(1)(1).bits, Map("header.src.x" -> BigInt(1),
    "header.src.y" -> BigInt(1), "header.dst.x" -> BigInt(1), "header.dst.y" -> BigInt(1)))

  for (i <- 0 until 20) {
    println("Cycle: " + i)
    for (y <- 0 until Parameters.ySize) {
      for (x <- 0 until Parameters.xSize) {
        poke(mesh.io.deqToTiles(y)(x).ready, true)
        poke(mesh.io.enqFromTiles(y)(x).bits, Map("payload" -> BigInt(x + y + i)))
        print(peek(mesh.io.deqToTiles(y)(x).bits)("payload").toString() + "\t")
      }
      print("\n")
    }
    step(1)
  }

}


/** An important tester of the multi-channel mesh packet-switched networks.
 * It is used by the router comipler.
 *
 * @param mesh          the network module
 * @param algorithm     an under test algorithm
 * @param globalRouting the routing tasks or strategies
 * @param injectionRate the injection rate
 * @param onceInjection indicating whether only inject packets at cycle 0
 */
class MeshNoCInjection(mesh: MeshNoC, algorithm: AlgorithmType = AlgorithmType.XY,
                       globalRouting: GlobalRouting = new GlobalRouting(List()),
                       injectionRate: Double = 0.5, onceInjection: Boolean = true) extends PeekPokeTester(mesh) {
  def getRandomArray[T](source: Array[T], num: Int): ArrayBuffer[T] = {
    val res = new ArrayBuffer[T]()
    val oriSize = source.length
    var indexSet = Set[Int]()
    while (indexSet.size < num) {
      indexSet += scala.util.Random.nextInt(oriSize)
    }
    for (index <- indexSet) {
      res.append(source(index))
    }
    res
  }

  def dumpLatency(networkLatency: Seq[Int], step: Int, max: Int,
                  fileName: String = "latencyDistribution.txt"): Unit = {
    val file = new File(fileName)
    val exist = file.exists()

    val end = max / step
    val outputFile = new FileWriter(fileName, true)
    if (!exist) {
      outputFile.write("%-20s".format("Algorithm"))
      for (index <- 0 until end - 1) {
        outputFile.write("%-20s".format("[" + (index * step).toString + ", " + ((index + 1) * step).toString + ")"))
      }
      outputFile.write("%-20s".format("[" + ((end - 1) * step).toString + ", max)"))
      outputFile.write("\n")
    }


    outputFile.write("%-20s".format(algorithm.toString))
    for (index <- 0 until end - 1) {
      val num = networkLatency.filter(i => (i >= index * step) && (i < (index + 1) * step)).size
      outputFile.write("%-20s".format(num.toString))
    }
    val num = networkLatency.filter(i => (i >= (end - 1) * step)).size
    outputFile.write("%-20s".format(num.toString))
    outputFile.write("\n")

    outputFile.flush()
    outputFile.close()
  }

  val xSize = Parameters.xSize
  val ySize = Parameters.ySize
  val channelSize = Parameters.channelSize

  val portNum = xSize * ySize * channelSize
  val portArray = new ArrayBuffer[(Int, Int)]()
  var existPacketFromTile = scala.collection.mutable.Map[(Int, Int), ArrayBuffer[Int]]()
  var routingStrategy = new ArrayBuffer[BigInt]()

  for (x <- 0 until xSize) {
    for (y <- 0 until ySize) {
      existPacketFromTile += (x, y) -> (new ArrayBuffer[Int]())
      for (c <- 0 until channelSize) {
        portArray.append((x, y))
      }
    }
  }


  var injectionNum = (portNum * injectionRate).toInt
  var cycleUpper = 100


  var srcArray = new ArrayBuffer[(Int, Int)]()
  var dstArray = new ArrayBuffer[(Int, Int)]()
  var defaultPacketLength = 10
  var defaultInjectionCycle = 0
  var packetLengths = (0 until injectionNum).map(_ => defaultPacketLength)

  if (globalRouting.messages.length != 0) {
    for (message <- globalRouting.messages) {
      srcArray.append((message.srcX, message.srcY))
      dstArray.append((message.dstX, message.dstY))
    }

    injectionNum = globalRouting.messages.length
    cycleUpper = Math.max(globalRouting.messages.map(item =>
      item.injectionCycle.getOrElse(defaultInjectionCycle)).max * 3, 2000)

    defaultPacketLength = globalRouting.messages(0).packetLength.getOrElse(defaultPacketLength)
    packetLengths = (0 until injectionNum).map(i =>
      globalRouting.messages(i).packetLength.getOrElse(defaultPacketLength))
  } else {
    srcArray = getRandomArray(portArray.toArray, injectionNum)
    dstArray = srcArray.map(i => {
      var x = scala.util.Random.nextInt(xSize)
      var y = scala.util.Random.nextInt(ySize)
      while (x == i._1) {
        x = scala.util.Random.nextInt(xSize)
      }
      while (y == i._2) {
        y = scala.util.Random.nextInt(ySize)
      }
      (x, y)
    }
    )
  }


  val minimalDis = new ArrayBuffer[Int]()

  for (i <- 0 until srcArray.size) {
    val xSrc = srcArray(i)._1
    val ySrc = srcArray(i)._2

    val xDst = dstArray(i)._1
    val yDst = dstArray(i)._2

    val xDis = xDst - xSrc
    val yDis = yDst - ySrc

    for (j <- 0 until packetLengths(i)) {
      minimalDis.append(Math.abs(xDis) + Math.abs(yDis))
    }

    val directionArray = new ArrayBuffer[String]()
    if (xDis > 0) {
      for (j <- 0 until xDis) {
        directionArray.append("E")
      }
    } else {
      for (j <- 0 until -xDis) {
        directionArray.append("W")
      }
    }

    if (yDis > 0) {
      for (j <- 0 until yDis) {
        directionArray.append("S")
      }
    } else {
      for (j <- 0 until -yDis) {
        directionArray.append("N")
      }
    }

    val routing = algorithm match {
      case AlgorithmType.XY => directionArray.map(d => Parameters.intDirection(d)).reduce((l, r) => (l << 2) + r)
      case AlgorithmType.DyXY => directionArray.map(d => Parameters.intDirection(d)).reduce((l, r) => (l << 2) + r)
      case AlgorithmType.WestFirst => directionArray.map(d => Parameters.intDirection(d)).reduce((l, r) => (l << 2) + r)
      case AlgorithmType.ModifiedWestFirst => directionArray.map(d => Parameters.intDirection(d)).reduce((l, r) => (l << 2) + r)
      case AlgorithmType.random => Random.shuffle(directionArray)
        .map(d => Parameters.intDirection(d)).reduce((l, r) => (l << 2) + r)
      case _ => {
        //source routing
        val tmpArray = new ArrayBuffer[Int]()
        val message = globalRouting.messages(i)
        val strategies = message.routingStrategy.getOrElse(List())
        for (index <- 0 until strategies.size - 1) {
          val strategy = strategies(index)
          tmpArray.append(strategy.dstDirection)
        }
        tmpArray.reverse.reduce((l, r) => (l << 2) + r)
      }
    }
    //x-y routing
    //        val routing = directionArray.map(d => NoCParam.intDirection(d)).reduce((l, r) => (l << 2) + r)

    //random routing
    //val routing = Random.shuffle(directionArray).map(d => Parameters.intDirection(d)).reduce((l, r) => (l << 2) + r)

    routingStrategy.append(routing)

    existPacketFromTile((xSrc, ySrc)).append(i)
  }

  if (globalRouting.messages.size > 0) {
    for (x <- 0 until xSize)
      for (y <- 0 until ySize) {
        existPacketFromTile((x, y)) = existPacketFromTile((x, y)).sortBy(i =>
          globalRouting.messages(i).injectionCycle.getOrElse(defaultInjectionCycle))
      }
  }


  var sentMiniPackets = scala.collection.mutable.Map[(Int, Int), Int]()
  var currentPacketOrder = scala.collection.mutable.Map[(Int, Int), Int]()

  var expInjCycle = Map[Int, Int]()
  var originalCycle = Map[(Int, Int), Int]()
  var experiencedCycle = Map[(Int, Int), Int]()

  for (x <- 0 until xSize) {
    for (y <- 0 until ySize) {
      sentMiniPackets += (x, y) -> 0
      currentPacketOrder += (x, y) -> 0
    }
  }


  poke(mesh.io.en, true)

  step(defaultInjectionCycle)


  for (cycle <- defaultInjectionCycle until cycleUpper) {
    println("Cycle: " + cycle)

    //send packets
    for (x <- 0 until xSize) {
      for (y <- 0 until ySize) {
        val pArray = existPacketFromTile(x, y)

        poke(mesh.io.enqFromTiles(y)(x).valid, false)
        val enqBundle = mesh.io.enqFromTiles(y)(x).bits.asInstanceOf[MultiChannelPacket]
        if (onceInjection) {
          //all packets should have the same length in this test
          if (peek(mesh.io.enqFromTiles(y)(x).ready) == 1 && (sentMiniPackets((x, y)) < defaultPacketLength)) {
            poke(mesh.io.enqFromTiles(y)(x).valid, true)
            val validNum = pArray.size
            val packetID = sentMiniPackets((x, y))
            sentMiniPackets((x, y)) = 1 + sentMiniPackets((x, y))
            poke(enqBundle.validNum, validNum)
            for (p <- 0 until validNum) {
              val index = pArray(p)
              val xSrc = srcArray(index)._1
              val ySrc = srcArray(index)._2

              val xDst = dstArray(index)._1
              val yDst = dstArray(index)._2

              val routing = routingStrategy(index)

              val payload = index * 256 + packetID

              originalCycle += (index, packetID) -> cycle
              expInjCycle += index -> 0

              poke(enqBundle.packets(p).header.src.x, xSrc)
              poke(enqBundle.packets(p).header.src.y, ySrc)
              poke(enqBundle.packets(p).header.dst.x, xDst)
              poke(enqBundle.packets(p).header.dst.y, yDst)
              if (Parameters.sourceRouting) {
                poke(enqBundle.packets(p).header.routing, routing)
              }
              poke(enqBundle.packets(p).payload, payload)
            }
          }
        } else {
          //only inject 1 mini-packet to a router at the same cycle
          if (currentPacketOrder((x, y)) < pArray.size) {
            val validNum = 1
            val index = pArray(currentPacketOrder((x, y)))

            val packetLength = packetLengths(index)
            val injectionCycle = globalRouting.messages(index).injectionCycle.getOrElse(defaultInjectionCycle)

            expInjCycle += index -> injectionCycle

            if (peek(mesh.io.enqFromTiles(y)(x).ready) == 1 &&
              (sentMiniPackets((x, y)) < packetLength) && cycle >= injectionCycle) {
              poke(mesh.io.enqFromTiles(y)(x).valid, true)
              poke(enqBundle.validNum, validNum)
              val packetID = sentMiniPackets((x, y))
              sentMiniPackets((x, y)) = 1 + sentMiniPackets((x, y))

              val xSrc = srcArray(index)._1
              val ySrc = srcArray(index)._2

              val xDst = dstArray(index)._1
              val yDst = dstArray(index)._2

              val routing = routingStrategy(index)

              val payload = index * 256 + packetID

              originalCycle += (index, packetID) -> cycle

              poke(enqBundle.packets(0).header.src.x, xSrc)
              poke(enqBundle.packets(0).header.src.y, ySrc)
              poke(enqBundle.packets(0).header.dst.x, xDst)
              poke(enqBundle.packets(0).header.dst.y, yDst)
              if (Parameters.sourceRouting) {
                poke(enqBundle.packets(0).header.routing, routing)
              }
              poke(enqBundle.packets(0).payload, payload)

              //next mini-packet
              if (sentMiniPackets((x, y)) == packetLength) {
                currentPacketOrder((x, y)) += 1
                sentMiniPackets((x, y)) = 0
              }
            }


          }
        }
      }
    }



    //receive packets
    for (x <- 0 until xSize) {
      for (y <- 0 until ySize) {
        poke(mesh.io.deqToTiles(y)(x).ready, true)
        val bundle = mesh.io.deqToTiles(y)(x).bits.asInstanceOf[MultiChannelPacket]
        print(peek(bundle.validNum) + ": (")
        for (c <- 0 until channelSize) {
          val payload = peek(bundle.packets(c).payload)
          var printItem = payload.toString(16)
          if (c < channelSize - 1) {
            printItem += ", "
          }
          print(printItem)
          if (peek(bundle.validNum) > c) {
            val index = payload.toInt / 256
            val packetID = payload.toInt - index * 256
            //            val oriCycle = payload.toInt - index * 65536 * 64 - packetID * 65536
            val oriCycle = originalCycle((index, packetID))
            experiencedCycle += (index, packetID) -> (cycle - oriCycle)
            //            println("\n" + payload + ", " + index + ", " + packetID + ", " + (cycle - oriCycle))
          }
        }
        print(")\t")
      }
      print("\n")
    }

    step(1)
  }


  var flag = false
  var flitNum = 0
  for (i <- 0 until injectionNum) {
    if (globalRouting.messages.size == 0) {
      flitNum += defaultPacketLength
      for (j <- 0 until defaultPacketLength) {
        val key = (i, j)
        if (!experiencedCycle.contains(key)) {
          println(i + ", " + j + ", " + srcArray(i)._1 + ", " + srcArray(i)._2
            + ", " + dstArray(i)._1 + ", " + dstArray(i)._2)
          flag = true
        }
      }
    } else {
      val packetLength = packetLengths(i)
      flitNum += packetLength
      for (j <- 0 until packetLength) {
        val key = (i, j)
        if (!experiencedCycle.contains(key)) {
          println(i + ", " + j + ", " + srcArray(i)._1 + ", " + srcArray(i)._2
            + ", " + dstArray(i)._1 + ", " + dstArray(i)._2)
          flag = true
        }
      }
    }
  }

  if (flag) {
    val warningLog = "Maybe deadlock appeared." +
      " You could set NoCRouterModel.deadlockPrevented as true to prevent deadlock." + " Or maybe cycleUpper is too small."
    println(warningLog)
    val outputFile = new FileWriter("NoCTestingResults.txt", true)
    outputFile.write("DEADLOCK!\n" )
    outputFile.flush()
    outputFile.close()
  }else{
    val minimalFiltLatency = minimalDis.reduce(_ + _).toDouble / flitNum
    val realFlitLatency = experiencedCycle.map(item => item._2).reduce(_ + _)
      .toDouble / flitNum
    val networkLatencyDistribution = (0 until injectionNum).map(i =>
      experiencedCycle(i, packetLengths(i) - 1) + originalCycle(i, packetLengths(i) - 1) - originalCycle(i, 0))
    val networkLatency = networkLatencyDistribution.reduce(_ + _).toDouble / injectionNum
    val packetLatencyDistribution = (0 until injectionNum).map(i =>
      experiencedCycle(i, packetLengths(i) - 1) + originalCycle(i, packetLengths(i) - 1) - expInjCycle(i))
    val packetLatency = packetLatencyDistribution.reduce(_ + _).toDouble / injectionNum

    dumpLatency(networkLatencyDistribution, 4, 68)

    println("Expected received flits: " + flitNum +
      "\nReal received flits: " + experiencedCycle.size +
      "\nMinimal average flit latency: " + minimalFiltLatency +
      "\nReal average flit latency: " + realFlitLatency +
      "\nReal average network latency: " + networkLatency +
      "\nReal average packet latency: " + packetLatency
    )

    val outputFile = new FileWriter("NoCTestingResults.txt", true)
    outputFile.write("%-16s%-16s%-16s%-16s%-16s%-16s\n".format(flitNum, experiencedCycle.size,
      minimalFiltLatency.formatted("%.3f"), realFlitLatency.formatted("%.3f"),
      networkLatency.formatted("%.3f"), packetLatency.formatted("%.3f")))
    outputFile.flush()
    outputFile.close()

    val outputDetailFile = new FileWriter(algorithm.toString + "-DetailLatency.txt", true)
    for (latency <- networkLatencyDistribution) {
      outputDetailFile.write(latency + " ")
    }
    outputDetailFile.flush()
    outputDetailFile.close()
  }

}

/** A tester example of the multi-channel mesh packet-switched networks.
 */
class MeshMCNoCTester(mesh: MeshNoC) extends PeekPokeTester(mesh) {
  poke(mesh.io.en, true)
  poke(mesh.io.enqFromTiles(1)(1).valid, true)
  val enqBundle = mesh.io.enqFromTiles(1)(1).bits.asInstanceOf[MultiChannelPacket]
  poke(enqBundle.validNum, 2)
  poke(enqBundle.packets(0).header.src.x, 1)
  poke(enqBundle.packets(0).header.src.y, 1)
  poke(enqBundle.packets(0).header.dst.x, 1)
  poke(enqBundle.packets(0).header.dst.y, 1)
  poke(enqBundle.packets(1).header.src.x, 1)
  poke(enqBundle.packets(1).header.src.y, 1)
  poke(enqBundle.packets(1).header.dst.x, 0)
  poke(enqBundle.packets(1).header.dst.y, 0)
  val routing = Array("W", "N").reverse
    .map(d => Parameters.intDirection(d)).reduce((l, r) => (l << 2) + r)
  poke(enqBundle.packets(1).header.routing, routing)

  val enqBundle2 = mesh.io.enqFromTiles(2)(0).bits.asInstanceOf[MultiChannelPacket]
  poke(mesh.io.enqFromTiles(2)(0).valid, true)
  poke(enqBundle2.validNum, 1)
  poke(enqBundle2.packets(0).header.src.x, 0)
  poke(enqBundle2.packets(0).header.src.y, 2)
  poke(enqBundle2.packets(0).header.dst.x, 1)
  poke(enqBundle2.packets(0).header.dst.y, 1)
  val routing2 = Array("E", "N").reverse
    .map(d => Parameters.intDirection(d)).reduce((l, r) => (l << 2) + r)
  poke(enqBundle2.packets(0).header.routing, routing2)

  val enqBundle3 = mesh.io.enqFromTiles(3)(3).bits.asInstanceOf[MultiChannelPacket]
  poke(mesh.io.enqFromTiles(3)(3).valid, true)
  poke(enqBundle3.validNum, 1)
  poke(enqBundle3.packets(0).header.src.x, 3)
  poke(enqBundle3.packets(0).header.src.y, 3)
  poke(enqBundle3.packets(0).header.dst.x, 3)
  poke(enqBundle3.packets(0).header.dst.y, 3)

  //  val enqBundle4 = mesh.io.enqFromTiles(3)(1).bits.asInstanceOf[MultiChannelPacket]
  //  poke(mesh.io.enqFromTiles(3)(1).valid, true)
  //  poke(enqBundle4.validNum, 1)
  //  poke(enqBundle4.packets(0).header.src.x, 1)
  //  poke(enqBundle4.packets(0).header.src.y, 3)
  //  poke(enqBundle4.packets(0).header.dst.x, 1)
  //  poke(enqBundle4.packets(0).header.dst.y, 0)
  //  val routing4 =  Array("N", "N", "N", "N").reverse
  //    .map(d => NoCParam.intDirection(d)).reduce((l ,r) => (l << 2) + r)
  //  poke(enqBundle4.packets(0).header.routing, routing4)

  testRunning()

  def testRunning() = {
    for (i <- 0 until 30) {
      println("Cycle: " + i)
      for (y <- 0 until Parameters.ySize) {
        for (x <- 0 until Parameters.xSize) {
          poke(mesh.io.deqToTiles(y)(x).ready, true)
          if (i < 10) {
            poke(mesh.io.enqFromTiles(y)(x).bits.asInstanceOf[MultiChannelPacket].packets(0).payload,
              x + y * 10 + i * 100)
            poke(mesh.io.enqFromTiles(y)(x).bits.asInstanceOf[MultiChannelPacket].packets(1).payload,
              10 * (x + y * 10 + i * 100))
          } else {
            poke(mesh.io.enqFromTiles(3)(3).valid, false)
            poke(mesh.io.enqFromTiles(1)(1).valid, false)
            //            poke(mesh.io.enqFromTiles(3)(1).valid, false)
          }
          val bundle = mesh.io.deqToTiles(y)(x).bits.asInstanceOf[MultiChannelPacket]
          print(peek(bundle.validNum) + ": (")
          for (c <- 0 until Parameters.channelSize) {
            var printItem = peek(bundle.packets(c).payload).toString()
            if (c < Parameters.channelSize - 1) {
              printItem += ", "
            }
            print(printItem)
          }
          print(")\t")
        }
        print("\n")
      }
      step(1)
    }
  }


}


