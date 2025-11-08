package purlin.NoC

import Chisel.Enum
import chisel3.util.log2Ceil
import chisel3.{Bool, _}
import purlin.utils.{AnalyzedPacket, FunctionType, MiniPacket, MultiChannelPacket, Parameters}

import scala.collection.mutable.ArrayBuffer

class VCRouter(y: Int, x: Int, betterFrequency: Boolean = false)
  extends Router(y, x, () => new MiniPacket) {

  assert(Parameters.grantNumLimit == 1, "Broadcast is not supported in this router.")
  val idle :: analyzed :: vcChannelAllocated :: hold :: packetComplete :: Nil = Enum(5)
  val tailFlit :: bodyFlit :: headFlit :: Nil = Enum(3)

  //Use state machine only if use head-body-tail packet
  var stateBuffer = (0 until size).map(i => (0 until Parameters.channelSize)
    .map(c => RegInit(idle)))
  var vcIdRegs = (0 until size).map(i => (0 until Parameters.channelSize)
    .map(c => RegInit(0.U(log2Ceil(Parameters.channelSize + 1).W))))
  var grantRegs = (0 until size).map(i => (0 until Parameters.channelSize)
    .map(c => RegInit(0.U(Parameters.getGrantWidth.W))))
  var  grantNumRegs = (0 until size).map(i => (0 until Parameters.channelSize)
    .map(c => RegInit(0.U(log2Ceil(Parameters.grantNumLimit + 1).W))))
  var channelReadyRegs = (0 until size).map(i => (0 until Parameters.channelSize)
    .map(c => RegInit(false.B)))



  //initialization
  for (i <- 0 until size) {

    val MCPacket = Wire(new MiniPacket)
    deqs(i).valid := false.B
    enqs(i).ready := false.B

    val packet = MCPacket
    packet.payload := 0.U
    packet.vcId := 0.U
    packet.flitType := tailFlit
    packet.header.routing := defaultRouting
    packet.header.src.x := defaultX
    packet.header.src.y := defaultY
    packet.header.dst.x := defaultX
    packet.header.dst.y := defaultY
    packet.header.srcPort := defaultP

    deqs(i).bits := MCPacket
  }

  val buffers = (0 until size).map(s => (0 until Parameters.channelSize)
    .map(c => Module(new FIFO(new MiniPacket, Parameters.fifoDep,
      "packetBuffer_" + s.toString + "_" + c.toString, betterFrequency))))

  val vcIds = (0 until size).map(i => {
    val enqBundle = enqs(i).bits.asInstanceOf[MiniPacket]
    enqBundle.vcId
  })


  val minStressOuts = (0 until size).map(i => (0 until Parameters.channelSize)
    .map(c => buffers(i)(c).io.stressOut).reduce((x, y) => Mux(x < y, x, y)))

  for (i <- 0 until size) {
    when(minStressOuts(i) <= (Parameters.fifoDep - 1).U) {
      enqs(i).ready := true.B
    }
    for (c <- 0 until Parameters.channelSize) {
      when(vcIds(i) === c.U && enqs(i).valid) {
        buffers(i)(c).io.enq <> enqs(i)
      }.otherwise {
        buffers(i)(c).io.enq <> DontCare
        buffers(i)(c).io.enq.valid := false.B
      }

      buffers(i)(c).io.deq.ready := false.B
    }
  }


  io.stressOut := (0 until size - 1).map(i => {
    val res = Wire(UInt(stressWidth.W))
    res := minStressOuts(i)
    res
  }).reduce(_ + _)

  for (i <- 0 until size - 1) {
    io.vcValidOut(i) := 0.U
    for (c <- 0 until Parameters.channelSize) {
      when(buffers(i)(c).io.stressOut === minStressOuts(i)) {
        io.vcValidOut(i) := c.U
      }
    }
  }


  val arbiterVC = (0 until size)
    .map(_ => Module(new ArbiterVC(Parameters.channelSize, stressWidth)))
  val arbiterSwitch = Module(new ArbiterSwitch(size, Parameters.grantRegion))

  val deqSeq = new ArrayBuffer[(UInt, UInt)]()
  for (i <- 0 until connectArray.size) {
    deqSeq.append(connectArray(i).U(Parameters.getRoutingRegionWidth.W) -> i.U(Parameters.getGrantWidth.W))
  }

  val validBroadcastArray = Array(Parameters.E, Parameters.W, Parameters.S, Parameters.N)
    .filter(i => connectArray.indexOf(i) >= 0)
  val broadcastArray = validBroadcastArray.map(i => (i, connectArray.indexOf(i)))

  for (i <- 0 until size) {

    arbiterSwitch.io.grants(i) := 0.U
    arbiterSwitch.io.states(i) := 0.U

    for (c <- 0 until Parameters.channelSize) {

      val analyzer = Module(new Analyzer(size, y, x, deqSeq.toArray, broadcastArray))
      for (tmp <- 0 until 4) {
        val stress = if (validBroadcastArray.indexOf(tmp) >= 0) {
          io.stressIn(validBroadcastArray.indexOf(tmp))
        } else {
          0.U
        }
        analyzer.io.stressIn(tmp) := stress
      }

      analyzer.io.packet := buffers(i)(c).io.deq.bits
      analyzer.io.valid := false.B
      analyzer.io.deqsReady := deqs.map(deq => deq.ready)

      var stateFlag = true.B
      val isHeadFlit = buffers(i)(c).io.deq.bits.flitType === headFlit
      stateFlag = (stateBuffer(i)(c) === idle) && isHeadFlit

      arbiterVC(i).io.priority(c) := 0.U
      arbiterVC(i).io.states(c) := 0.U

      def connectArbiterVC() {
        //Use stress as priority
        arbiterVC(i).io.priority(c) := buffers(i)(c).io.stressOut
        arbiterVC(i).io.states(c) := stateBuffer(i)(c)
      }

      def connectArbiterSwitch() {
        arbiterSwitch.io.grants(i) := grantRegs(i)(c)
        arbiterSwitch.io.states(i) := stateBuffer(i)(c)
      }

      def connectDeqs(): Unit = {
        //Broadcast is not supported in this router.
          when(grantNumRegs(i)(c) > 0.U) {
            val dire = grantRegs(i)(c)
            val deqWire = Wire(new MiniPacket())
            deqWire := buffers(i)(c).io.deq.bits
            io.deqs(dire).valid := true.B
            when(dire =/= (size - 1).U) {
              deqWire.vcId := vcIdRegs(i)(c)
            }
            io.deqs(dire).bits := deqWire

        }
        buffers(i)(c).io.deq.ready := true.B
      }

      when(buffers(i)(c).io.deq.valid && io.en) {
        analyzer.io.valid := true.B
        when(stateFlag) {
          stateBuffer(i)(c) := analyzed
          grantRegs(i)(c) := analyzer.io.analyzedPacket.grants(0)
          grantNumRegs(i)(c) := analyzer.io.analyzedPacket.grantNum
          channelReadyRegs(i)(c) := analyzer.io.channelReady
          vcIdRegs(i)(c) := io.vcValidIn(analyzer.io.analyzedPacket.grants(0))
        }
      }

      when(stateBuffer(i)(c) === analyzed && io.en) {
        when(channelReadyRegs(i)(c)) {
          connectArbiterVC()
          when(arbiterVC(i).io.winners(c)) {
            stateBuffer(i)(c) := vcChannelAllocated
            connectArbiterSwitch()
          }
        }.otherwise {
          analyzer.io.valid := true.B
          channelReadyRegs(i)(c) := analyzer.io.channelReady
          vcIdRegs(i)(c) := io.vcValidIn(analyzer.io.analyzedPacket.grants(0))
        }
      }

      when(stateBuffer(i)(c) === vcChannelAllocated && io.en) {
        connectArbiterVC()
        connectArbiterSwitch()
        when(arbiterSwitch.io.winners(i)) {
          stateBuffer(i)(c) := hold
          connectDeqs()
        }.otherwise{
          stateBuffer(i)(c) := analyzed
        }
      }

      when(stateBuffer(i)(c) === hold && io.en) {
        connectArbiterVC()
        connectArbiterSwitch()
        connectDeqs()

        when(analyzer.io.packet.flitType === tailFlit) {
          stateBuffer(i)(c) := packetComplete
        }
      }

      when(stateBuffer(i)(c) === packetComplete && io.en) {
        connectArbiterVC()
        connectArbiterSwitch()
        stateBuffer(i)(c) := idle
        grantRegs(i)(c) := 0.U
        grantNumRegs(i)(c) := 0.U
        channelReadyRegs(i)(c) := 0.U
        vcIdRegs(i)(c) := 0.U
      }

    }

  }
}

class ArbiterVC(channelNum: Int, priorityWidth: Int) extends Module {
  val idle :: analyzed :: vcChannelAllocated :: hold :: packetComplete :: Nil = Enum(5)
  val stateWidth = 3
  val io = IO(new Bundle() {
    val priority = Input(Vec(channelNum, UInt(priorityWidth.W)))
    val states = Input(Vec(channelNum, UInt(stateWidth.W)))
    val winners = Output(Vec(channelNum, Bool()))
  })
  //Please make sure at most 1 channel has winner state.
  val hasWinner = (0 until channelNum)
    .map(c => io.states(c) === vcChannelAllocated || io.states(c) === hold
      || io.states(c) === packetComplete).reduce(_ | _)
  when(hasWinner) {
    for (c <- 0 until channelNum) {
      when(io.states(c) === vcChannelAllocated || io.states(c) === hold
        || io.states(c) === packetComplete) {
        io.winners(c) := true.B
      }.otherwise {
        io.winners(c) := false.B
      }
    }
  }.otherwise {
    val maxPriority = (0 until channelNum).map(c => {
      val res = Wire(UInt(priorityWidth.W))
      when(io.states(c) === analyzed) {
        res := io.priority(c)
      }.otherwise {
        res := 0.U
      }
      res
    }).reduce((x, y) => Mux(x > y, x, y))

    val maxIndex = Wire(UInt(log2Ceil(channelNum + 1).W))
    maxIndex := 0.U
    for (c <- 0 until channelNum) {
      when(io.priority(c) === maxPriority) {
        maxIndex := c.U
      }
    }

    for (c <- 0 until channelNum) {
      when(c.U === maxIndex) {
        io.winners(c) := true.B
      }.otherwise {
        io.winners(c) := false.B
      }
    }
  }
}

class ArbiterSwitch(size: Int, grantRegion: Int) extends Module {
  val idle :: analyzed :: vcChannelAllocated :: hold :: packetComplete :: Nil = Enum(5)
  val stateWidth = 3

  val grantWidth = log2Ceil(grantRegion)
  val outputResourceLimit = size

  val io = IO(new Bundle() {
    val grants = Input(Vec(size, UInt(grantWidth.W)))
    val states = Input(Vec(size, UInt(stateWidth.W)))
    val winners = Output(Vec(size, Bool()))
  })
  //Please make sure at most 1 channel has winner state.
  val preWinners = (0 until size)
    .map(c => io.states(c) === hold
      || io.states(c) === packetComplete)
  val hasWinner = preWinners.reduce(_ | _)
  val usedGrant = VecInit((0 until grantRegion).map(_ => false.B))

  val winners = (0 until size).map(_ => Wire(Bool()))
  winners.foreach(win => win := true.B)
  io.winners := winners

  when(hasWinner) {
    for (i <- 0 until size) {
      when(preWinners(i)) {
        winners(i) := true.B
        usedGrant(io.grants(i)) := true.B
      }
    }
  }

  val competitors = (0 until size)
    .map(c => io.states(c) === vcChannelAllocated)

  for (i <- 0 until size) {
    when(!preWinners(i)){
      when(competitors(i)){
        for (index <- 0 until i) {
          when(winners(index) && io.grants(index) === io.grants(i)) {
            winners(i) := false.B
          }
        }
        when(usedGrant(io.grants(i))){
          winners(i) := false.B
        }
      }.otherwise{
        winners(i) := false.B
      }
    }
  }

}