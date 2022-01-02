package tetriski.purlin.NoC

import chisel3._
import chisel3.util.log2Ceil
import tetriski.purlin.utils.Parameters

/** An useful module performing filtering function.
 * Filtering function: {(flag, data)}.filer(item => item.flag == true).map(item => item.data)
 *
 * @param requestSize   the size of the original (flag, data) bundles
 * @param resourceLimit the limit number of available resources
 * @param dataWidth     the data width
 * @example If resourceLimit = 2, it means there are 2 available resources,
 *          the number of true signals of requests may be 0, 1 or 2.
 *          So it needs 2 bits to distinguish it.
 */
class Filter[T <: Data](requestSize: Int, resourceLimit: Int, dataWidth: Int) extends Module {
  val resWidth = log2Ceil(resourceLimit + 1)

  val io = IO(new Bundle {
    val signalRequests = Input(Vec(requestSize, Bool()))
    val dataRequests = Input(Vec(requestSize, UInt(dataWidth.W)))
    val validNum = Output(UInt(resWidth.W))
    val resources = Output(Vec(resourceLimit, UInt(dataWidth.W)))
  })

  io.resources.foreach(res => res := 0.U)

  val signalUInt = (0 until requestSize).map(_ => Wire(UInt(resWidth.W)))
  (0 until requestSize).foreach(i => signalUInt(i) := io.signalRequests(i))
  io.validNum := signalUInt.reduce(_ + _)

  val accumRequest = (0 until requestSize).map(_ => Wire(UInt(log2Ceil(resourceLimit + 1).W)))
  accumRequest.foreach(accum => accum := 0.U)
  accumRequest(0) := signalUInt(0)
  for (i <- 0 until requestSize - 1) {
    accumRequest(i + 1) := signalUInt(i + 1) + accumRequest(i)
  }

  when(accumRequest(0) =/= 0.U) {
    io.resources(0) := io.dataRequests(0)
  }

  for (i <- 0 until requestSize - 1) {
    when(accumRequest(i) =/= accumRequest(i + 1)) {
      when(accumRequest(i) < resourceLimit.U) {
        io.resources(accumRequest(i)) := io.dataRequests(i + 1)
      }
    }
  }
}