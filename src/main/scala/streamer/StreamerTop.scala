package streamer

import chisel3._
import chisel3.util._

class StreamerTopCsrIO(addrWidth: Int) extends Bundle {

  val req = Flipped(Decoupled(new CsrReq(addrWidth)))
  val rsp = Decoupled(new CsrRsp)

}

class StreamerTopIO(
    params: StreamerParams
) extends Bundle {

  val csr = new StreamerTopCsrIO(params.addrWidth)

  val data = new StreamerDataIO(
    params
  )
}

// add csr as well as read and write signals for streamer
class StreamerTop(
    params: StreamerParams = StreamerParams()
) extends Module
    with RequireAsyncReset {
  val io = IO(
    new StreamerTopIO(
      params
    )
  )

  // configuration csrs plus one config valid csr
  def csr_num =
    params.temporalLoopDim + params.dataMoverNum * params.temporalLoopDim + params.unrollingDim.sum + params.dataMoverNum + 1

  val csr_manager = Module(new CsrManager(csr_num, params.addrWidth))

  val streamer = Module(
    new Streamer(
      params
    )
  )

  streamer.io.csr.valid := csr_manager.io.csr_config_out.valid
  csr_manager.io.csr_config_out.ready := streamer.io.csr.ready

  csr_manager.io.csr_config_in <> io.csr

  // temporal loop bounds
  for (i <- 0 until params.temporalLoopDim) {
    streamer.io.csr.bits
      .loopBounds_i(i) := csr_manager.io.csr_config_out.bits(i)
  }

  // temporal loop strides
  for (i <- 0 until params.dataMoverNum) {
    for (j <- 0 until params.temporalLoopDim) {
      streamer.io.csr.bits
        .temporalStrides_csr_i(i)(j) := csr_manager.io.csr_config_out.bits(
        params.temporalLoopDim + i * params.temporalLoopDim + j
      )
    }
  }

  // spatial loop strides
  for (i <- 0 until params.unrollingDim.length) {
    for (j <- 0 until params.unrollingDim(i)) {
      streamer.io.csr.bits
        .spatialStrides_csr_i(i)(j) := csr_manager.io.csr_config_out.bits(
        params.temporalLoopDim + params.dataMoverNum * params.temporalLoopDim + i * params
          .unrollingDim(
            i
          ) + j
      )
    }
  }

  // base ptrs
  for (i <- 0 until params.dataMoverNum) {
    streamer.io.csr.bits.ptr_i(i) := csr_manager.io.csr_config_out.bits(
      params.temporalLoopDim + params.dataMoverNum * params.temporalLoopDim + params.unrollingDim.sum + i
    )
  }

  io.data <> streamer.io.data

}

// Scala main function for generating test streamerTop system verilog file
object StreamerTopTester extends App {
  emitVerilog(
    new StreamerTop(StreamerParams()),
    Array("--target-dir", "generated/streamertop/tester")
  )
}
