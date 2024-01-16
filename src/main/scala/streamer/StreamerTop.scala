package streamer

import chisel3._
import chisel3.util._

/** This class represents the csr input and output ports of the streamer top
  * module
  *
  * @param csrAddrWidth
  *   csr registers address width
  */
class StreamerTopCsrIO(csrAddrWidth: Int) extends Bundle {

  val req = Flipped(Decoupled(new CsrReq(csrAddrWidth)))
  val rsp = Decoupled(new CsrRsp)

}

/** This class represents the input and output ports of the streamer top module
  *
  * @param params
  *   the parameters class instantiation for the streamer top module
  */
class StreamerTopIO(
    params: StreamerTopParams = new StreamerTopParams()
) extends Bundle {

  // ports for csr configuration
  val csr = new StreamerTopCsrIO(params.csrAddrWidth)

  // ports for data in and out
  val data = new StreamerDataIO(
    params
  )
}

/** This class represents the streamer top module which adds the csr registers
  * as well as csr read and write logic based on the streamer
  *
  * @param params
  *   the parameters class instantiation for the streamer top module
  */
class StreamerTop(
    params: StreamerTopParams = new StreamerTopParams()
) extends Module
    with RequireAsyncReset {
  val io = IO(
    new StreamerTopIO(
      params
    )
  )

  // csrManager instantiation
  val csr_manager = Module(new CsrManager(params))

  // streamer instantiation
  val streamer = Module(
    new Streamer(
      params
    )
  )

  // io.csr and csrManager input connection
  csr_manager.io.csr_config_in <> io.csr

  // connect the streamer and csrManager output
  // control signals
  streamer.io.csr.valid := csr_manager.io.csr_config_out.valid
  csr_manager.io.csr_config_out.ready := streamer.io.csr.ready

  // splitting csrManager data ports to the streamer components
  // temporal loop bounds
  for (i <- 0 until params.temporalDim) {
    streamer.io.csr.bits
      .loopBounds_i(i) := csr_manager.io.csr_config_out.bits(i)
  }

  // temporal loop strides
  for (i <- 0 until params.dataMoverNum) {
    for (j <- 0 until params.temporalDim) {
      streamer.io.csr.bits
        .temporalStrides_csr_i(i)(j) := csr_manager.io.csr_config_out.bits(
        params.temporalDim + i * params.temporalDim + j
      )
    }
  }

  // spatial loop strides
  for (i <- 0 until params.spatialDim.length) {
    for (j <- 0 until params.spatialDim(i)) {
      streamer.io.csr.bits
        .spatialStrides_csr_i(i)(j) := csr_manager.io.csr_config_out.bits(
        params.temporalDim + params.dataMoverNum * params.temporalDim + i * params
          .spatialDim(
            i
          ) + j
      )
    }
  }

  // base ptrs
  for (i <- 0 until params.dataMoverNum) {
    streamer.io.csr.bits.ptr_i(i) := csr_manager.io.csr_config_out.bits(
      params.temporalDim + params.dataMoverNum * params.temporalDim + params.spatialDim.sum + i
    )
  }

  // io.data and streamer data ports connection
  io.data <> streamer.io.data

}

// Scala main function for generating test streamerTop system verilog file
object StreamerTopTester extends App {
  emitVerilog(
    new StreamerTop(new StreamerTopParams()),
    Array("--target-dir", "generated/streamertop/tester")
  )
}
