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
    params: StreamerParams = StreamerParams(),
    csrAddrWidth: Int
) extends Bundle {

  // ports for csr configuration
  val csr = new StreamerTopCsrIO(csrAddrWidth)

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
    params: StreamerParams = StreamerParams()
) extends Module
    with RequireAsyncReset {

  val csrNum: Int =
    params.temporalDim + params.dataMoverNum * params.temporalDim + params.spatialDim.sum + params.dataMoverNum + 1
  val csrAddrWidth: Int = log2Up(csrNum)

  val io = IO(
    new StreamerTopIO(
      params,
      csrNum
    )
  )

  // csrManager instantiation
  val csr_manager = Module(new CsrManager(csrNum, csrAddrWidth))

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
  // Total number of csr is temporalDim + dataMoverNum * temporalDim + spatialDim.sum + dataMoverNum + 1.

  // lowest temporalDim address (0 to temporalDim - 1) is for temporal loop bound.
  //  0 is for innermost loop, temporalDim - 1 is for outermost loop

  // Then address (temporalDim, temporalDim +  dataMoverNum * temporalDim) is for temporal strides.
  // lowest address (temporalDim, temporalDim + temporalDim) is for the first data reader, with address temporalDim for the innermost loop.
  // address (temporalDim + (dataMoverNum - 1) * temporalDim, temporalDim + dataMoverNum * temporalDim -1) is for the first data reader.

  // Then address (temporalDim +  dataMoverNum * temporalDim, temporalDim + dataMoverNum * temporalDim + spatialDim.sum - 1)  is for spatial loop strides. The order is the same as temporal strides.

  // Then address (temporalDim +  dataMoverNum * temporalDim, temporalDim + dataMoverNum * temporalDim + spatialDim.sum + dataMoverNum - 1) is for the base pointers for each data mover.
  // the lowest address for teh first data mover.

  // temporal loop bounds
  for (i <- 0 until params.temporalDim) {
    streamer.io.csr.bits
      .loopBounds_i(i) := csr_manager.io.csr_config_out.bits(i)
  }

  // Connect configuration registers for temporal loop strides
  for (i <- 0 until params.dataMoverNum) {
    for (j <- 0 until params.temporalDim) {
      streamer.io.csr.bits
        .temporalStrides_csr_i(i)(j) := csr_manager.io.csr_config_out.bits(
        params.temporalDim + i * params.temporalDim + j
      )
    }
  }

  // Connect configuration registers for spatial loop strides
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
    new StreamerTop(
      StreamerParams(
        temporalAddrGenUnitParams =
          StreamerTestConstant.temporalAddrGenUnitParams,
        fifoReaderParams = StreamerTestConstant.fifoReaderParams,
        fifoWriterParams = StreamerTestConstant.fifoWriterParams,
        stationarity = StreamerTestConstant.stationarity,
        dataReaderParams = StreamerTestConstant.dataReaderParams,
        dataWriterParams = StreamerTestConstant.dataWriterParams
      )
    ),
    Array("--target-dir", "generated/streamertop/tester")
  )
}

// streamertop for GEMM
object GeMMStreamerTop extends App {
  emitVerilog(
    new StreamerTop(
      StreamerParams(
        temporalAddrGenUnitParams =
          GeMMStreamerParameters.temporalAddrGenUnitParams,
        fifoReaderParams = GeMMStreamerParameters.fifoReaderParams,
        fifoWriterParams = GeMMStreamerParameters.fifoWriterParams,
        stationarity = GeMMStreamerParameters.stationarity,
        dataReaderParams = GeMMStreamerParameters.dataReaderParams,
        dataWriterParams = GeMMStreamerParameters.dataWriterParams
      )
    ),
    Array("--target-dir", "generated/streamertop/tester")
  )
}

// streamertop for PP-SIMD
object PostProcessingTopTester extends App {
  emitVerilog(
    new StreamerTop(
      StreamerParams(
        temporalAddrGenUnitParams =
          PostProcessingStreamerParameters.temporalAddrGenUnitParams,
        fifoReaderParams = PostProcessingStreamerParameters.fifoReaderParams,
        fifoWriterParams = PostProcessingStreamerParameters.fifoWriterParams,
        stationarity = PostProcessingStreamerParameters.stationarity,
        dataReaderParams = PostProcessingStreamerParameters.dataReaderParams,
        dataWriterParams = PostProcessingStreamerParameters.dataWriterParams
      )
    ),
    Array("--target-dir", "generated/streamertop/tester")
  )
}

// streamertop for MAC
object MacStreamerTop extends App {
  emitVerilog(
    new StreamerTop(
      StreamerParams(
        temporalAddrGenUnitParams =
          MacStreamerParameters.temporalAddrGenUnitParams,
        fifoReaderParams = MacStreamerParameters.fifoReaderParams,
        fifoWriterParams = MacStreamerParameters.fifoWriterParams,
        stationarity = MacStreamerParameters.stationarity,
        dataReaderParams = MacStreamerParameters.dataReaderParams,
        dataWriterParams = MacStreamerParameters.dataWriterParams
      )
    ),
    Array("--target-dir", "generated/streamertop/tester")
  )
}
