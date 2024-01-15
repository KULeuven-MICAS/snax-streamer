package streamer

import chisel3._
import chisel3.util._

// streamer parameters for the GEMM Accelerator
object GeMMStreamerParameters extends CommonParams {

  def temporalAddrGenUnitParams: TemporalAddrGenUnitParams =
    TemporalAddrGenUnitParams(
      loopDim = 3,
      loopBoundWidth = 8,
      addrWidth
    )

  def fifoReaderParams: Seq[FIFOParams] = Seq(
    FIFOParams(512, 2),
    FIFOParams(512, 2)
  )

  def fifoWriterParams: Seq[FIFOParams] = Seq(FIFOParams(2048, 2))

  def dataReaderParams: Seq[DataMoverParams] = Seq(
    DataMoverParams(
      tcdmPortsNum = 8,
      spatialBounds = Seq(8, 8),
      spatialDim = 2,
      elementWidth = 8,
      fifoWidth = fifoReaderParams(0).width
    ),
    DataMoverParams(
      tcdmPortsNum = 8,
      spatialBounds = Seq(8, 8),
      spatialDim = 2,
      elementWidth = 8,
      fifoWidth = fifoReaderParams(1).width
    )
  )

  def dataWriterParams: Seq[DataMoverParams] = Seq(
    DataMoverParams(
      tcdmPortsNum = 32,
      spatialBounds = Seq(8, 8),
      spatialDim = 2,
      elementWidth = 32,
      fifoWidth = fifoWriterParams(0).width
    )
  )

  def stationarity = Seq(0, 0, 1)

}

// streamer parameters for the Post-processing SIMD Accelerator
object PostProcessingStreamerParameters extends CommonParams {

  def temporalAddrGenUnitParams: TemporalAddrGenUnitParams =
    TemporalAddrGenUnitParams(
      loopDim = 1,
      loopBoundWidth = 8,
      addrWidth
    )

  def fifoReaderParams: Seq[FIFOParams] = Seq(
    FIFOParams(2048, 2)
  )

  def fifoWriterParams: Seq[FIFOParams] = Seq(FIFOParams(512, 2))

  def dataReaderParams: Seq[DataMoverParams] = Seq(
    DataMoverParams(
      tcdmPortsNum = 32,
      spatialBounds = Seq(64),
      spatialDim = 1,
      elementWidth = 32,
      fifoWidth = fifoReaderParams(0).width
    )
  )

  def dataWriterParams: Seq[DataMoverParams] = Seq(
    DataMoverParams(
      tcdmPortsNum = 8,
      spatialBounds = Seq(64),
      spatialDim = 1,
      elementWidth = 8,
      fifoWidth = fifoWriterParams(0).width
    )
  )

  def stationarity = Seq(0, 0)

}