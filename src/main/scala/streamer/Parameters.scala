package streamer

import chisel3._
import chisel3.util._

/** This class represents some common parameters used in several modules
  * @param addrWidth
  *   The bit width of the address.
  * @param tcdmDataWidth
  *   data width for each TCDm port
  */
trait CommonParams {

  def addrWidth = 32
  def tcdmDataWidth = 64

}

trait CommonWideParams {
  def addrWidth = 32
  def tcdmDataWidth = 512
}

object CommonParams {
  val addrWidth = 32
  val tcdmDataWidth = 64
}

object CommonWideParams {
  val addrWidth = 32
  val tcdmDataWidth = 512
}

/** This class represents all the parameters for the Temporal Address Generation
  * Unit.
  * @param loopDim
  *   The dimension of the temporal loops = the number of for loops.
  * @param loopBoundWidth
  *   The bit width of the loop bounds.
  * @param addrWidth
  *   The bit width of the address.
  */
case class TemporalAddrGenUnitParams(
    loopDim: Int = TemporalAddrGenUnitTestParameters.loopDim,
    loopBoundWidth: Int = TemporalAddrGenUnitTestParameters.loopBoundWidth,
    addrWidth: Int = TemporalAddrGenUnitTestParameters.addrWidth
)

/** This class represents all the parameters for the Spatial Address Generation
  * Unit.
  * @param loopDim
  *   The number of nested for loops.
  * @param loopBounds
  *   The bounds of each loop dimension.
  * @param addrWidth
  *   The bit width of the address.
  */
case class SpatialAddrGenUnitParams(
    loopDim: Int = SpatialAddrGenUnitTestParameters.loopDim,
    loopBounds: Seq[Int] = SpatialAddrGenUnitTestParameters.loopBounds,
    addrWidth: Int = SpatialAddrGenUnitTestParameters.addrWidth
)

/** This class represents all the parameters for the Data Mover (including Data
  * Reader and Data Writer).
  *
  * @param tcdmPortsNum
  *   the number of TCDM ports connected to each data mover
  * @param spatialBounds
  *   spatial unrolling factors (your parfor) for each data mover
  * @param spatialDim
  *   the dimension of spatial unrolling factors (your parfor) for each data
  *   mover
  * @param elementWidth
  *   single data element width for each data mover, useful for generating
  *   spatial addresses
  * @param fifoWidth
  *   FIFO width
  */
case class DataMoverParams(
    tcdmPortsNum: Int = DataMoverTestParameters.tcdmPortsNum,
    spatialBounds: Seq[Int] = DataMoverTestParameters.spatialBounds,
    spatialDim: Int = DataMoverTestParameters.spatialDim,
    elementWidth: Int = DataMoverTestParameters.elementWidth,
    fifoWidth: Int = DataMoverTestParameters.fifoWidth,
    addrWidth: Int = CommonParams.addrWidth,
    tcdmDataWidth: Int = CommonParams.tcdmDataWidth
)

/** FIFO parameters
  *
  * @param width
  *   the width of the FIFO
  * @param depth
  *   the depth of the FIFO
  */
case class FIFOParams(width: Int, depth: Int)

/** trait for Streamer core parameters
  * @param temporalAddrGenUnitParams
  *   a parameters case class instantiation for temporal address generation unit
  * @param stationarity
  *   accelerator stationarity feature for each data mover (data reader and data
  *   writer)
  * @param dataReaderParams
  *   a seq of case class DataMoverParams instantiation for the Data Readers
  * @param dataWriterParams
  *   a seq of case class DataMoverParams instantiation for the Data Writers
  * @param fifoReaderParams
  *   a seq of case class FIFOParams instantiation for the FIFO connected to
  *   Data Readers
  * @param fifoReaderParams
  *   a seq of case class FIFOParams instantiation for the FIFO connected to
  *   Data Writers
  */
trait HasStreamerCoreParams {

  val temporalAddrGenUnitParams: TemporalAddrGenUnitParams

  val stationarity: Seq[Int]

  val dataReaderParams: Seq[DataMoverParams]
  val dataWriterParams: Seq[DataMoverParams]

  val fifoReaderParams: Seq[FIFOParams]
  val fifoWriterParams: Seq[FIFOParams]

}

/** trait for Streamer inferred parameters
  * @param temporalDim
  *   the dimension of the temporal loop
  * @param temporalBoundWidth
  *   the register width for storing the temporal loop bound
  * @param spatialDim
  *   a Seq contains the spatial dimensions for both data reader and data writer
  * @param tcdmDataWidth
  *   data width for each TCDm port
  * @param addrWidth
  *   the address width
  * @param dataReaderNum
  *   number of data readers
  * @param dataWriterNum
  *   number of data writers
  * @param dataMoverNum
  *   the number of data movers (including data reader and writer)
  * @param dataReaderTcdmPorts
  *   a Seq contains the number of TCDM ports connected to each data reader
  * @param dataWriterTcdmPorts
  *   a Seq contains the number of TCDM ports connected to each data writer
  * @param tcdmPortsNum
  *   the total number of TCDM ports connected the data movers (including data
  *   reader and writer)
  * @param fifoWidthReader
  *   FIFO width for the data readers
  * @param fifoWidthWriter
  *   FIFO width for the data writers
  */
trait HasStreamerInferredParams extends HasStreamerCoreParams {

  val temporalDim: Int = temporalAddrGenUnitParams.loopDim
  val temporalBoundWidth: Int = temporalAddrGenUnitParams.loopBoundWidth

  val spatialDim: Seq[Int] =
    dataReaderParams.map(_.spatialDim) ++ dataWriterParams.map(_.spatialDim)

  val tcdmDataWidth: Int = dataReaderParams(0).tcdmDataWidth
  val addrWidth: Int = temporalAddrGenUnitParams.addrWidth

  val dataReaderNum: Int = dataReaderParams.length
  val dataWriterNum: Int = dataWriterParams.length
  val dataMoverNum: Int = dataReaderNum + dataWriterNum
  val dataReaderTcdmPorts: Seq[Int] = dataReaderParams.map(_.tcdmPortsNum)
  val dataWriterTcdmPorts: Seq[Int] = dataWriterParams.map(_.tcdmPortsNum)
  val tcdmPortsNum: Int = dataReaderTcdmPorts.sum + dataWriterTcdmPorts.sum

  val fifoWidthReader: Seq[Int] = fifoReaderParams.map(_.width)
  val fifoWidthWriter: Seq[Int] = fifoWriterParams.map(_.width)

}

/** This case class represents all the parameters for the Streamer
  * @param temporalAddrGenUnitParams
  * @param stationarity
  * @param dataReaderParams
  * @param dataWriterParams
  * @param fifoReaderParams
  * @param fifoWriterParams
  *   the meaning of these parameters can be found at the top of this file the
  *   default value of these parameters is from the StreamerTestConstant object
  */
case class StreamerParams(
    temporalAddrGenUnitParams: TemporalAddrGenUnitParams =
      StreamerTestConstant.temporalAddrGenUnitParams,
    stationarity: Seq[Int] = StreamerTestConstant.stationarity,
    dataReaderParams: Seq[DataMoverParams] =
      StreamerTestConstant.dataReaderParams,
    dataWriterParams: Seq[DataMoverParams] =
      StreamerTestConstant.dataWriterParams,
    fifoReaderParams: Seq[FIFOParams] = StreamerTestConstant.fifoReaderParams,
    fifoWriterParams: Seq[FIFOParams] = StreamerTestConstant.fifoWriterParams
) extends HasStreamerCoreParams
    with HasStreamerInferredParams
