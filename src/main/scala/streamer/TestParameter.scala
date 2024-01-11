package streamer

import chisel3._
import chisel3.util._

/* the meaning of these testing parameters can be found at Parameter.scala */

object TemporalAdressGenUnitTestParameters {
  def temporalLoopDim = 3
  def temporalLoopBoundWidth = 8
  def addrWidth = 32

}

object UnrollingAddrGenUintTestParameters {
  def unrollingFactor = Seq(8, 8)
  def unrollingDim = unrollingFactor.length
  def addrWidth = 32

}

object DataReaderTestParameters {
  def dataReaderTcdmPorts = 8
  def tcdmDataWidth = 64
  def unrollingFactor = Seq(8, 8)
  def addrWidth = 32
  def fifoWidth = 512
  def elementWidth = 8

  def unrollingDim = unrollingFactor.length

}

object DataWriterTestParameters {
  def dataWriterTcdmPorts = 32
  def tcdmDataWidth = 64
  def unrollingFactor = Seq(8, 8)
  def addrWidth = 32
  def fifoWidth = 2048
  def elementWidth = 32

  def unrollingDim = unrollingFactor.length

}

object StreamerTestConstant {
  def MacScalingFactor = 4

  def fifoWidthReader = Seq(64 * MacScalingFactor, 64 * MacScalingFactor, 64)
  def fifoDepthReader = Seq(2, 2, 2)

  def fifoWidthWriter = Seq(64 * MacScalingFactor)
  def fifoDepthWriter = Seq(2)

  def dataReaderNum = 3
  def dataWriterNum = 1
  def dataReaderTcdmPorts = Seq(1 * MacScalingFactor, 1 * MacScalingFactor, 1)
  def dataWriterTcdmPorts = Seq(1 * MacScalingFactor)
  def readElementWidth = Seq(32, 32, 32)
  def writeElementWidth = Seq(32)

  def tcdmDataWidth = 64

  def unrollingFactorReader =
    Seq(Seq(2 * MacScalingFactor), Seq(2 * MacScalingFactor), Seq(2))
  def unrollingFactorWriter = Seq(Seq(2 * MacScalingFactor))

  def temporalLoopDim = 1
  def temporalLoopBoundWidth = 8

  def addrWidth = 32

  def stationarity = Seq(0, 0, 1, 1)

  // inferenced parameters
  def dataMoverNum = dataReaderNum + dataWriterNum
  def tcdmPortsNum = dataReaderTcdmPorts.sum + dataWriterTcdmPorts.sum
  def unrollingDimReader = (0 until unrollingFactorReader.length).map(i =>
    unrollingFactorReader(i).length
  )
  def unrollingDimWriter = (0 until unrollingFactorWriter.length).map(i =>
    unrollingFactorWriter(i).length
  )

  def unrollingDim: Seq[Int] = (0 until unrollingFactorReader.length).map(i =>
    unrollingFactorReader(i).length
  ) ++ (0 until unrollingFactorWriter.length).map(i =>
    unrollingFactorWriter(i).length
  )

}
