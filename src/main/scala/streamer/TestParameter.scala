package streamer

import chisel3._
import chisel3.util._

/* the meaning of these testing parameters can be found at Parameter.scala */

object TemporalAddrGenUnitTestParameters {
  def loopDim = 3
  def loopBoundWidth = 8
  def addrWidth = 32

}

object SpatialAddrGenUnitTestParameters {
  def loopBounds = Seq(8, 8)
  def loopDim = loopBounds.length
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

  def fifoWidthReader = Seq(512, 512)
  def fifoDepthReader = Seq(4, 4)

  def fifoWidthWriter = Seq(2048)
  def fifoDepthWriter = Seq(4)

  def dataReaderNum = 2
  def dataWriterNum = 1
  def dataReaderTcdmPorts = Seq(8, 8)
  def dataWriterTcdmPorts = Seq(32)
  def readElementWidth = Seq(8, 8)
  def writeElementWidth = Seq(32)

  def tcdmDataWidth = 64

  def unrollingFactorReader = Seq(Seq(8, 8), Seq(8, 8))

  def unrollingFactorWriter = Seq(Seq(8, 8))

  def temporalLoopDim = 3
  def temporalLoopBoundWidth = 8

  def addrWidth = 32

  def stationarity = Seq(0, 0, 1)

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
