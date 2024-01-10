package streamer

import chisel3._
import chisel3.util._

/** Parameter definitions fifoWidthReader - FIFO width for the data readers
  * fifoDepthReader - FIFO depth for the data readers fifoWidthWriter - FIFO
  * width for the data writers fifoDepthWriter - FIFO depth for the data writers
  * dataReaderNum - number of data readers dataWriterNum - number of data
  * writers dataReaderTcdmPorts - the number of connections to TCDM ports for
  * each data reader dataWriterTcdmPorts - the number of connections to TCDM
  * ports for each data writer readElementWidth - single data element width for
  * each data reader, useful for generating unrolling addresses
  * writeElementWidth - single data element width for each data writer, useful
  * for generating unrolling addresses tcdmDataWidth - data width for each TCDm
  * port unrollingFactorReader - spatial unrolling factors (your parfor) for
  * each data reader unrollingFactorWriter - spatial unrolling factors (your
  * parfor) for each data writer temporalLoopDim - the dimension of the temporal
  * loop temporalLoopBoundWidth - the register width for storing the temporal
  * loop bound addrWidth - the address width stationarity - accelerator
  * stationarity feature for each data mover (data reader and data writer)
  */

// streamer parameters for the GEMM Accelerator
object GeMMStreamerParameters {
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

// streamer parameters for the Post-processing SIMD Accelerator
object PostProcessingStreamerParameters {
  def fifoWidthReader = Seq(2048)
  def fifoDepthReader = Seq(2)

  def fifoWidthWriter = Seq(512)
  def fifoDepthWriter = Seq(2)

  def dataReaderNum = 1
  def dataWriterNum = 1
  def dataReaderTcdmPorts = Seq(32)
  def dataWriterTcdmPorts = Seq(8)
  def readElementWidth = Seq(32)
  def writeElementWidth = Seq(8)

  def tcdmDataWidth = 64

  def unrollingFactorReader = Seq(Seq(64))
  def unrollingFactorWriter = Seq(Seq(64))

  def temporalLoopDim = 1
  def temporalLoopBoundWidth = 8

  def addrWidth = 32

  def stationarity = Seq(0, 0)

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

// streamer parameters for the MAC Engine Accelerator
object MACStreamerParameters {
  def fifoWidthReader = Seq(64, 64, 64)
  def fifoDepthReader = Seq(2, 2, 2)

  def fifoWidthWriter = Seq(64)
  def fifoDepthWriter = Seq(2)

  def dataReaderNum = 3
  def dataWriterNum = 1
  def dataReaderTcdmPorts = Seq(1, 1, 1)
  def dataWriterTcdmPorts = Seq(1)
  def readElementWidth = Seq(32, 32, 32)
  def writeElementWidth = Seq(32)

  def tcdmDataWidth = 64

  def unrollingFactorReader = Seq(Seq(2), Seq(2), Seq(2))
  def unrollingFactorWriter = Seq(Seq(2))

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
