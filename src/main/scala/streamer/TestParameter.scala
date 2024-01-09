package streamer

import chisel3._
import chisel3.util._

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
