package streamer

import chisel3._
import chisel3.util._

object TemporalAddrGenUnitTestParameters {
  def temporalLoopDim = 3
  def temporalLoopBoundWidth = 8
  def addrWidth = 32

}

object SpatialAddrGenUnitTestParameters {
  def unrollingFactor = Seq(8, 8)
  def unrollingDim = unrollingFactor.length
  def addrWidth = 32

}
