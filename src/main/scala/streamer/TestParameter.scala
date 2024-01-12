package streamer

import chisel3._
import chisel3.util._

object TemporalAddrGenUnitTestParameters {
  def loopDim = 3
  def loopBoundWidth = 8
  def addrWidth = 32

}

object SpatialAddrGenUnitTestParameters {
  def loopBounds = Seq(8, 8)
  def loopDim = unrollingFactor.length
  def addrWidth = 32
}
