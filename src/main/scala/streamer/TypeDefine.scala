package streamer

import chisel3._
import chisel3.util._

// simplified tcdm interface
class TcdmReq(
    addrWidth: Int = StreamerTestConstant.addrWidth,
    tcdmDataWidth: Int = StreamerTestConstant.tcdmDataWidth
) extends Bundle {
  val addr = UInt(addrWidth.W)
  val write = Bool()
  val data = UInt(tcdmDataWidth.W)
}

class TcdmRsp(tcdmDataWidth: Int = StreamerTestConstant.tcdmDataWidth)
    extends Bundle {
  val data = UInt(tcdmDataWidth.W)
}
