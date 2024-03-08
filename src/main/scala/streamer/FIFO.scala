package streamer

import chisel3._
import chisel3.util._

// Customized FIFO with an extra almost_full signal.
// almost_full will be asserted when there is Depth-1 elements in the FIFO
class FIFOIO(width: Int = FIFOTestParameters.fifoWidth) extends Bundle {
  val in = Flipped(Decoupled(UInt(width.W)))
  val out = Decoupled(UInt(width.W))
  val almost_full = Output(Bool())
}

class FIFO(
    depth: Int = FIFOTestParameters.fifoDepth,
    width: Int = FIFOTestParameters.fifoWidth
) extends Module
    with RequireAsyncReset {

  val io = IO(new FIFOIO(width))
  val fifo = Module(new Queue(UInt(width.W), depth))

  fifo.io.enq <> io.in
  fifo.io.deq <> io.out

  io.almost_full := fifo.io.count === (depth - 1).U

}

object FIFO extends App {
  emitVerilog(
    new (FIFO),
    Array("--target-dir", "generated/streamer")
  )
}
