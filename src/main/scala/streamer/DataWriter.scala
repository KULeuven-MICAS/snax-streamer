package streamer

import chisel3._
import chisel3.util._

// input and output for data writer (data mover in write mode)
class DataWriterIO(
    params: DataMoverParams = DataMoverParams()
) extends DataMoverIO(params) {

  // valid data from the queue
  val data_fifo_i = Flipped(Decoupled(UInt(params.fifoWidth.W)))

  assert(
    params.fifoWidth == params.tcdmPortsNum * params.tcdmDataWidth,
    "params.fifoWidth should match with TCDM datawidth for now!"
  )

}

// data writer, for sending write request to TCDM, getting valid data from the queue
// data consumer from the accelerator X aspect
class DataWriter(
    params: DataMoverParams = DataMoverParams()
) extends DataMover(params) {

  override lazy val io = IO(new DataWriterIO(params))
  io.suggestName("io")

  can_send_tcdm_req := io.data_fifo_i.valid && cstate === sBUSY

  // deal with contention
  // ensure all the write request are sent successfully
  for (i <- 0 until params.tcdmPortsNum) {
    when(can_send_tcdm_req) {
      io.tcdm_req(i).bits.data := io.data_fifo_i.bits(
        (i + 1) * params.tcdmDataWidth - 1,
        i * params.tcdmDataWidth
      )
    }.otherwise {
      io.tcdm_req(i).bits.data := 0.U
    }
    io.tcdm_req(i).bits.write := 1.U
  }

  // ready signal for data queue
  io.data_fifo_i.ready := tcdm_req_all_ready

}

// Scala main function for generating system verilog file for the DataWriter module
object DataWriter extends App {
  emitVerilog(
    new DataWriter(DataMoverParams()),
    Array("--target-dir", "generated/streamer")
  )
}
