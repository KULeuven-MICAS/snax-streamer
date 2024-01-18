package streamer

import chisel3._
import chisel3.util._

/** This class is input and output for data writer (data mover in write mode).
  * It is extended from DataMoverIO. It adds fifo input ports.
  * @param params
  *   The parameter class contains all the parameters of a data mover module
  */
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

/** This class is data writer module.It is responsible for getting valid data
  * from the data queue and sending write request to TCDM. It is the data
  * consumer from the accelerator X aspect. It extends from the DataMover and
  * adds fifo input and split the fifo data to tcdm data ports logic.
  * @param params
  *   The parameter class contains all the parameters of a data mover module
  */
class DataWriter(
    params: DataMoverParams = DataMoverParams()
) extends DataMover(params) {

  // override the IO of DataMover
  override lazy val io = IO(new DataWriterIO(params))
  io.suggestName("io")

  // when write fifo isn't empty means there is data to be sent to the tcdm
  can_send_tcdm_req := io.data_fifo_i.valid && cstate === sBUSY

  // when there is valid data, split the data to several tcdm data ports for write
  for (i <- 0 until params.tcdmPortsNum) {
    when(can_send_tcdm_req) {
      io.tcdm_req(i).bits.data := io.data_fifo_i.bits(
        (i + 1) * params.tcdmDataWidth - 1,
        i * params.tcdmDataWidth
      )
    }.otherwise {
      io.tcdm_req(i).bits.data := 0.U
    }
  }

  // write is 1 for all the write request
  for (i <- 0 until params.tcdmPortsNum) {
    io.tcdm_req(i).bits.write := 1.B
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
