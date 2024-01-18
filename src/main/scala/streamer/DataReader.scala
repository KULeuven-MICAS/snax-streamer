package streamer

import chisel3._
import chisel3.util._

// input and output for data reader (data mover in read mode)
class DataReaderIO(
    params: DataMoverParams = DataMoverParams()
) extends DataMoverIO(params) {

  // tcdm rsp
  val tcdm_rsp =
    Vec(params.tcdmPortsNum, Flipped(Valid(new TcdmRsp(params.tcdmDataWidth))))

  // data pushed into the queue
  val data_fifo_o = Decoupled(UInt(params.fifoWidth.W))

  assert(
    params.fifoWidth == params.tcdmPortsNum * params.tcdmDataWidth,
    "fifoWidth should match with TCDM datawidth for now!"
  )

}

// data reader, for sending read request to TCDM and collect data from TCDM response, pushing valid data into the queue
// data producer from the accelerator X aspect
class DataReader(
    params: DataMoverParams = DataMoverParams()
) extends DataMover(params) {

  override lazy val io = IO(new DataReaderIO(params))
  io.suggestName("io")

  // Fifo input signals
  val fifo_input_bits = WireInit(0.U(params.fifoWidth.W))
  val fifo_input_valid = WireInit(0.B)

  // signals for dealing with contention
  val tcdm_rsp_valid = WireInit(VecInit(Seq.fill(params.tcdmPortsNum)(0.B)))
  val tcdm_rsp_valid_reg = RegInit(
    VecInit(Seq.fill(params.tcdmPortsNum)(0.B))
  )

  val wait_for_tcdm_rsp_all_valid = WireInit(0.B)

  // storing response data (part of whole transaction) when waiting for other part
  val data_reg = RegInit(
    VecInit(Seq.fill(params.tcdmPortsNum)(0.U(params.tcdmDataWidth.W)))
  )
  val data_fifo_input = WireInit(
    VecInit(Seq.fill(params.tcdmPortsNum)(0.U(params.tcdmDataWidth.W)))
  )

  can_send_tcdm_req := io.data_fifo_o.ready && cstate === sBUSY

  // check and wait for all the response valid
  for (i <- 0 until params.tcdmPortsNum) {
    when(can_send_tcdm_req && !fifo_input_valid) {
      wait_for_tcdm_rsp_all_valid := 1.B
    }.otherwise {
      wait_for_tcdm_rsp_all_valid := 0.B
    }
  }

  for (i <- 0 until params.tcdmPortsNum) {
    io.tcdm_req(i).bits.data := 0.U
    io.tcdm_req(i).bits.write := 0.U
  }

  for (i <- 0 until params.tcdmPortsNum) {
    when(wait_for_tcdm_rsp_all_valid && !fifo_input_valid) {
      when(io.tcdm_rsp(i).valid) {
        tcdm_rsp_valid_reg(i) := io.tcdm_rsp(i).valid
        data_reg(i) := io.tcdm_rsp(i).bits.data
      }
    }.otherwise {
      tcdm_rsp_valid_reg(i) := 0.B
    }
  }

  // fifo data
  for (i <- 0 until params.tcdmPortsNum) {
    when(fifo_input_valid) {
      when(io.tcdm_rsp(i).valid) {
        data_fifo_input(i) := io.tcdm_rsp(i).bits.data
      }.otherwise {
        data_fifo_input(i) := data_reg(i)
      }
    }
  }
  fifo_input_bits := Cat(data_fifo_input)
  io.data_fifo_o.bits := fifo_input_bits

  // fifo valid
  for (i <- 0 until params.tcdmPortsNum) {
    tcdm_rsp_valid(i) := io.tcdm_rsp(i).valid || tcdm_rsp_valid_reg(i)
  }
  fifo_input_valid := tcdm_rsp_valid.reduce(_ & _)
  io.data_fifo_o.valid := fifo_input_valid

}

// Scala main function for generating system verilog file for the DataReader module
object DataReader extends App {
  emitVerilog(
    new DataReader(DataMoverParams()),
    Array("--target-dir", "generated/streamer")
  )
}
