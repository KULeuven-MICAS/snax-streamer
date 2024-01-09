package streamer

import chisel3._
import chisel3.util._

// input and output for data reader (data mover in read mode)
class DataReaderIO(
    dataReaderTcdmPorts: Int = DataReaderTestParameters.dataReaderTcdmPorts,
    tcdmDataWidth: Int = DataReaderTestParameters.tcdmDataWidth,
    unrollingDim: Int = DataReaderTestParameters.unrollingDim,
    addrWidth: Int = DataReaderTestParameters.addrWidth,
    fifoWidth: Int = DataReaderTestParameters.fifoWidth
) extends Bundle {

  // signals for read request address generation
  val ptr_agu_i = Flipped(Decoupled(UInt(addrWidth.W)))
  val unrollingStrides_csr_i = Flipped(
    Decoupled(Vec(unrollingDim, UInt(addrWidth.W)))
  )

  // tcdm read req
  val tcdm_req_addr = Output(Vec(dataReaderTcdmPorts, UInt(addrWidth.W)))
  val read_tcmd_valid_o = Output(Vec(dataReaderTcdmPorts, Bool()))

  val tcdm_ready_i = Input(Vec(dataReaderTcdmPorts, Bool()))

  // tcdm rsp
  val data_tcdm_i = Flipped(
    (Vec(dataReaderTcdmPorts, Valid(UInt(tcdmDataWidth.W))))
  )

  // data pushed into the queue
  val data_fifo_o = Decoupled(UInt(fifoWidth.W))

  assert(
    fifoWidth == dataReaderTcdmPorts * tcdmDataWidth,
    "fifoWidth should match with TCDM datawidth for now!"
  )

  // from temporal address generation unit to indicate if the transaction is done
  val data_move_done = Input(Bool())

}

// data reader, for sending read request to TCDM and collect data from TCDM response, pushing valid data into the queue
// data producer from the accelerator X aspect
class DataReader(
    dataReaderTcdmPorts: Int = DataReaderTestParameters.dataReaderTcdmPorts,
    tcdmDataWidth: Int = DataReaderTestParameters.tcdmDataWidth,
    unrollingDim: Int = DataReaderTestParameters.unrollingDim,
    unrollingFactor: Seq[Int] = DataReaderTestParameters.unrollingFactor,
    addrWidth: Int = DataReaderTestParameters.addrWidth,
    fifoWidth: Int = DataReaderTestParameters.fifoWidth,
    elementWidth: Int = DataReaderTestParameters.elementWidth
) extends Module
    with RequireAsyncReset {

  val io = IO(
    new DataReaderIO(
      dataReaderTcdmPorts,
      tcdmDataWidth,
      unrollingDim,
      addrWidth,
      fifoWidth
    )
  )

  // storing the temporal start address when it is valid
  val ptr_agu = RegInit(0.U(addrWidth.W))
  val start_ptr = WireInit(0.U(addrWidth.W))

  // config valid signal for unrolling strides
  // storing the config when it is valid
  val config_valid = WireInit(0.B)
  val unrollingStrides = RegInit(
    VecInit(Seq.fill(unrollingDim)(0.U(addrWidth.W)))
  )

  // original unrolling address for TCDM request
  val unrolling_addr = WireInit(
    VecInit(Seq.fill(unrollingFactor.reduce(_ * _))(0.U(addrWidth.W)))
  )

  // for selecting the real TCDM request address from the original unrolling addresses
  // according to the tcdmDataWidth and the elementWidth relationship
  // !!! warning: assuming the data granularity is one tcdmDataWidth
  // no sub-data accessing within one TCDM bank
  def packed_addr_num = (tcdmDataWidth / 8) / (elementWidth / 8)

  // Fifo input signals
  val fifo_input_bits = WireInit(0.U(fifoWidth.W))
  val fifo_input_valid = WireInit(0.B)

  // not in the busy with sending current request process
  val ready_for_new_tcdm_reqs = WireInit(0.B)
  // data fifo isn't full
  val can_send_tcdm_read_req = WireInit(0.B)
  // means transaction success
  val tcdm_read_mem_all_ready = WireInit(0.B)

  // signals for dealing with contention
  val tcdm_rsp_i_p_valid = WireInit(VecInit(Seq.fill(dataReaderTcdmPorts)(0.B)))
  val tcdm_rsp_i_p_valid_reg = RegInit(
    VecInit(Seq.fill(dataReaderTcdmPorts)(0.B))
  )
  val tcdm_rsp_i_q_ready = WireInit(VecInit(Seq.fill(dataReaderTcdmPorts)(0.B)))
  val tcdm_rsp_i_q_ready_reg = RegInit(
    VecInit(Seq.fill(dataReaderTcdmPorts)(0.B))
  )
  val wait_for_q_ready_read = WireInit(0.B)
  val wait_for_p_valid_read = WireInit(0.B)

  // storing response data (part of whole transaction) when waiting for other part
  val data_reg = RegInit(
    VecInit(Seq.fill(dataReaderTcdmPorts)(0.U(tcdmDataWidth.W)))
  )
  val data_fifo_input = WireInit(
    VecInit(Seq.fill(dataReaderTcdmPorts)(0.U(tcdmDataWidth.W)))
  )

  // State declaration
  val sIDLE :: sBUSY :: Nil = Enum(2)
  val cstate = RegInit(sIDLE)
  val nstate = WireInit(sIDLE)

  // Changing states
  cstate := nstate

  chisel3.dontTouch(cstate)
  switch(cstate) {
    is(sIDLE) {
      when(config_valid) {
        nstate := sBUSY
      }.otherwise {
        nstate := sIDLE
      }

    }
    is(sBUSY) {
      when(io.data_move_done) {
        nstate := sIDLE
      }.otherwise {
        nstate := sBUSY
      }
    }
  }

  // store them for later use
  when(config_valid) {
    unrollingStrides := io.unrollingStrides_csr_i.bits
  }

  config_valid := io.unrollingStrides_csr_i.fire

  when(io.ptr_agu_i.fire) {
    ptr_agu := io.ptr_agu_i.bits
  }

  when(io.ptr_agu_i.fire) {
    start_ptr := io.ptr_agu_i.bits
  }.otherwise {
    start_ptr := ptr_agu
  }

  // generating original unrolling address for TCDM request
  val unrolling_addr_gen_unit = Module(
    new UnrollingAddrGenUint(unrollingDim, unrollingFactor, addrWidth)
  )

  unrolling_addr_gen_unit.io.valid_i := cstate =/= sIDLE
  unrolling_addr_gen_unit.io.start_ptr_i := start_ptr
  unrolling_addr_gen_unit.io.unrollingStrides_i := unrollingStrides
  unrolling_addr := unrolling_addr_gen_unit.io.unrolling_addr_o

  // simulation time address constraint check
  for (i <- 0 until dataReaderTcdmPorts) {
    for (j <- 0 until packed_addr_num - 1) {
      assert(
        unrolling_addr(i * packed_addr_num + j + 1) === unrolling_addr(
          i * packed_addr_num + j
        ),
        "read address in not consecutive in the same bank!"
      )
    }
  }

  can_send_tcdm_read_req := io.data_fifo_o.ready && cstate === sBUSY

  // assuming addresses are packed in one tcmd request
  // the data granularity constrain
  for (i <- 0 until dataReaderTcdmPorts) {
    io.tcdm_req_addr(i) := unrolling_addr(i * packed_addr_num)
  }

  // deal with contention
  // ensure all the read reqeust are sent successfully
  for (i <- 0 until dataReaderTcdmPorts) {
    when(can_send_tcdm_read_req) {
      when(io.ptr_agu_i.fire) {
        io.read_tcmd_valid_o(i) := 1.B
      }.otherwise {
        io.read_tcmd_valid_o(i) := ~tcdm_rsp_i_q_ready_reg(i)
      }
    }.otherwise {
      io.read_tcmd_valid_o(i) := 0.B
    }
  }

  for (i <- 0 until dataReaderTcdmPorts) {
    when(can_send_tcdm_read_req && !tcdm_read_mem_all_ready) {
      wait_for_q_ready_read := 1.B
    }.otherwise {
      wait_for_q_ready_read := 0.B
    }
  }

  for (i <- 0 until dataReaderTcdmPorts) {
    when(wait_for_q_ready_read && !tcdm_read_mem_all_ready) {
      when(io.tcdm_ready_i(i)) {
        tcdm_rsp_i_q_ready_reg(i) := io.tcdm_ready_i(i)
      }
    }.otherwise {
      tcdm_rsp_i_q_ready_reg(i) := 0.B
    }
  }

  for (i <- 0 until dataReaderTcdmPorts) {
    tcdm_rsp_i_q_ready(i) := io.tcdm_ready_i(i) || tcdm_rsp_i_q_ready_reg(i)
  }

  tcdm_read_mem_all_ready := tcdm_rsp_i_q_ready.reduce(_ & _)
  ready_for_new_tcdm_reqs := !wait_for_q_ready_read && can_send_tcdm_read_req

  // check and wait for all the response valid
  for (i <- 0 until dataReaderTcdmPorts) {
    when(can_send_tcdm_read_req && !fifo_input_valid) {
      wait_for_p_valid_read := 1.B
    }.otherwise {
      wait_for_p_valid_read := 0.B
    }
  }

  for (i <- 0 until dataReaderTcdmPorts) {
    when(wait_for_p_valid_read && !fifo_input_valid) {
      when(io.data_tcdm_i(i).valid) {
        tcdm_rsp_i_p_valid_reg(i) := io.data_tcdm_i(i).valid
        data_reg(i) := io.data_tcdm_i(i).bits
      }
    }.otherwise {
      tcdm_rsp_i_p_valid_reg(i) := 0.B
    }
  }

  // fifo data
  for (i <- 0 until dataReaderTcdmPorts) {
    when(fifo_input_valid) {
      when(io.data_tcdm_i(i).valid) {
        data_fifo_input(i) := io.data_tcdm_i(i).bits
      }.otherwise {
        data_fifo_input(i) := data_reg(i)
      }
    }
  }
  fifo_input_bits := Cat(data_fifo_input)
  io.data_fifo_o.bits := fifo_input_bits

  // fifo valid
  for (i <- 0 until dataReaderTcdmPorts) {
    tcdm_rsp_i_p_valid(i) := io.data_tcdm_i(i).valid || tcdm_rsp_i_p_valid_reg(
      i
    )
  }
  fifo_input_valid := tcdm_rsp_i_p_valid.reduce(_ & _)
  io.data_fifo_o.valid := fifo_input_valid

  // signal indicating a new address data transaction is ready
  io.ptr_agu_i.ready := cstate === sBUSY && ready_for_new_tcdm_reqs

  // signal for indicating ready for new config
  io.unrollingStrides_csr_i.ready := cstate === sIDLE

}

// Scala main function for generating system veriog file for the DataReader module
object DataReader extends App {
  emitVerilog(
    new (DataReader),
    Array("--target-dir", "generated/streamer")
  )
}
