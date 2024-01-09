package streamer

import chisel3._
import chisel3.util._

// input and output for data writer (data mover in write mode)
class DataWriterIO(
    dataWriterTcdmPorts: Int = DataWriterTestParameters.dataWriterTcdmPorts,
    tcdmDataWidth: Int = DataWriterTestParameters.tcdmDataWidth,
    unrollingDim: Int = DataWriterTestParameters.unrollingDim,
    addrWidth: Int = DataWriterTestParameters.addrWidth,
    fifoWidth: Int = DataWriterTestParameters.fifoWidth
) extends Bundle {

  // signals for write request address generation
  val ptr_agu_i = Flipped(Decoupled(UInt(addrWidth.W)))
  val unrollingStrides_csr_i = Flipped(
    Decoupled(Vec(unrollingDim, UInt(addrWidth.W)))
  )

  // valid data from the queue
  val data_fifo_i = Flipped(Decoupled(UInt(fifoWidth.W)))

  assert(
    fifoWidth == dataWriterTcdmPorts * tcdmDataWidth,
    "fifoWidth should match with TCDM datawidth for now!"
  )

  // tcdm write req signals
  val tcdm_req_addr = Output(Vec(dataWriterTcdmPorts, UInt(addrWidth.W)))
  val write_tcmd_valid_o = Output(Vec(dataWriterTcdmPorts, Bool()))
  val tcdm_req_data = Output(Vec(dataWriterTcdmPorts, UInt(tcdmDataWidth.W)))

  val tcdm_ready_i = Input(Vec(dataWriterTcdmPorts, Bool()))

  // from temporal address generation unit to indicate if the transaction is done
  val data_move_done = Input(Bool())

}

// data writer, for sending write request to TCDM, getting valid data from the queue
// data consumer from the accelerator X aspect
class DataWriter(
    dataWriterTcdmPorts: Int = DataWriterTestParameters.dataWriterTcdmPorts,
    tcdmDataWidth: Int = DataWriterTestParameters.tcdmDataWidth,
    unrollingDim: Int = DataWriterTestParameters.unrollingDim,
    unrollingFactor: Seq[Int] = DataWriterTestParameters.unrollingFactor,
    addrWidth: Int = DataWriterTestParameters.addrWidth,
    fifoWidth: Int = DataWriterTestParameters.fifoWidth,
    elementWidth: Int = DataWriterTestParameters.elementWidth
) extends Module
    with RequireAsyncReset {

  val io = IO(
    new DataWriterIO(
      dataWriterTcdmPorts,
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

  val tcdm_rsp_i_q_ready = WireInit(VecInit(Seq.fill(dataWriterTcdmPorts)(0.B)))
  val tcdm_rsp_i_q_ready_reg = RegInit(
    VecInit(Seq.fill(dataWriterTcdmPorts)(0.B))
  )
  val wait_for_q_ready_write = WireInit(0.B)

  val can_send_tcdm_write_req = WireInit(0.B)
  val ready_for_new_tcdm_reqs = WireInit(0.B)
  val tcdm_write_mem_all_ready = WireInit(0.B)

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

  // address constraint check
  for (i <- 0 until dataWriterTcdmPorts) {
    for (j <- 0 until packed_addr_num - 1) {
      assert(
        unrolling_addr(i * packed_addr_num + j + 1) === unrolling_addr(
          i * packed_addr_num + j
        ),
        "write address in not consecutive in the same bank!"
      )
    }
  }

  can_send_tcdm_write_req := io.data_fifo_i.valid && cstate === sBUSY

  // assuming addresses are packed in one tcmd request
  for (i <- 0 until dataWriterTcdmPorts) {
    io.tcdm_req_addr(i) := unrolling_addr(i * packed_addr_num)
  }

  // deal with contention
  // ensure all the write reqeust are sent successfully
  for (i <- 0 until dataWriterTcdmPorts) {
    when(can_send_tcdm_write_req) {
      when(io.ptr_agu_i.fire) {
        io.write_tcmd_valid_o(i) := 1.B
      }.otherwise {
        io.write_tcmd_valid_o(i) := ~tcdm_rsp_i_q_ready_reg(i)
      }
      io.tcdm_req_data(i) := io.data_fifo_i.bits(
        (i + 1) * tcdmDataWidth - 1,
        i * tcdmDataWidth
      )
    }.otherwise {
      io.write_tcmd_valid_o(i) := 0.B
      io.tcdm_req_data(i) := 0.U
    }
  }

  for (i <- 0 until dataWriterTcdmPorts) {
    when(can_send_tcdm_write_req && !tcdm_write_mem_all_ready) {
      wait_for_q_ready_write := 1.B
    }.otherwise {
      wait_for_q_ready_write := 0.B
    }
  }

  // ensure getting all the grants
  for (i <- 0 until dataWriterTcdmPorts) {
    when(wait_for_q_ready_write && !tcdm_write_mem_all_ready) {
      when(io.tcdm_ready_i(i)) {
        tcdm_rsp_i_q_ready_reg(i) := io.tcdm_ready_i(i)
      }
    }.otherwise {
      tcdm_rsp_i_q_ready_reg(i) := 0.B
    }
  }

  for (i <- 0 until dataWriterTcdmPorts) {
    tcdm_rsp_i_q_ready(i) := io.tcdm_ready_i(i) || tcdm_rsp_i_q_ready_reg(i)
  }

  tcdm_write_mem_all_ready := tcdm_rsp_i_q_ready.reduce(_ & _)
  ready_for_new_tcdm_reqs := !wait_for_q_ready_write && can_send_tcdm_write_req

  // signal indicating new address data transaction is ready
  io.ptr_agu_i.ready := cstate === sBUSY && ready_for_new_tcdm_reqs

  // signal for indicating ready for new config
  io.unrollingStrides_csr_i.ready := cstate === sIDLE

  // ready signal for data queue
  io.data_fifo_i.ready := tcdm_write_mem_all_ready

}

// Scala main function for generating system veriog file for the DataWriter module
object DataWriter extends App {
  emitVerilog(
    new (DataWriter),
    Array("--target-dir", "generated/streamer")
  )
}
