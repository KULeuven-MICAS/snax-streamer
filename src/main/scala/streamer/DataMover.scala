package streamer

import chisel3._
import chisel3.util._

/** This Data Mover module is a common base class for Data Reader and Data
  * Writer. It has the common IO and common function that Data Reader and Data
  * Writer both has, including interface with temporal address generation unit
  * and state machine, sending tcmd req etc.
  */

/** This class represents the input/output interface for Data Mover module
  * @param params
  *   The parameter class contains all the parameters of a data mover module
  */
class DataMoverIO(params: DataMoverParams = DataMoverParams()) extends Bundle {
  // signals for write request address generation
  val ptr_agu_i = Flipped(Decoupled(UInt(params.addrWidth.W)))
  val spatialStrides_csr_i = Flipped(
    Decoupled(Vec(params.spatialDim, UInt(params.addrWidth.W)))
  )

  // tcdm request ports
  val tcdm_req = (Vec(
    params.tcdmPortsNum,
    Decoupled(new TcdmReq(params.addrWidth, params.tcdmDataWidth))
  ))

  // from temporal address generation unit to indicate if the transaction is done
  val addr_gen_done = Input(Bool())

  val data_movement_done = Output(Bool())
}

/** DataMover is a base class for Data Reader and Data Writer. It contains the
  * logic of getting temporal address from the TemporalAddrGenUnit, generating
  * spatial address and sending request to the tcdm.
  *
  * @param params
  *   The parameter class contains all the parameters of a data mover module
  */
class DataMover(params: DataMoverParams = DataMoverParams())
    extends Module
    with RequireAsyncReset {

  lazy val io = IO(new DataMoverIO(params))

  // storing the temporal start address when it is valid
  val ptr_agu = RegInit(0.U(params.addrWidth.W))
  val start_ptr = WireInit(0.U(params.addrWidth.W))

  // config valid signal for spatial strides
  // storing the config when it is valid
  val config_valid = WireInit(0.B)

  val spatialStrides = RegInit(
    VecInit(Seq.fill(params.spatialDim)(0.U(params.addrWidth.W)))
  )

  val data_movement_done = WireInit(0.B)

  // original spatial address for TCDM request
  val spatial_addr = WireInit(
    VecInit(
      Seq.fill(params.spatialBounds.reduce(_ * _))(0.U(params.addrWidth.W))
    )
  )

  // for selecting the real TCDM request address from the original spatial addresses
  // according to the tcdmDataWidth and the elementWidth relationship
  // !!! warning: assuming the data granularity is one tcdmDataWidth
  // no sub-data accessing within one TCDM bank
  def packed_addr_num = (params.tcdmDataWidth / 8) / (params.elementWidth / 8)

  // not in the busy with sending current request process
  val ready_for_new_tcdm_reqs = WireInit(0.B)
  // reader data fifo isn't full or writer data fifo isn't empty
  // and not almost at the time sending request currently
  val can_send_new_tcdm_req = WireInit(0.B)
  // tcdm req ready signals
  val tcdm_req_ready = WireInit(VecInit(Seq.fill(params.tcdmPortsNum)(0.B)))
  val tcdm_req_ready_reg = RegInit(
    VecInit(Seq.fill(params.tcdmPortsNum)(0.B))
  )
  // means transaction success
  val tcdm_req_all_ready = WireInit(0.B)
  // means waiting for all the tcdm ports ready (including ready before for this transaction)
  val wait_for_tcdm_req_all_ready = WireInit(0.B)

  // a signal for recording sending request currently
  def currently_sending_request = io.tcdm_req.map(_.valid).reduce(_ || _)
  val tcdm_req_success_once = WireInit(0.B)

  // State declaration
  val sIDLE :: sBUSY :: sLAST :: Nil = Enum(3)
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
      when(io.addr_gen_done) {
        // nstate := sLAST
        nstate := sIDLE
      }.otherwise {
        nstate := sBUSY
      }
    }
    // is(sLAST) {
    //   when(data_movement_done) {
    //     nstate := sIDLE
    //   }.otherwise {
    //     nstate := sLAST
    //   }
    // }
  }

  config_valid := io.spatialStrides_csr_i.fire

  // store them for later use
  when(config_valid) {
    spatialStrides := io.spatialStrides_csr_i.bits
  }

  start_ptr := io.ptr_agu_i.bits

  // instantiate a spatial address generation module for generating original spatial address for TCDM request
  val spatial_addr_gen_unit = Module(
    new SpatialAddrGenUnit(
      SpatialAddrGenUnitParams(
        params.spatialDim,
        params.spatialBounds,
        params.addrWidth
      )
    )
  )

  spatial_addr_gen_unit.io.valid_i := cstate =/= sIDLE
  spatial_addr_gen_unit.io.ptr_i := start_ptr
  spatial_addr_gen_unit.io.strides_i := spatialStrides
  spatial_addr := spatial_addr_gen_unit.io.ptr_o

  // simulation time address constraint check. The addresses in one tcdm bank should be continuous
  when(cstate =/= sIDLE) {
    for (i <- 0 until params.tcdmPortsNum) {
      for (j <- 0 until packed_addr_num - 1) {
        assert(
          spatial_addr(i * packed_addr_num + j + 1) === spatial_addr(
            i * packed_addr_num + j
          ) + ((params.tcdmDataWidth / 8) / packed_addr_num).U,
          "read address in not consecutive in the same bank!"
        )
      }
    }
  }

  // data_movement_done := (cstate === sLAST || (io.addr_gen_done && io.ptr_agu_i.fire)) && tcdm_req_all_ready
  data_movement_done := io.addr_gen_done

  // assuming addresses are packed in one tcmd request
  // the data granularity constrain
  for (i <- 0 until params.tcdmPortsNum) {
    io.tcdm_req(i).bits.addr := spatial_addr(i * packed_addr_num)
  }

  // asserting means time to send new request. can read or can write new data. needs to be override in extended class!
  can_send_new_tcdm_req := cstate === sBUSY

  // deal with contention
  // ensure all the read request are sent successfully (get ready signal)
  // if not, keep sending valid request
  for (i <- 0 until params.tcdmPortsNum) {
    when(can_send_new_tcdm_req) {
      when(wait_for_tcdm_req_all_ready) {
        io.tcdm_req(i).valid := ~tcdm_req_ready_reg(i)
      }.elsewhen(io.ptr_agu_i.valid) {
        io.tcdm_req(i).valid := 1.B
      }.otherwise {
        io.tcdm_req(i).valid := 0.B
      }
    }.otherwise {
        io.tcdm_req(i).valid := 0.B
    }
  }

  // store partial request ready signals
  for (i <- 0 until params.tcdmPortsNum) {
    when(!tcdm_req_success_once && can_send_new_tcdm_req) {
      when(io.tcdm_req(i).ready) {
        tcdm_req_ready_reg(i) := io.tcdm_req(i).ready
      }
    }.otherwise {
      // clear all the ready bits before a new transaction
      tcdm_req_ready_reg(i) := 0.B
    }
  }

  // indicating obtained request ready signal when ready previously or currently
  for (i <- 0 until params.tcdmPortsNum) {
    tcdm_req_ready(i) := io.tcdm_req(i).ready || tcdm_req_ready_reg(i)
  }

  tcdm_req_all_ready := tcdm_req_ready.reduce(_ & _)

  tcdm_req_success_once := currently_sending_request && tcdm_req_all_ready

  // when can_send_new_tcdm_req and current request isn't all ready means wait_for_tcdm_req_all_ready
  wait_for_tcdm_req_all_ready := RegNext(
    currently_sending_request && !tcdm_req_all_ready
  )

  // signal indicating a new address data transaction is ready
  io.ptr_agu_i.ready := cstate === sBUSY && tcdm_req_success_once

  // signal for indicating ready for new config
  io.spatialStrides_csr_i.ready := cstate === sIDLE

  io.data_movement_done := data_movement_done

}
