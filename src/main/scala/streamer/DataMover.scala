package streamer

import chisel3._
import chisel3.util._

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
  val done = Input(Bool())

}

class DataMover(params: DataMoverParams = DataMoverParams())
    extends Module
    with RequireAsyncReset {

  lazy val io = IO(new DataMoverIO(params))

  // storing the temporal start address when it is valid
  val ptr_agu = RegInit(0.U(params.addrWidth.W))
  val start_ptr = WireInit(0.U(params.addrWidth.W))

  // config valid signal for unrolling strides
  // storing the config when it is valid
  val config_valid = WireInit(0.B)

  val unrollingStrides = RegInit(
    VecInit(Seq.fill(params.spatialDim)(0.U(params.addrWidth.W)))
  )

  val done = WireInit(0.B)

  // original unrolling address for TCDM request
  val unrolling_addr = WireInit(
    VecInit(
      Seq.fill(params.spatialBounds.reduce(_ * _))(0.U(params.addrWidth.W))
    )
  )

  // for selecting the real TCDM request address from the original unrolling addresses
  // according to the tcdmDataWidth and the elementWidth relationship
  // !!! warning: assuming the data granularity is one tcdmDataWidth
  // no sub-data accessing within one TCDM bank
  def packed_addr_num = (params.tcdmDataWidth / 8) / (params.elementWidth / 8)

  // not in the busy with sending current request process
  val ready_for_new_tcdm_reqs = WireInit(0.B)
  // data fifo isn't full
  val can_send_tcdm_req = WireInit(0.B)
  // tcdm req ready signals
  val tcdm_req_ready = WireInit(VecInit(Seq.fill(params.tcdmPortsNum)(0.B)))
  val tcdm_req_ready_reg = RegInit(
    VecInit(Seq.fill(params.tcdmPortsNum)(0.B))
  )
  // means transaction success
  val tcdm_req_all_ready = WireInit(0.B)
  // means waiting for all the tcdm ports ready (including ready before for this transaction)
  val wait_for_tcdm_req_all_ready = WireInit(0.B)

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
      when(done) {
        nstate := sIDLE
      }.otherwise {
        nstate := sBUSY
      }
    }
  }

  config_valid := io.spatialStrides_csr_i.fire

  // store them for later use
  when(config_valid) {
    unrollingStrides := io.spatialStrides_csr_i.bits
  }

  start_ptr := io.ptr_agu_i.bits

  // generating original unrolling address for TCDM request
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
  spatial_addr_gen_unit.io.strides_i := unrollingStrides
  unrolling_addr := spatial_addr_gen_unit.io.ptr_o

  // simulation time address constraint check
  when(cstate === sBUSY) {
    for (i <- 0 until params.tcdmPortsNum) {
      for (j <- 0 until packed_addr_num - 1) {
        assert(
          unrolling_addr(i * packed_addr_num + j + 1) === unrolling_addr(
            i * packed_addr_num + j
          ) + ((params.tcdmDataWidth / 8) / packed_addr_num).U,
          "read address in not consecutive in the same bank!"
        )
      }
    }
  }

  done := io.done

  // needs to be override in extended class!
  can_send_tcdm_req := cstate === sBUSY

  // assuming addresses are packed in one tcmd request
  // the data granularity constrain
  for (i <- 0 until params.tcdmPortsNum) {
    io.tcdm_req(i).bits.addr := unrolling_addr(i * packed_addr_num)
  }

  // deal with contention
  // ensure all the read request are sent successfully
  for (i <- 0 until params.tcdmPortsNum) {
    when(can_send_tcdm_req) {
      when(io.ptr_agu_i.fire) {
        io.tcdm_req(i).valid := 1.B
      }.otherwise {
        io.tcdm_req(i).valid := ~tcdm_req_ready_reg(i)
      }
    }.otherwise {
      io.tcdm_req(i).valid := 0.B
    }
  }

  for (i <- 0 until params.tcdmPortsNum) {
    when(can_send_tcdm_req && !tcdm_req_all_ready) {
      wait_for_tcdm_req_all_ready := 1.B
    }.otherwise {
      wait_for_tcdm_req_all_ready := 0.B
    }
  }

  for (i <- 0 until params.tcdmPortsNum) {
    when(wait_for_tcdm_req_all_ready && !tcdm_req_all_ready) {
      when(io.tcdm_req(i).ready) {
        tcdm_req_ready_reg(i) := io.tcdm_req(i).ready
      }
    }.otherwise {
      tcdm_req_ready_reg(i) := 0.B
    }
  }

  for (i <- 0 until params.tcdmPortsNum) {
    tcdm_req_ready(i) := io.tcdm_req(i).ready || tcdm_req_ready_reg(i)
  }

  tcdm_req_all_ready := tcdm_req_ready.reduce(_ & _)
  ready_for_new_tcdm_reqs := !wait_for_tcdm_req_all_ready && can_send_tcdm_req

  // signal indicating a new address data transaction is ready
  io.ptr_agu_i.ready := cstate === sBUSY && ready_for_new_tcdm_reqs

  // signal for indicating ready for new config
  io.spatialStrides_csr_i.ready := cstate === sIDLE

}
