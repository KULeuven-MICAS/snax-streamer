package streamer

import chisel3._
import chisel3.util._

/** This class represents the input and output ports of the CsrManager module
  */
class CsrManagerIO(
    params: StreamerTopParams
) extends Bundle {

  val csr_config_in = new StreamerTopCsrIO(params.csrAddrWidth)
  val csr_config_out = Decoupled(Vec(params.CsrNum, UInt(32.W)))

}

class CsrManager(
    params: StreamerTopParams
) extends Module
    with RequireAsyncReset {

  val io = IO(new CsrManagerIO(params))

  val csr = RegInit(VecInit(Seq.fill(params.CsrNum)(0.U(32.W))))

  val read_csr = io.csr_config_in.req.fire && !io.csr_config_in.req.bits.write
  val write_csr = io.csr_config_in.req.fire && io.csr_config_in.req.bits.write

  val keep_sending_csr_rsp = RegNext(
    io.csr_config_in.rsp.valid && !io.csr_config_in.rsp.ready
  )
  val csr_rsp_data_reg = RegInit(0.U(32.W))

  val config_valid = WireInit(0.B)

  when(io.csr_config_in.req.fire) {

    assert(
      io.csr_config_in.req.bits.addr <= params.CsrNum.U,
      "csr address overflow!"
    )

  }

  // write req
  when(write_csr) {
    csr(io.csr_config_in.req.bits.addr) := io.csr_config_in.req.bits.data
  }

  // read req and keep giving result until get the grant
  when(read_csr) {
    io.csr_config_in.rsp.bits.data := csr(io.csr_config_in.req.bits.addr)
    io.csr_config_in.rsp.valid := 1.B
  }.elsewhen(keep_sending_csr_rsp) {
    io.csr_config_in.rsp.bits.data := csr_rsp_data_reg
    io.csr_config_in.rsp.valid := 1.B
  }.otherwise {
    io.csr_config_in.rsp.valid := 0.B
    io.csr_config_in.rsp.bits.data := 0.U
  }

  // store the csr data for later output because the address only valid when io.csr.fire
  csr_rsp_data_reg := Mux(
    read_csr,
    csr(io.csr_config_in.req.bits.addr),
    csr_rsp_data_reg
  )

  // can not do csr operation when the streamer is busy but attempt start a new data transaction
  // or when still keep sending current rsp but a new read comes
  io.csr_config_in.req.ready := !(!io.csr_config_out.ready && io.csr_config_in.req.bits.addr === params.CsrNum.U - 1.U) && !(keep_sending_csr_rsp && !io.csr_config_in.req.bits.write)

  // when write/read to the last csr means config valid
  config_valid := io.csr_config_in.req.fire && (io.csr_config_in.req.bits.addr === params.CsrNum.U - 1.U)

  io.csr_config_out.bits <> csr
  io.csr_config_out.valid <> config_valid

}

// Scala main function for generating CsrManager system verilog file
object CsrManager extends App {
  emitVerilog(
    new CsrManager(new StreamerTopParams()),
    Array("--target-dir", "generated/csr_manager")
  )
}
