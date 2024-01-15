package streamer

import chisel3._
import chisel3.util._

/** This class represents the input and output ports of the CsrManager module.
  * The input is connected to the SNAX CSR port. The output is connected to the
  * streamer configuration port.
  * @param params
  *   It shares the same parameters as streamer top module.
  */
class CsrManagerIO(
    params: StreamerTopParams
) extends Bundle {

  val csr_config_in = new StreamerTopCsrIO(params.csrAddrWidth)
  val csr_config_out = Decoupled(Vec(params.csrNum, UInt(32.W)))

}

/** This class represents the CsrManager module. It contains the csr registers
  * and the read and write control logic.
  * @param params
  *   It shares the same parameters as streamer top module.
  */
class CsrManager(
    params: StreamerTopParams
) extends Module
    with RequireAsyncReset {

  val io = IO(new CsrManagerIO(params))

  // generate a vector of registers
  val csr = RegInit(VecInit(Seq.fill(params.csrNum)(0.U(32.W))))

  // read and write csr cmd
  val read_csr = io.csr_config_in.req.fire && !io.csr_config_in.req.bits.write
  val write_csr = io.csr_config_in.req.fire && io.csr_config_in.req.bits.write

  // keep sending response when didn't receive the response ready signal
  val keep_sending_csr_rsp = RegNext(
    io.csr_config_in.rsp.valid && !io.csr_config_in.rsp.ready
  )
  // also store the data for later output because the address only be correct when the request  is valid
  val csr_rsp_data_reg = RegInit(0.U(32.W))

  // streamer configuration valid signal
  val config_valid = WireInit(0.B)

  // check if the csr address overflow (access certain csr that doesn't exist)
  when(io.csr_config_in.req.fire) {

    assert(
      io.csr_config_in.req.bits.addr <= params.csrNum.U,
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
  io.csr_config_in.req.ready := !(!io.csr_config_out.ready && io.csr_config_in.req.bits.addr === params.csrNum.U - 1.U) && !(keep_sending_csr_rsp && !io.csr_config_in.req.bits.write)

  // when write/read to the last csr means config valid
  config_valid := io.csr_config_in.req.fire && (io.csr_config_in.req.bits.addr === params.csrNum.U - 1.U)

  // signals connected to the output ports
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
