package streamer

import chisel3._
import chisel3.util._

// input and output declaration for unrolling adress generation unit
// it's a submodule of a data mover
class UnrollingAddrGenUintIO(
    unrollingDim: Int = UnrollingAddrGenUintTestParameters.unrollingDim,
    unrollingFactor: Seq[Int] =
      UnrollingAddrGenUintTestParameters.unrollingFactor,
    addrWidth: Int = UnrollingAddrGenUintTestParameters.addrWidth
) extends Bundle {

  // configurations for unrolling address generation
  val valid_i = Input(Bool())
  val start_ptr_i = Input(UInt(addrWidth.W))
  val unrollingStrides_i = Input(Vec(unrollingDim, UInt(addrWidth.W)))

  // generated output address
  val unrolling_addr_o = Output(
    Vec(unrollingFactor.reduce(_ * _), UInt(addrWidth.W))
  )
}

// unrolling adress generation unit module
class UnrollingAddrGenUint(
    unrollingDim: Int = UnrollingAddrGenUintTestParameters.unrollingDim,
    unrollingFactor: Seq[Int] =
      UnrollingAddrGenUintTestParameters.unrollingFactor,
    addrWidth: Int = UnrollingAddrGenUintTestParameters.addrWidth
) extends Module
    with RequireAsyncReset {

  val io = IO(
    new UnrollingAddrGenUintIO(unrollingDim, unrollingFactor, addrWidth)
  )

  // a scala function to generate the nested indices of the unrolled loop counter for generating unrolling addresses
  // for instance:
  // genUnrollingLoopIndices(2,Seq(8,8),0) returns (0,0)
  // genUnrollingLoopIndices(2,Seq(8,8),1) returns (1,0)
  // genUnrollingLoopIndices(2,Seq(8,8),10) returns (2,1)
  def genUnrollingLoopIndices(
      unrollingDim: Int,
      unrollingFactor: Seq[Int],
      i: Int
  ): Seq[Int] = {
    val indices = (0 until unrollingDim).map(j =>
      (i / (1 +: unrollingFactor)
        .dropRight(unrollingDim - j)
        .product) % unrollingFactor(j)
    )
    indices
  }

  // a function for generating all the unrolled addresses
  def genUnrollingAddr(
      valid: Bool,
      unrollingDim: Int,
      unrollingFactor: Seq[Int],
      unrollingStrides: Vec[UInt],
      start_ptr: UInt
  ): Vec[UInt] = {

    // a generation time requirement
    require(
      unrollingFactor.length == unrollingDim && unrollingStrides.length == unrollingDim,
      "unrollingFactor and unrollingStrides must have the same length which is unrollingDim"
    )

    val unrolling_addr = WireInit(
      VecInit(Seq.fill(unrollingFactor.reduce(_ * _))(0.U(addrWidth.W)))
    )

    when(valid) {
      // address generation for each unrolling data element
      // will ignore some later to be aligned with bank width
      for (i <- 0 until unrollingFactor.product) {

        // indices for each nested unrolling loop
        val indices = genUnrollingLoopIndices(unrollingDim, unrollingFactor, i)

        // address generation with affine mapping definition
        unrolling_addr(i) := ((0 until unrollingDim)
          .map(j => indices(j).U * unrollingStrides(j)))
          .reduce(_ + _) +& start_ptr
      }
    }

    unrolling_addr
  }

  chisel3.dontTouch(io.unrolling_addr_o)

  io.unrolling_addr_o := genUnrollingAddr(
    io.valid_i,
    unrollingDim,
    unrollingFactor,
    io.unrollingStrides_i,
    io.start_ptr_i
  )

}

// Scala main function for generating system verilog file for the UnrollingAddrGenUint module
object UnrollingAddrGenUint extends App {
  emitVerilog(
    new (UnrollingAddrGenUint),
    Array("--target-dir", "generated/streamer")
  )
}
