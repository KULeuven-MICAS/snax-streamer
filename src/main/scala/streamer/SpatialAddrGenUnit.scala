package streamer

import chisel3._
import chisel3.util._

// input and output declaration for spatial unrolling address generation unit
// it's a submodule of a data mover
class SpatialAddrGenUnitIO(
    loopDim: Int = SpatialAddrGenUnitTestParameters.loopDim,
    loopBounds: Seq[Int] = SpatialAddrGenUnitTestParameters.loopBounds,
    addrWidth: Int = SpatialAddrGenUnitTestParameters.addrWidth
) extends Bundle {

  // configurations for unrolling address generation
  val valid_i = Input(Bool())
  val ptr_i = Input(UInt(addrWidth.W))
  val strides_i = Input(Vec(loopDim, UInt(addrWidth.W)))

  // generated output addresses
  val ptr_o = Output(
    Vec(loopBounds.reduce(_ * _), UInt(addrWidth.W))
  )
}

// unrolling adress generation unit module
class SpatialAddrGenUnit(
    loopDim: Int = SpatialAddrGenUnitTestParameters.loopDim,
    loopBounds: Seq[Int] =
      SpatialAddrGenUnitTestParameters.loopBounds,
    addrWidth: Int = SpatialAddrGenUnitTestParameters.addrWidth
) extends Module with RequireAsyncReset {

  val io = IO(
    new SpatialAddrGenUnitIO(loopDim, loopBounds, addrWidth)
  )

  // a scala function to generate the nested indices of the unrolled loop counter for generating unrolling addresses
  // for instance:
  // genSpatialLoopIndeces(2,Seq(8,8),0) returns (0,0)
  // genSpatialLoopIndeces(2,Seq(8,8),1) returns (1,0)
  // genSpatialLoopIndeces(2,Seq(8,8),10) returns (2,1)
  def genSpatialLoopIndeces(
      loopDim: Int,
      loopBounds: Seq[Int],
      i: Int
  ): Seq[Int] = {
    val indices = (0 until loopDim).map(j =>
      (i / (1 +: loopBounds)
        .dropRight(loopDim - j)
        .product) % loopBounds(j)
    )
    indices
  }

  // a function for generating all the spatial addresses
  def genSpatialAddr(
      valid: Bool,
      loopDim: Int,
      loopBounds: Seq[Int],
      strides: Vec[UInt],
      start_ptr: UInt
  ): Vec[UInt] = {

    // a generation time requirement
    require(
      loopBounds.length == loopDim && strides.length == loopDim,
      "loopBounds and strides must have the same length which is loopDim"
    )

    val unrolling_addr = WireInit(
      VecInit(Seq.fill(loopBounds.reduce(_ * _))(0.U(addrWidth.W)))
    )

    when(valid) {
      // address generation for each unrolling data element
      // will ignore some later to be aligned with bank width
      for (i <- 0 until loopBounds.product) {

        // indices for each nested unrolling loop
        val indices = genSpatialLoopIndeces(loopDim, loopBounds, i)

        // address generation with affine mapping definition
        unrolling_addr(i) := ((0 until loopDim)
          .map(j => indices(j).U * strides(j)))
          .reduce(_ + _) +& start_ptr
      }
    }

    unrolling_addr
  }

  chisel3.dontTouch(io.ptr_o)

  io.ptr_o := genSpatialAddr(
    io.valid_i,
    loopDim,
    loopBounds,
    io.strides_i,
    io.ptr_i
  )

}

// Scala main function for generating system verilog file for the SpatialAddrGenUnit module
object SpatialAddrGenUnit extends App {
  emitVerilog(
    new (SpatialAddrGenUnit),
    Array("--target-dir", "generated/streamer")
  )
}
