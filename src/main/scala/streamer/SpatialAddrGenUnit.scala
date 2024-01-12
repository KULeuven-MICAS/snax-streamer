package streamer

import chisel3._
import chisel3.util._

/** The spatial address generation unit is a module that generates addresses
  * based on a nested for loop configuration.
  *
  * an example of a 2-dimensional nested for loop is:
  *
  * for (i = 0 until 4) for (j = 0 until 4) addr_out = addr_in + i * 16 + j * 32
  *
  * The loop bounds in this example are 4 and 4. The address is generated by
  * multiplying multiplying the loop counters by the strides 16 and 32
  * respectively.
  *
  * The spatial address generation unit computes all of the addresses in
  * parallel, and outputs them in a vector.
  *
  * The number of nested for loops and the loop bounds are parametrizeable at
  * design time. The loop strides are programmable at run time.
  */

/** This class represents the input and output ports of the SpatialAddrGenUnit
  * module.
  *
  * @param loopDim
  *   The number of nested for loops.
  * @param loopBounds
  *   The bounds of each loop dimension.
  * @param addrWidth
  *   The bit width of the address.
  */
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

/** This class represents a Spatial Address Generation Unit.
  *
  * @param loopDim
  *   The number of nested for loops.
  * @param loopBounds
  *   The bounds of each loop.
  * @param addrWidth
  *   The bit width of the address.
  */
class SpatialAddrGenUnit(
    loopDim: Int = SpatialAddrGenUnitTestParameters.loopDim,
    loopBounds: Seq[Int] = SpatialAddrGenUnitTestParameters.loopBounds,
    addrWidth: Int = SpatialAddrGenUnitTestParameters.addrWidth
) extends Module
    with RequireAsyncReset {

  // the input/output ports
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

  /** Generates spatial addresses based on the given parameters.
    *
    * @param valid
    *   Indicates if the inputs of are valid.
    * @param loopDim
    *   The number of nested for loops.
    * @param loopBounds
    *   The bounds of the for loops.
    * @param strides
    *   The strides for each dimension.
    * @param start_ptr
    *   The starting pointer for address generation.
    * @return
    *   The generated spatial addresses as a vector of unsigned integers.
    */
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

    // the output vector of spatial addresses
    val spatial_addr = WireInit(
      VecInit(Seq.fill(loopBounds.reduce(_ * _))(0.U(addrWidth.W)))
    )

    when(valid) {
      // address generation for each unrolling data element
      // will ignore some later to be aligned with bank width
      for (i <- 0 until loopBounds.product) {

        // indices for each nested unrolling loop
        val indices = genSpatialLoopIndeces(loopDim, loopBounds, i)

        // address generation with affine mapping definition
        spatial_addr(i) := ((0 until loopDim)
          .map(j => indices(j).U * strides(j)))
          .reduce(_ + _) +& start_ptr
      }
    }

    spatial_addr
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
