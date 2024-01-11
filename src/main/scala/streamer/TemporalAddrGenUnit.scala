package streamer

import chisel3._
import chisel3.util._

// input and output declaration for temporal address generation unit
class TemporalAddrGenUnitIO(
    temporalLoopDim: Int = TemporalAddrGenUnitTestParameters.temporalLoopDim,
    temporalLoopBoundWidth: Int =
      TemporalAddrGenUnitTestParameters.temporalLoopBoundWidth,
    addrWidth: Int = TemporalAddrGenUnitTestParameters.addrWidth
) extends Bundle {

  // configurations for temporal address generation, including temporal loop bounds, strides, and base addresses
  // they are defined by the temporal loop meaning...
  // it is decoupled interface for handshake
  // if the signals are not ready (busy with last transaction), config is not taken-in by the temporal address generation unit
  val temporalLoopBounds_i = Flipped(
    Decoupled(Vec(temporalLoopDim, UInt(temporalLoopBoundWidth.W)))
  )
  val temporalStrides_i = Flipped(
    Decoupled(Vec(temporalLoopDim, UInt(addrWidth.W)))
  )
  val ptr_i = Flipped(Decoupled(UInt(addrWidth.W)))

  // generated output address, decoupled interface for handshake
  // if ptr_o is ready, the current address is taken successfully, can generate next address
  val ptr_o = Decoupled(UInt(addrWidth.W))

  // done signal indicating current transaction is done, ready for next config and transaction
  val data_move_done = Output(Bool())
}

// temporal address generation unit module
class TemporalAddrGenUnit(
    temporalLoopDim: Int = TemporalAddrGenUnitTestParameters.temporalLoopDim,
    temporalLoopBoundWidth: Int =
      TemporalAddrGenUnitTestParameters.temporalLoopBoundWidth,
    addrWidth: Int = TemporalAddrGenUnitTestParameters.addrWidth
) extends Module
    with RequireAsyncReset {

  val io = IO(
    new TemporalAddrGenUnitIO(
      temporalLoopDim,
      temporalLoopBoundWidth,
      addrWidth
    )
  )

  // control signals for state change
  val config_valid = WireInit(0.B)
  val addr_gen_finish = WireInit(0.B)

  // Assuming temporalLoopBounds and temporalStrides are of type Vec[UInt]
  // configuration registers, when config valid, store the configuration for later address generation
  val temporalLoopBounds = RegInit(
    VecInit(Seq.fill(temporalLoopDim)(0.U(temporalLoopBoundWidth.W)))
  )
  val temporalStrides = RegInit(
    VecInit(Seq.fill(temporalLoopDim)(0.U(addrWidth.W)))
  )
  val ptr = RegInit(0.U((temporalLoopBoundWidth).W))

  // Assuming addr_gen_counter and loop_counters are of type UInt
  // counter tracking address generation process
  val addr_gen_counter = RegInit(
    0.U((temporalLoopDim * temporalLoopBoundWidth).W)
  )
  // sub-counters for tracking index for each nested loop
  val loop_counters = WireInit(
    VecInit(Seq.fill(temporalLoopDim)(0.U(temporalLoopBoundWidth.W)))
  )

  // signals indicating can generating next address
  // signal indicating next address can be generated
  val addr_gen_counter_inc = Wire(Bool())

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
      when(addr_gen_finish) {
        nstate := sIDLE
      }.otherwise {
        nstate := sBUSY
      }
    }
  }

  // when the config is valid, store the configuration for address generation
  config_valid := io.temporalLoopBounds_i.fire && io.temporalStrides_i.fire && io.ptr_i.fire

  when(config_valid) {
    temporalLoopBounds := io.temporalLoopBounds_i.bits
    temporalStrides := io.temporalStrides_i.bits
    ptr := io.ptr_i.bits
  }

  // generating next address
  chisel3.dontTouch(addr_gen_counter_inc)
  addr_gen_counter_inc := io.ptr_o.ready === 1.B && cstate === sBUSY && !addr_gen_finish

  when(addr_gen_counter_inc === 1.B) {
    addr_gen_counter := addr_gen_counter + 1.U
  }.elsewhen(cstate === sIDLE) {
    addr_gen_counter := 0.U
  }.otherwise {
    addr_gen_counter := addr_gen_counter
  }

  addr_gen_finish := addr_gen_counter === temporalLoopBounds.reduce(
    _ * _
  ) && cstate === sBUSY

  // function for generating sub-counters for each nested loop to generate the output address
  def genNestedLoopCounter(
      valid: Bool,
      temporalLoopDim: Int,
      temporalLoopBounds: Vec[UInt]
  ): Vec[UInt] = {

    val loop_counters = RegInit(
      VecInit(Seq.fill(temporalLoopDim)(0.U(temporalLoopBoundWidth.W)))
    )
    val loop_counters_next = WireInit(
      VecInit(Seq.fill(temporalLoopDim)(0.U(temporalLoopBoundWidth.W)))
    )
    val loop_counters_valid = WireInit(VecInit(Seq.fill(temporalLoopDim)(0.B)))
    val loop_counters_last = WireInit(VecInit(Seq.fill(temporalLoopDim)(0.B)))

    for (i <- 0 until temporalLoopDim) {
      loop_counters_next(i) := loop_counters(i) + 1.U
      loop_counters_last(i) := loop_counters_next(i) === temporalLoopBounds(i)
    }

    loop_counters_valid(0) := valid
    for (i <- 1 until temporalLoopDim) {
      loop_counters_valid(i) := loop_counters_last(
        i - 1
      ) && loop_counters_valid(i - 1)
    }

    for (i <- 0 until temporalLoopDim) {
      when(loop_counters_valid(i)) {
        loop_counters(i) := Mux(
          loop_counters_last(i),
          0.U,
          loop_counters_next(i)
        )
      }.otherwise {
        loop_counters(i) := loop_counters(i)
      }
    }

    loop_counters

  }

  // generating sub-loop counters using the addr_gen_counter
  loop_counters := genNestedLoopCounter(
    io.ptr_o.fire,
    temporalLoopDim,
    temporalLoopBounds
  )

  // address generation with affine mapping definition
  io.ptr_o.bits := (loop_counters
    .zip(temporalStrides)
    .map { case (a, b) => a * b })
    .reduce(_ +& _) +& ptr

  // decoupled interface driven by the module state
  io.ptr_o.valid := cstate === sBUSY && !addr_gen_finish

  io.temporalLoopBounds_i.ready := cstate === sIDLE
  io.temporalStrides_i.ready := cstate === sIDLE
  io.ptr_i.ready := cstate === sIDLE

  io.data_move_done := addr_gen_finish
}

// Scala main function for generating system verilog file for the TemporalAddrGenUnit module
object TemporalAddrGenUnit extends App {
  emitVerilog(
    new TemporalAddrGenUnit(),
    Array("--target-dir", "generated/streamer")
  )
}
