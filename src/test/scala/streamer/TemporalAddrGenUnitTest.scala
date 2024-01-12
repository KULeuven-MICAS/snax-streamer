package streamer

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import scala.math.BigInt
import org.scalatest.matchers.should.Matchers
import org.scalatest.Tag

// manual test for temporal adress generation unit
// TODO: automated random configuration test and automated results check
class TemporalAddrGenUnitTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  "DUT" should "pass" in {
    test(new TemporalAddrGenUnit).withAnnotations(
      Seq(WriteVcdAnnotation)
    ) { dut =>
      dut.clock.step(5)
      // random config generation
      val loopBounds =
        Seq.fill(TemporalAddrGenUnitTestParameters.loopDim)(
          (
            4.U
          )
        )
      val strides =
        Seq.fill(TemporalAddrGenUnitTestParameters.loopDim)(
          (
            scala.util.Random.nextInt(10).U
          )
        )

      // sending these configruation to the dut
      for (i <- 0 until TemporalAddrGenUnitTestParameters.loopDim) {
        val lb = loopBounds(i)
        val ts = strides(i)
        dut.io.loopBounds_i.bits(i).poke(lb)
        dut.io.strides_i.bits(i).poke(ts)
      }
      dut.io.ptr_i.bits.poke(16.U)

      dut.io.loopBounds_i.valid.poke(1.B)
      dut.io.strides_i.valid.poke(1.B)
      dut.io.ptr_i.valid.poke(1.B)

      dut.clock.step(1)
      dut.io.loopBounds_i.valid.poke(0.B)
      dut.io.strides_i.valid.poke(0.B)
      dut.io.ptr_i.valid.poke(0.B)

      // keep consuming generated addresses by asserting dut.io.ptr_o.ready
      dut.clock.step(1)
      dut.io.ptr_o.ready.poke(1.B)

      dut.clock.step(100)

    }
  }
}
