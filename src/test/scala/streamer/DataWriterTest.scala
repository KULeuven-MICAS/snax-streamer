package streamer

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import scala.math.BigInt
import org.scalatest.matchers.should.Matchers
import org.scalatest.Tag
import scala.util.control.Breaks._

class DataWriterTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  "DUT" should "pass" in {
    test(new DataWriter).withAnnotations(
      Seq(WriteVcdAnnotation)
    ) { dut =>
      dut.clock.step(5)
      dut.io.unrollingStrides_csr_i.bits(0).poke(1)
      dut.io.unrollingStrides_csr_i.bits(1).poke(8)
      dut.io.unrollingStrides_csr_i.valid.poke(1)
      dut.io.data_fifo_i.valid.poke(1)
      for (i <- 0 until DataWriterTestParameters.dataWriterTcdmPorts) {
        dut.io.tcdm_ready_i(i).poke(1)
      }
      dut.clock.step(5)
      dut.io.unrollingStrides_csr_i.valid.poke(0)

      dut.io.ptr_agu_i.bits.poke(100)
      dut.io.ptr_agu_i.valid.poke(1)

      var ready_counter = 0
      breakable {
        while (true) {
          if (dut.io.ptr_agu_i.ready.peekBoolean()) {
            ready_counter = ready_counter + 1
            dut.clock.step(5)
            if (ready_counter == 8) {
              break()
            }
          }
        }
      }
      dut.io.data_move_done.poke(1.B)

      dut.clock.step(5)
      dut.clock.step(50)
    }
  }
}
