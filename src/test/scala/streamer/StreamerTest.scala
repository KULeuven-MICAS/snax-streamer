package streamer

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import scala.math.BigInt
import org.scalatest.matchers.should.Matchers
import org.scalatest.Tag

class StreamerTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  "DUT" should "pass" in {
    test(new Streamer)
      .withAnnotations(
        Seq(WriteVcdAnnotation)
      ) { dut =>
        dut.clock.step(5)

        // give valid transaction config
        dut.io.csr.valid.poke(1.B)
        for (i <- 0 until StreamerTestConstant.temporalLoopDim) {
          dut.io.csr.bits.loopBounds_i(i).poke(2)
        }
        // give the proper unrolling strides so that is a aligned in one TCDM bank
        dut.io.csr.bits.spatialStrides_csr_i(0)(0).poke(1)
        dut.io.csr.bits.spatialStrides_csr_i(1)(0).poke(1)
        dut.io.csr.bits.spatialStrides_csr_i(2)(0).poke(1)
        dut.clock.step(1)
        dut.io.csr.valid.poke(0.B)

        dut.clock.step(5)

        // give tcdm ports signals, no contention scene
        for (i <- 0 until StreamerTestConstant.dataReaderTcdmPorts.sum) {
          dut.io.data.tcdm_req(i).ready.poke(1.B)
          dut.io.data.tcdm_rsp(i).valid.poke(1.B)
        }
        dut.clock.step(10)

        // give accelerator signals
        for (i <- 0 until StreamerTestConstant.dataReaderNum) {
          dut.io.data.streamer2accelerator.data(i).ready.poke(1.B)
        }

        dut.clock.step(10)
        for (i <- 0 until StreamerTestConstant.dataReaderTcdmPorts.sum) {
          dut.io.data.tcdm_req(i).ready.poke(0.B)
          dut.io.data.tcdm_rsp(i).valid.poke(0.B)
        }

        // mimic accelerator gives valid data
        dut.clock.step(10)
        for (i <- 0 until StreamerTestConstant.dataWriterNum) {
          dut.io.data.accelerator2streamer.data(i).valid.poke(1.B)
        }
        dut.clock.step(4)
        for (i <- 0 until StreamerTestConstant.dataWriterNum) {
          dut.io.data.accelerator2streamer.data(i).valid.poke(0.B)
        }

        // mimic tcdm is ready for write request
        for (i <- 0 until StreamerTestConstant.dataWriterTcdmPorts.sum) {
          dut.io.data
            .tcdm_req(i + StreamerTestConstant.dataReaderTcdmPorts.sum)
            .ready
            .poke(1.B)
        }
        dut.clock.step(10)

      }
  }
}
