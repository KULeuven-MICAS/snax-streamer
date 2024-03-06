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
    test(new Streamer(StreamerParams()))
      .withAnnotations(
        Seq(WriteVcdAnnotation)
      ) { dut =>
        dut.clock.step(5)

        // give valid transaction config
        dut.io.csr.valid.poke(1.B)
        for (i <- 0 until StreamerParams().temporalDim) {
          dut.io.csr.bits.loopBounds_i(i).poke(2)
        }

        // give the proper spatial strides so that is a aligned in one TCDM bank
        for (i <- 0 until StreamerParams().dataMoverNum) {
          dut.io.csr.bits.spatialStrides_csr_i(i)(0).poke(4)
        }

        dut.clock.step(1)
        dut.io.csr.valid.poke(0.B)

        dut.clock.step(5)

        // give tcdm ports signals, no contention scene
        for (i <- 0 until StreamerParams().dataReaderTcdmPorts.sum) {
          dut.io.data.tcdm_req(i).ready.poke(1.B)
          dut.io.data.tcdm_rsp(i).valid.poke(1.B)
        }
        dut.clock.step(10)

        // give accelerator signals
        for (i <- 0 until StreamerParams().dataReaderNum) {
          dut.io.data.streamer2accelerator.data(i).ready.poke(1.B)
        }

        dut.clock.step(10)
        for (i <- 0 until StreamerParams().dataReaderTcdmPorts.sum) {
          dut.io.data.tcdm_req(i).ready.poke(0.B)
          dut.io.data.tcdm_rsp(i).valid.poke(0.B)
        }

        // mimic accelerator gives valid data
        dut.clock.step(10)
        for (i <- 0 until StreamerParams().dataWriterNum) {
          dut.io.data.accelerator2streamer.data(i).valid.poke(1.B)
        }
        dut.clock.step(4)
        for (i <- 0 until StreamerParams().dataWriterNum) {
          dut.io.data.accelerator2streamer.data(i).valid.poke(0.B)
        }

        // mimic tcdm is ready for write request
        for (i <- 0 until StreamerParams().dataWriterTcdmPorts.sum) {
          dut.io.data
            .tcdm_req(i + StreamerParams().dataReaderTcdmPorts.sum)
            .ready
            .poke(1.B)
        }
        dut.clock.step(10)

      }
  }
}
