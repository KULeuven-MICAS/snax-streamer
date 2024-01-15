package streamer

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import scala.math.BigInt
import org.scalatest.matchers.should.Matchers
import org.scalatest.Tag

// simple test for streamer top module, mimic the csr write process (config streamer process)
class StreamerTopTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  "DUT" should "pass" in {
    test(new StreamerTop(new StreamerTopParams()))
      .withAnnotations(
        Seq(WriteVcdAnnotation)
      ) { dut =>
        dut.clock.step(5)

        // write csr function for better code reuse
        def write_csr(addr: Int, data: Int) = {

          // give the data and address to the right ports
          dut.io.csr.req.bits.write.poke(1.B)
          dut.io.csr.req.bits.data.poke(data.U)
          dut.io.csr.req.bits.addr.poke(addr.U)
          dut.io.csr.req.valid.poke(1.B)

          // wait for grant
          while (dut.io.csr.req.ready.peekBoolean() == false) {
            dut.clock.step(1)
          }

          dut.clock.step(1)

          dut.io.csr.req.valid.poke(0.B)

        }

        // read csr function for better code reuse
        def read_csr(addr: Int, data: Int) = {

          // give the data and address to the right ports
          dut.io.csr.req.bits.write.poke(0.B)
          dut.io.csr.req.bits.data.poke(data.U)
          dut.io.csr.req.bits.addr.poke(addr.U)
          dut.io.csr.req.valid.poke(1.B)

          // wait for grant
          while (dut.io.csr.req.ready.peekBoolean() == false) {
            dut.clock.step(1)
          }

          dut.clock.step(1)

          dut.io.csr.req.valid.poke(0.B)

          // wait for valid signal
          while (dut.io.csr.rsp.valid.peekBoolean() == false) {
            dut.clock.step(1)
          }
          dut.io.csr.rsp.bits.data.peekInt()
        }

        // give valid transaction config
        // temporal loop bound
        val temporal_loop_bound = 20
        write_csr(0, temporal_loop_bound)

        // temporal loop strides
        write_csr(1, 2)
        write_csr(2, 2)
        write_csr(3, 2)
        write_csr(4, 2)

        // spatial loop strides
        // warning!!! give the proper unrolling strides so that is a aligned in one TCDM bank
        write_csr(5, 1)
        write_csr(6, 1)
        write_csr(7, 1)
        write_csr(8, 1)

        // base ptr
        write_csr(9, 10)
        write_csr(10, 20)
        write_csr(11, 30)
        write_csr(12, 40)

        dut.clock.step(5)

        // start
        write_csr(13, 0)

        dut.clock.step(5)

        // give tcdm ports signals, no contention scene
        for (i <- 0 until StreamerParams().dataReaderTcdmPorts.sum) {
          dut.io.data.tcdm_req(i).ready.poke(1.B)
          dut.io.data.tcdm_rsp(i).valid.poke(1.B)
        }

        // give accelerator ready to get input signals
        for (i <- 0 until StreamerParams().dataReaderNum) {
          dut.io.data.streamer2accelerator.data(i).ready.poke(1.B)
        }

        // wait for temporal_loop_bound cycles
        dut.clock.step(temporal_loop_bound)
        for (i <- 0 until StreamerParams().dataReaderTcdmPorts.sum) {
          dut.io.data.tcdm_req(i).ready.poke(0.B)
          dut.io.data.tcdm_rsp(i).valid.poke(0.B)
        }

        // mimic accelerator gives valid data
        for (i <- 0 until StreamerParams().dataWriterNum) {
          dut.io.data.accelerator2streamer.data(i).valid.poke(1.B)
        }

        // mimic tcdm is ready for write request
        for (i <- 0 until StreamerParams().dataWriterTcdmPorts.sum) {
          dut.io.data
            .tcdm_req(i + StreamerParams().dataReaderTcdmPorts.sum)
            .ready
            .poke(1.B)
        }

        // wait for temporal_loop_bound cycles
        dut.clock.step(temporal_loop_bound)
        for (i <- 0 until StreamerParams().dataWriterNum) {
          dut.io.data.accelerator2streamer.data(i).valid.poke(0.B)
        }

        // wait until finish
        write_csr(13, 0)

        dut.clock.step(10)

        // csr read test and check the result
        val read_csr_value = read_csr(0, 0)
        assert(read_csr_value == temporal_loop_bound)

        dut.clock.step(10)

      }
  }
}
