package streamer

import chisel3._
import chisel3.util._

case class FIFOParams(width: Int, depth: Int)

/** trait for Streamer parameters
  * @param temporalAddrGenUnitParams
  *   a parameters case class instantiation for temporal address generation unit
  * @param stationarity
  *   accelerator stationarity feature for each data mover (data reader and data
  *   writer)
  * @param dataReaderParams
  *   a seq of case class DataMoverParams instantiation for the Data Readers
  * @param dataWriterParams
  *   a seq of case class DataMoverParams instantiation for the Data Writers
  * @param fifoReaderParams
  *   a seq of case class FIFOParams instantiation for the FIFO connected to
  *   Data Readers
  * @param fifoReaderParams
  *   a seq of case class FIFOParams instantiation for the FIFO connected to
  *   Data Writers
  */
trait HasStreamerCoreParams {

  val temporalAddrGenUnitParams: TemporalAddrGenUnitParams

  val stationarity: Seq[Int]

  val dataReaderParams: Seq[DataMoverParams]
  val dataWriterParams: Seq[DataMoverParams]

  val fifoReaderParams: Seq[FIFOParams]
  val fifoWriterParams: Seq[FIFOParams]

}

/** trait for Streamer inferred parameters
  * @param temporalLoopDim
  *   the dimension of the temporal loop
  * @param temporalLoopBoundWidth
  *   the register width for storing the temporal loop bound
  * @param unrollingDim
  *   a Seq contains the unrolling dimensions for both data reader and data
  *   writer
  * @param tcdmDataWidth
  *   data width for each TCDm port
  * @param addrWidth
  *   the address width
  * @param dataReaderNum
  *   number of data readers
  * @param dataWriterNum
  *   number of data writers
  * @param dataMoverNum
  *   the number of data movers (including data reader and writer)
  * @param dataReaderTcdmPorts
  *   a Seq contains the number of TCDM ports connected to each data reader
  * @param dataWriterTcdmPorts
  *   a Seq contains the number of TCDM ports connected to each data writer
  * @param tcdmPortsNum
  *   the total number of TCDM ports connected the data movers (including data
  *   reader and writer)
  * @param fifoWidthReader
  *   FIFO width for the data readers
  * @param fifoWidthWriter
  *   FIFO width for the data writers
  */
trait HasStreamerInferredParams extends HasStreamerCoreParams {

  val temporalLoopDim: Int = temporalAddrGenUnitParams.loopDim
  val temporalLoopBoundWidth: Int = temporalAddrGenUnitParams.loopBoundWidth

  val unrollingDim: Seq[Int] =
    dataReaderParams.map(_.unrollingDim) ++ dataWriterParams.map(_.unrollingDim)

  val tcdmDataWidth: Int = dataReaderParams(0).tcdmDataWidth
  val addrWidth: Int = temporalAddrGenUnitParams.addrWidth

  val dataReaderNum: Int = dataReaderParams.length
  val dataWriterNum: Int = dataWriterParams.length
  val dataMoverNum: Int = dataReaderNum + dataWriterNum
  val dataReaderTcdmPorts: Seq[Int] = dataReaderParams.map(_.tcdmPortsNum)
  val dataWriterTcdmPorts: Seq[Int] = dataWriterParams.map(_.tcdmPortsNum)
  val tcdmPortsNum: Int = dataReaderTcdmPorts.sum + dataWriterTcdmPorts.sum

  val fifoWidthReader: Seq[Int] = fifoReaderParams.map(_.width)
  val fifoWidthWriter: Seq[Int] = fifoWriterParams.map(_.width)

}

/** This case class represents all the parameters for the Streamer
  * @param temporalAddrGenUnitParams
  * @param stationarity
  * @param dataReaderParams
  * @param dataWriterParams
  * @param fifoReaderParams
  * @param fifoWriterParams
  *   the meaning of these parameters can be found at the top of this file the
  *   default value of these parameters is from the StreamerTestConstant object
  */
case class StreamerParams(
    temporalAddrGenUnitParams: TemporalAddrGenUnitParams =
      StreamerTestConstant.temporalAddrGenUnitParams,
    stationarity: Seq[Int] = StreamerTestConstant.stationarity,
    dataReaderParams: Seq[DataMoverParams] =
      StreamerTestConstant.dataReaderParams,
    dataWriterParams: Seq[DataMoverParams] =
      StreamerTestConstant.dataWriterParams,
    fifoReaderParams: Seq[FIFOParams] = StreamerTestConstant.fifoReaderParams,
    fifoWriterParams: Seq[FIFOParams] = StreamerTestConstant.fifoWriterParams
) extends HasStreamerCoreParams
    with HasStreamerInferredParams

// data to accelerator interface generator
// a vector of decoupled interface with configurable number and configurable width for each port
class DataToAcceleratorX(
    params: StreamerParams
) extends Bundle {
  val data = MixedVec((0 until params.dataReaderNum).map { i =>
    Decoupled(UInt(params.fifoWidthReader(i).W))
  })
}

// data from accelerator interface generator
// a vector of decoupled interface with configurable number and configurable width for each port
class DataFromAcceleratorX(
    params: StreamerParams
) extends Bundle {
  val data = MixedVec((0 until params.dataWriterNum).map { i =>
    Flipped(Decoupled(UInt(params.fifoWidthWriter(i).W)))
  })
}

// csr related io
class StreamerCsrIO(
    params: StreamerParams
) extends Bundle {

  // configurations interface for a new data operation
  val loopBounds_i =
    Vec(params.temporalLoopDim, UInt(params.temporalLoopBoundWidth.W))

  val temporalStrides_csr_i =
    Vec(
      params.dataMoverNum,
      Vec(params.temporalLoopDim, UInt(params.addrWidth.W))
    )

  val spatialStrides_csr_i =
    MixedVec((0 until params.unrollingDim.length).map { i =>
      Vec(params.unrollingDim(i), UInt(params.addrWidth.W))
    })

  val ptr_i =
    Vec(params.dataMoverNum, UInt(params.addrWidth.W))

}

// data related io
class StreamerDataIO(
    params: StreamerParams
) extends Bundle {
  // specify the interface to the accelerator
  val streamer2accelerator =
    new DataToAcceleratorX(params)
  val accelerator2streamer =
    new DataFromAcceleratorX(params)

  // specify the interface to the TCDM
  // request interface with q_valid and q_ready
  val tcdm_req =
    (Vec(
      params.tcdmPortsNum,
      Decoupled(new TcdmReq(params.addrWidth, params.tcdmDataWidth))
    ))
  // response interface with p_valid
  val tcdm_rsp = (Vec(
    params.tcdmPortsNum,
    Flipped(Valid(new TcdmRsp(params.tcdmDataWidth)))
  ))
}

// input and output declaration for streamer generator
class StreamerIO(
    params: StreamerParams
) extends Bundle {

  val csr = Flipped(
    Decoupled(
      new StreamerCsrIO(
        params
      )
    )
  )

  val data = new StreamerDataIO(
    params
  )
}

// streamer generator module
class Streamer(
    params: StreamerParams
) extends Module
    with RequireAsyncReset {

  val io = IO(
    new StreamerIO(
      params
    )
  )

  def tcdm_read_ports_num = params.dataReaderTcdmPorts.reduce(_ + _)

  // data readers instantiation
  // a vector of data reader generator instantiation with different parameters for each module
  val data_reader = Seq((0 until params.dataReaderNum).map { i =>
    Module(
      new DataReader(
        params.dataReaderParams(i)
      )
    )
  }: _*)

  // data writers instantiation
  // a vector of data writer generator instantiation with different parameters for each module
  val data_writer = Seq((0 until params.dataWriterNum).map { i =>
    Module(
      new DataWriter(
        params.dataWriterParams(i)
      )
    )
  }: _*)

  // address generation units instantiation
  // a vector of address generation unit generator instantiation with different parameters for each module
  val address_gen_unit = Seq((0 until params.dataMoverNum).map { i =>
    Module(
      new TemporalAddrGenUnit(
        params.temporalAddrGenUnitParams
      )
    )
  }: _*)

  // signals for state transition
  val config_valid = WireInit(0.B)
  val streamming_finish = WireInit(0.B)

  val datamover_states = RegInit(VecInit(Seq.fill(params.dataMoverNum)(0.B)))

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
      when(streamming_finish) {
        nstate := sIDLE
      }.otherwise {
        nstate := sBUSY
      }
    }
  }

  config_valid := io.csr.fire && io.csr.fire && io.csr.fire && io.csr.fire

  for (i <- 0 until params.dataMoverNum) {
    when(config_valid) {
      datamover_states(i) := 1.B
    }.elsewhen(address_gen_unit(i).io.done) {
      datamover_states(i) := 0.B
    }
  }

  // data streaming finish when all the data mover finished the data movement
  streamming_finish := !datamover_states.reduce(_ | _)

  io.csr.ready := cstate === sIDLE

  // signals connections for the instantiated modules
  // TODO: try to use case map to connect...
  // address generation units csr configuration interface <> streamer IO
  for (i <- 0 until params.dataMoverNum) {
    if (params.stationarity(i) == 1) {
      for (j <- 0 until params.temporalLoopDim) {
        if (j == 0) {
          address_gen_unit(i).io.loopBounds_i.bits(j) := 1.U
        } else {
          address_gen_unit(i).io.loopBounds_i
            .bits(j) := io.csr.bits.loopBounds_i(j)
        }
      }
    } else {
      address_gen_unit(
        i
      ).io.loopBounds_i.bits := io.csr.bits.loopBounds_i
    }
    address_gen_unit(
      i
    ).io.loopBounds_i.valid := io.csr.valid
    address_gen_unit(
      i
    ).io.strides_i.bits := io.csr.bits.temporalStrides_csr_i(
      i
    )
    address_gen_unit(
      i
    ).io.strides_i.valid := io.csr.valid
    address_gen_unit(i).io.ptr_i.bits := io.csr.bits.ptr_i(i)
    address_gen_unit(i).io.ptr_i.valid := io.csr.valid
  }

  // data reader and data writer <> streamer IO
  for (i <- 0 until params.dataMoverNum) {
    if (i < params.dataReaderNum) {
      data_reader(i).io.spatialStrides_csr_i.bits := io.csr.bits
        .spatialStrides_csr_i(i)
      data_reader(
        i
      ).io.spatialStrides_csr_i.valid := io.csr.valid
    } else {
      data_writer(
        i - params.dataReaderNum
      ).io.spatialStrides_csr_i.bits := io.csr.bits.spatialStrides_csr_i(i)
      data_writer(
        i - params.dataReaderNum
      ).io.spatialStrides_csr_i.valid := io.csr.valid
    }
  }

  // data reader and data writer <> address generation units interface
  for (i <- 0 until params.dataMoverNum) {
    if (i < params.dataReaderNum) {
      address_gen_unit(i).io.ptr_o <> data_reader(i).io.ptr_agu_i
      address_gen_unit(i).io.done <> data_reader(i).io.done
    } else {
      address_gen_unit(i).io.ptr_o <> data_writer(
        i - params.dataReaderNum
      ).io.ptr_agu_i
      address_gen_unit(i).io.done <> data_writer(
        i - params.dataReaderNum
      ).io.done
    }
  }

  // data reader and data writer <> accelerator interface
  // with a queue between each data mover and accelerator data decoupled interface
  for (i <- 0 until params.dataMoverNum) {
    if (i < params.dataReaderNum) {
      io.data.streamer2accelerator.data(i) <> Queue(
        data_reader(i).io.data_fifo_o,
        params.fifoReaderParams(i).depth
      )
    } else {
      data_writer(
        i - params.dataReaderNum
      ).io.data_fifo_i <> Queue(
        io.data.accelerator2streamer.data(i - params.dataReaderNum),
        params.fifoWriterParams(i - params.dataReaderNum).depth
      )
    }
  }

  // below does data reader and data writer <> TCDM interface

  // a scala function that flattens the sequenced tcdm ports
  // returns the list of (i,j,k) in which i is data mover index, j is the TCDM port index in ith data mover,
  // k is the overall TCDM port index from the TCDM point of view
  // for instance:
  // flattenSeq(Seq(2,3)) returns List((0,0,0), (0,1,1), (1,0,2), (1,1,3), (1,2,4))
  // flattenSeq(Seq(2,2)) returns List((0,0,0), (0,1,1), (1,0,2), (1,1,3))
  def flattenSeq(inputSeq: Seq[Int]): Seq[(Int, Int, Int)] = {
    var flattenedIndex = -1
    val flattenedSeq = inputSeq.zipWithIndex.flatMap { case (size, dimIndex) =>
      (0 until size).map { innerIndex =>
        flattenedIndex = flattenedIndex + 1
        (dimIndex, innerIndex, flattenedIndex)
      }
    }

    flattenedSeq
  }

  // data reader <> TCDM read ports
  val read_flatten_seq = flattenSeq(params.dataReaderTcdmPorts)
  for ((dimIndex, innerIndex, flattenedIndex) <- read_flatten_seq) {
    // read request to TCDM
    io.data.tcdm_req(flattenedIndex).valid := data_reader(dimIndex).io
      .read_tcmd_valid_o(innerIndex)
    io.data.tcdm_req(flattenedIndex).bits.addr := data_reader(dimIndex).io
      .tcdm_req_addr(innerIndex)
    io.data.tcdm_req(flattenedIndex).bits.data := 0.U
    io.data.tcdm_req(flattenedIndex).bits.write := 0.B

    // request ready signals from TCDM
    data_reader(dimIndex).io
      .tcdm_ready_i(innerIndex) := io.data.tcdm_req(flattenedIndex).ready

    // signals from TCDM responses
    data_reader(dimIndex).io.data_tcdm_i(innerIndex).valid := io.data
      .tcdm_rsp(flattenedIndex)
      .valid
    data_reader(dimIndex).io
      .data_tcdm_i(innerIndex)
      .bits := io.data.tcdm_rsp(flattenedIndex).bits.data
  }

  // data writer <> TCDM write ports
  // TCDM request port bias based on the read TCDM ports number
  val write_flatten_seq = flattenSeq(params.dataWriterTcdmPorts)
  for ((dimIndex, innerIndex, flattenedIndex) <- write_flatten_seq) {

    // write request to TCDM
    io.data.tcdm_req(flattenedIndex + tcdm_read_ports_num).valid := data_writer(
      dimIndex
    ).io.write_tcmd_valid_o(innerIndex)
    io.data
      .tcdm_req(flattenedIndex + tcdm_read_ports_num)
      .bits
      .addr := data_writer(
      dimIndex
    ).io.tcdm_req_addr(innerIndex)
    io.data
      .tcdm_req(flattenedIndex + tcdm_read_ports_num)
      .bits
      .data := data_writer(
      dimIndex
    ).io.tcdm_req_data(innerIndex)
    io.data.tcdm_req(flattenedIndex + tcdm_read_ports_num).bits.write := 1.B

    // request ready signals from TCDM
    data_writer(dimIndex).io.tcdm_ready_i(innerIndex) := io.data
      .tcdm_req(flattenedIndex + tcdm_read_ports_num)
      .ready
  }

}

// Scala main function for generating test streamer system verilog file
object StreamerTester extends App {
  emitVerilog(
    new Streamer(StreamerParams()),
    Array("--target-dir", "generated/streamer/tester")
  )
}

// Scala main function for generating system verilog file for different accelerators
// including GEMM, Post-processing SIMD and MAC engine
object GemmStreamer extends App {
  emitVerilog(
    new Streamer(
      StreamerParams(
        temporalAddrGenUnitParams =
          GeMMStreamerParameters.temporalAddrGenUnitParams,
        fifoReaderParams = GeMMStreamerParameters.fifoReaderParams,
        fifoWriterParams = GeMMStreamerParameters.fifoWriterParams,
        stationarity = GeMMStreamerParameters.stationarity,
        dataReaderParams = GeMMStreamerParameters.dataReaderParams,
        dataWriterParams = GeMMStreamerParameters.dataWriterParams
      )
    ),
    Array("--target-dir", "generated/streamer/gemm")
  )
}

object PostProcessingStreamer extends App {
  emitVerilog(
    new Streamer(
      StreamerParams(
        temporalAddrGenUnitParams =
          PostProcessingStreamerParameters.temporalAddrGenUnitParams,
        fifoReaderParams = PostProcessingStreamerParameters.fifoReaderParams,
        fifoWriterParams = PostProcessingStreamerParameters.fifoWriterParams,
        stationarity = PostProcessingStreamerParameters.stationarity,
        dataReaderParams = PostProcessingStreamerParameters.dataReaderParams,
        dataWriterParams = PostProcessingStreamerParameters.dataWriterParams
      )
    ),
    Array("--target-dir", "generated/streamer/pp")
  )
}

object MacStreamer extends App {
  emitVerilog(
    new Streamer(
      StreamerParams()
    ),
    Array("--target-dir", "generated/streamer/mac")
  )
}
