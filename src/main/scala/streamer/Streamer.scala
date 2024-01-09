package streamer

import chisel3._
import chisel3.util._

// data to accelerator interface generator
// a vector of decoupled interface with configurable number and configurable width for each port
class DataToAcceleratorX(
    dataReaderNum: Int = StreamerTestConstant.dataReaderNum,
    fifoWidth: Seq[Int] = StreamerTestConstant.fifoWidthReader
) extends Bundle {
  val data = MixedVec((0 until dataReaderNum).map { i =>
    Decoupled(UInt(fifoWidth(i).W))
  })
}

// data from accelerator interface generator
// a vector of decoupled interface with configurable number and configurable width for each port
class DataFromAcceleratorX(
    dataWriterNum: Int = StreamerTestConstant.dataWriterNum,
    fifoWidth: Seq[Int] = StreamerTestConstant.fifoWidthWriter
) extends Bundle {
  val data = MixedVec((0 until dataWriterNum).map { i =>
    Flipped(Decoupled(UInt(fifoWidth(i).W)))
  })
}

// input and output decleration for streamer generator
class StreamerIO(
    temporalLoopDim: Int = StreamerTestConstant.temporalLoopDim,
    temporalLoopBoundWidth: Int = StreamerTestConstant.temporalLoopBoundWidth,
    addrWidth: Int = StreamerTestConstant.addrWidth,
    unrollingDim: Seq[Int] = StreamerTestConstant.unrollingDim,
    dataMoverNum: Int = StreamerTestConstant.dataMoverNum,
    dataReaderNum: Int = StreamerTestConstant.dataReaderNum,
    dataWriterNum: Int = StreamerTestConstant.dataWriterNum,
    fifoWidthReader: Seq[Int] = StreamerTestConstant.fifoWidthReader,
    fifoWidthWriter: Seq[Int] = StreamerTestConstant.fifoWidthWriter,
    tcdmPortsNum: Int = StreamerTestConstant.tcdmPortsNum
) extends Bundle {

  // configurations interface for a new data operation
  val temporalLoopBounds_i = Flipped(
    Decoupled(Vec(temporalLoopDim, UInt(temporalLoopBoundWidth.W)))
  )
  val temporalStrides_i = Flipped(
    Decoupled(
      Vec(dataMoverNum, Vec(temporalLoopDim, UInt(addrWidth.W)))
    )
  )
  val unrollingStrides_i = Flipped(
    Decoupled(
      MixedVec((0 until unrollingDim.length).map { i =>
        Vec(unrollingDim(i), UInt(addrWidth.W))
      })
    )
  )
  val ptr_i = Flipped(
    Decoupled(Vec(dataMoverNum, UInt(addrWidth.W)))
  )

  // specify the interface to the accelerator
  val data_accelerator_o =
    new DataToAcceleratorX(dataReaderNum, fifoWidthReader)
  val data_accelerator_i =
    new DataFromAcceleratorX(dataWriterNum, fifoWidthWriter)

  // specify the interface to the TCDM
  // reqeust interface with q_valid and q_ready
  val tcdm_req = (Vec(tcdmPortsNum, Decoupled(new TcdmReq())))
  // response interface with p_valid
  val tcdm_rsp = (Vec(tcdmPortsNum, Flipped(Valid(new TcdmRsp()))))
}

// streamer generator module
class Streamer(
    temporalLoopDim: Int = StreamerTestConstant.temporalLoopDim,
    temporalLoopBoundWidth: Int = StreamerTestConstant.temporalLoopBoundWidth,
    addrWidth: Int = StreamerTestConstant.addrWidth,
    unrollingDim: Seq[Int] = StreamerTestConstant.unrollingDim,
    unrollingFactorReader: Seq[Seq[Int]] =
      StreamerTestConstant.unrollingFactorReader,
    unrollingDimReader: Seq[Int] = StreamerTestConstant.unrollingDimReader,
    unrollingFactorWriter: Seq[Seq[Int]] =
      StreamerTestConstant.unrollingFactorWriter,
    unrollingDimWriter: Seq[Int] = StreamerTestConstant.unrollingDimWriter,
    tcdmDataWidth: Int = StreamerTestConstant.tcdmDataWidth,
    dataMoverNum: Int = StreamerTestConstant.dataMoverNum,
    dataReaderNum: Int = StreamerTestConstant.dataReaderNum,
    dataWriterNum: Int = StreamerTestConstant.dataWriterNum,
    tcdmPortsNum: Int = StreamerTestConstant.tcdmPortsNum,
    dataReaderTcdmPorts: Seq[Int] = StreamerTestConstant.dataReaderTcdmPorts,
    dataWriterTcdmPorts: Seq[Int] = StreamerTestConstant.dataWriterTcdmPorts,
    readElementWidth: Seq[Int] = StreamerTestConstant.readElementWidth,
    writeElementWidth: Seq[Int] = StreamerTestConstant.writeElementWidth,
    fifoWidthReader: Seq[Int] = StreamerTestConstant.fifoWidthReader,
    fifoDepthReader: Seq[Int] = StreamerTestConstant.fifoDepthReader,
    fifoWidthWriter: Seq[Int] = StreamerTestConstant.fifoWidthWriter,
    fifoDepthWriter: Seq[Int] = StreamerTestConstant.fifoDepthWriter,
    stationarity: Seq[Int] = StreamerTestConstant.stationarity
) extends Module
    with RequireAsyncReset {

  val io = IO(
    new StreamerIO(
      temporalLoopDim,
      temporalLoopBoundWidth,
      addrWidth,
      unrollingDim,
      dataMoverNum,
      dataReaderNum,
      dataWriterNum,
      fifoWidthReader,
      fifoWidthWriter,
      tcdmPortsNum
    )
  )

  def tcdm_read_ports_num = dataReaderTcdmPorts.reduce(_ + _)

  // data readers instantiation
  // a vector of data reader generator instantiation with different parameters for each module
  val data_reader = Seq((0 until dataReaderNum).map { i =>
    Module(
      new DataReader(
        dataReaderTcdmPorts(i),
        tcdmDataWidth,
        unrollingDimReader(i),
        unrollingFactorReader(i),
        addrWidth,
        fifoWidthReader(i),
        readElementWidth(i)
      )
    )
  }: _*)

  // data writers instantiation
  // a vector of data writer generator instantiation with different parameters for each module
  val data_writer = Seq((0 until dataWriterNum).map { i =>
    Module(
      new DataWriter(
        dataWriterTcdmPorts(i),
        tcdmDataWidth,
        unrollingDimWriter(i),
        unrollingFactorWriter(i),
        addrWidth,
        fifoWidthWriter(i),
        writeElementWidth(i)
      )
    )
  }: _*)

  // address generation units instantiation
  // a vector of address generation unit generator instantiation with different parameters for each module
  val address_gen_unit = Seq((0 until dataMoverNum).map { i =>
    Module(
      new TemporalAdressGenUnit(
        temporalLoopDim,
        temporalLoopBoundWidth,
        addrWidth
      )
    )
  }: _*)

  // signals for state transition
  val config_valid = WireInit(0.B)
  val streamming_finish = WireInit(0.B)

  val datamover_states = RegInit(VecInit(Seq.fill(dataMoverNum)(0.B)))

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

  config_valid := io.temporalLoopBounds_i.fire && io.temporalStrides_i.fire && io.unrollingStrides_i.fire && io.ptr_i.fire

  for (i <- 0 until dataMoverNum) {
    when(config_valid) {
      datamover_states(i) := 1.B
    }.elsewhen(address_gen_unit(i).io.data_move_done) {
      datamover_states(i) := 0.B
    }
  }

  // data streaming finish when all the data mover finished the data movement
  streamming_finish := !datamover_states.reduce(_ | _)

  io.temporalLoopBounds_i.ready := cstate === sIDLE
  io.temporalStrides_i.ready := cstate === sIDLE
  io.unrollingStrides_i.ready := cstate === sIDLE
  io.ptr_i.ready := cstate === sIDLE

  // signals connections for the instantiated modules
  // TODO: try to use case map to connect...
  // address generation units csr configuration interface <> streamer IO
  for (i <- 0 until dataMoverNum) {
    if (stationarity(i) == 1) {
      for (j <- 0 until temporalLoopDim) {
        if (j == 0) {
          address_gen_unit(i).io.temporalLoopBounds_i.bits(j) := 1.U
        } else {
          address_gen_unit(i).io.temporalLoopBounds_i
            .bits(j) := io.temporalLoopBounds_i.bits(j)
        }
      }
    } else {
      address_gen_unit(
        i
      ).io.temporalLoopBounds_i.bits := io.temporalLoopBounds_i.bits
    }
    address_gen_unit(
      i
    ).io.temporalLoopBounds_i.valid := io.temporalLoopBounds_i.valid
    address_gen_unit(i).io.temporalStrides_i.bits := io.temporalStrides_i.bits(
      i
    )
    address_gen_unit(i).io.temporalStrides_i.valid := io.temporalStrides_i.valid
    address_gen_unit(i).io.ptr_i.bits := io.ptr_i.bits(i)
    address_gen_unit(i).io.ptr_i.valid := io.ptr_i.valid
  }

  // data reader and data writer <> streamer IO
  for (i <- 0 until dataMoverNum) {
    if (i < dataReaderNum) {
      data_reader(i).io.unrollingStrides_csr_i.bits := io.unrollingStrides_i
        .bits(i)
      data_reader(
        i
      ).io.unrollingStrides_csr_i.valid := io.unrollingStrides_i.valid
    } else {
      data_writer(
        i - dataReaderNum
      ).io.unrollingStrides_csr_i.bits := io.unrollingStrides_i.bits(i)
      data_writer(
        i - dataReaderNum
      ).io.unrollingStrides_csr_i.valid := io.unrollingStrides_i.valid
    }
  }

  // data reader and data writer <> address generation units interface
  for (i <- 0 until dataMoverNum) {
    if (i < dataReaderNum) {
      address_gen_unit(i).io.ptr_o <> data_reader(i).io.ptr_agu_i
      address_gen_unit(i).io.data_move_done <> data_reader(i).io.data_move_done
    } else {
      address_gen_unit(i).io.ptr_o <> data_writer(
        i - dataReaderNum
      ).io.ptr_agu_i
      address_gen_unit(i).io.data_move_done <> data_writer(
        i - dataReaderNum
      ).io.data_move_done
    }
  }

  // data reader and data writer <> accelerator interface
  // with a queue between each data mover and accelerator data decoupled interface
  for (i <- 0 until dataMoverNum) {
    if (i < dataReaderNum) {
      io.data_accelerator_o.data(i) <> Queue(
        data_reader(i).io.data_fifo_o,
        fifoDepthReader(i)
      )
    } else {
      data_writer(
        i - dataReaderNum
      ).io.data_fifo_i <> Queue(
        io.data_accelerator_i.data(i - dataReaderNum),
        fifoDepthWriter(i - dataReaderNum)
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
  val read_flatten_seq = flattenSeq(dataReaderTcdmPorts)
  for ((dimIndex, innerIndex, flattenedIndex) <- read_flatten_seq) {
    // read request to TCDM
    io.tcdm_req(flattenedIndex).valid := data_reader(dimIndex).io
      .read_tcmd_valid_o(innerIndex)
    io.tcdm_req(flattenedIndex).bits.addr := data_reader(dimIndex).io
      .tcdm_req_addr(innerIndex)
    io.tcdm_req(flattenedIndex).bits.data := 0.U
    io.tcdm_req(flattenedIndex).bits.write := 0.B

    // request ready signals from TCDM
    data_reader(dimIndex).io
      .tcdm_ready_i(innerIndex) := io.tcdm_req(flattenedIndex).ready

    // signals from TCDM responses
    data_reader(dimIndex).io.data_tcdm_i(innerIndex).valid := io
      .tcdm_rsp(flattenedIndex)
      .valid
    data_reader(dimIndex).io
      .data_tcdm_i(innerIndex)
      .bits := io.tcdm_rsp(flattenedIndex).bits.data
  }

  // data writer <> TCDM write ports
  // TCDM request port bais based on the read TCDM ports number
  val write_flatten_seq = flattenSeq(dataWriterTcdmPorts)
  for ((dimIndex, innerIndex, flattenedIndex) <- write_flatten_seq) {

    // write request to TCDM
    io.tcdm_req(flattenedIndex + tcdm_read_ports_num).valid := data_writer(
      dimIndex
    ).io.write_tcmd_valid_o(innerIndex)
    io.tcdm_req(flattenedIndex + tcdm_read_ports_num).bits.addr := data_writer(
      dimIndex
    ).io.tcdm_req_addr(innerIndex)
    io.tcdm_req(flattenedIndex + tcdm_read_ports_num).bits.data := data_writer(
      dimIndex
    ).io.tcdm_req_data(innerIndex)
    io.tcdm_req(flattenedIndex + tcdm_read_ports_num).bits.write := 1.B

    // request ready signals from TCDM
    data_writer(dimIndex).io.tcdm_ready_i(innerIndex) := io
      .tcdm_req(flattenedIndex + tcdm_read_ports_num)
      .ready
  }

}

// Scala main function for generating system veriog file for different acceleratros
// including GEMM, Post-processing SIMD and MAC engine
object Streamer extends App {
  emitVerilog(
    new Streamer(
      GeMMStreamerParameters.temporalLoopDim,
      GeMMStreamerParameters.temporalLoopBoundWidth,
      GeMMStreamerParameters.addrWidth,
      GeMMStreamerParameters.unrollingDim,
      GeMMStreamerParameters.unrollingFactorReader,
      GeMMStreamerParameters.unrollingDimReader,
      GeMMStreamerParameters.unrollingFactorWriter,
      GeMMStreamerParameters.unrollingDimWriter,
      GeMMStreamerParameters.tcdmDataWidth,
      GeMMStreamerParameters.dataMoverNum,
      GeMMStreamerParameters.dataReaderNum,
      GeMMStreamerParameters.dataWriterNum,
      GeMMStreamerParameters.tcdmPortsNum,
      GeMMStreamerParameters.dataReaderTcdmPorts,
      GeMMStreamerParameters.dataWriterTcdmPorts,
      GeMMStreamerParameters.readElementWidth,
      GeMMStreamerParameters.writeElementWidth,
      GeMMStreamerParameters.fifoWidthReader,
      GeMMStreamerParameters.fifoDepthReader,
      GeMMStreamerParameters.fifoWidthWriter,
      GeMMStreamerParameters.fifoDepthWriter,
      GeMMStreamerParameters.stationarity
    ),
    Array("--target-dir", "generated/streamer/gemm")
  )

  emitVerilog(
    new Streamer(
      PostProcessingStreamerParameters.temporalLoopDim,
      PostProcessingStreamerParameters.temporalLoopBoundWidth,
      PostProcessingStreamerParameters.addrWidth,
      PostProcessingStreamerParameters.unrollingDim,
      PostProcessingStreamerParameters.unrollingFactorReader,
      PostProcessingStreamerParameters.unrollingDimReader,
      PostProcessingStreamerParameters.unrollingFactorWriter,
      PostProcessingStreamerParameters.unrollingDimWriter,
      PostProcessingStreamerParameters.tcdmDataWidth,
      PostProcessingStreamerParameters.dataMoverNum,
      PostProcessingStreamerParameters.dataReaderNum,
      PostProcessingStreamerParameters.dataWriterNum,
      PostProcessingStreamerParameters.tcdmPortsNum,
      PostProcessingStreamerParameters.dataReaderTcdmPorts,
      PostProcessingStreamerParameters.dataWriterTcdmPorts,
      PostProcessingStreamerParameters.readElementWidth,
      PostProcessingStreamerParameters.writeElementWidth,
      PostProcessingStreamerParameters.fifoWidthReader,
      PostProcessingStreamerParameters.fifoDepthReader,
      PostProcessingStreamerParameters.fifoWidthWriter,
      PostProcessingStreamerParameters.fifoDepthWriter,
      PostProcessingStreamerParameters.stationarity
    ),
    Array("--target-dir", "generated/streamer/pp")
  )
  emitVerilog(
    new Streamer(
      MACStreamerParameters.temporalLoopDim,
      MACStreamerParameters.temporalLoopBoundWidth,
      MACStreamerParameters.addrWidth,
      MACStreamerParameters.unrollingDim,
      MACStreamerParameters.unrollingFactorReader,
      MACStreamerParameters.unrollingDimReader,
      MACStreamerParameters.unrollingFactorWriter,
      MACStreamerParameters.unrollingDimWriter,
      MACStreamerParameters.tcdmDataWidth,
      MACStreamerParameters.dataMoverNum,
      MACStreamerParameters.dataReaderNum,
      MACStreamerParameters.dataWriterNum,
      MACStreamerParameters.tcdmPortsNum,
      MACStreamerParameters.dataReaderTcdmPorts,
      MACStreamerParameters.dataWriterTcdmPorts,
      MACStreamerParameters.readElementWidth,
      MACStreamerParameters.writeElementWidth,
      MACStreamerParameters.fifoWidthReader,
      MACStreamerParameters.fifoDepthReader,
      MACStreamerParameters.fifoWidthWriter,
      MACStreamerParameters.fifoDepthWriter,
      MACStreamerParameters.stationarity
    ),
    Array("--target-dir", "generated/streamer/mac")
  )
}
