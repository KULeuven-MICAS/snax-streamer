# Flexible Streamer Generator for SNAX

The Flexible Streamer Generator is built to alleviate the data layout concern of an accelerator. It decouples the the accelerator with the real memory system. It simplifies the data interface of the accelerator. The accelerator don't need to consider the complicate request and response to/from the real memory system. The accelerator's data interface is simply valid-ready decoupled handshake interface. Besides, the pre-fetch mechanism inside the Streamer also helps to relieve the data contention problem.

The Streamer has a compatible interface with the [SNAX core](https://github.com/KULeuven-micas/snitch_cluster) and can be integrated into it as a data streaming engine. This repository contains the chisel sources and unit tests for the Flexible Streamer Generator.

It is written in CHISEL 5.0.0 and is intended to be connected to the SNAX accelerator RISC-V manager core through a SystemVerilog wrapper.

Thanks to strong expresion power of Chisel, the Flexible Streamer Generator has the capability of dealing with any number of temporal loops and any number of parallel loops for the data address generation. More importantly, the Streamre generator is accelerator agnostic and highly flexible and parameterizable which means it is suitable for a range of accelerators with [tiled-strided-layout](https://github.com/KULeuven-MICAS/snax-mlir/tree/main/compiler/ir/tsl). We have generated Streamers for three accelerators, including the SNAX-GEMM, SNAX-PostProcessing-SIMD, and MAC Engine.

## Microarchitecture
The microarchitecture of the Flexible Streamer Generator is shown below.

The main module of the Streamer is the data mover, including data reader (read data from the real memory system and act as the producer for the accelerator) and data writer (write data to the real memory system and act as consumer for the accelerator). Each data mover has its own address generation unit and works independently so that each data mover can do the data transfer as quick as possible.

The data reader gives read request to the memory system (the address, and read signal, etc.) and get the response. When all the responses of one transaction are obtained, it push the valid data into a FIFO for accelerator to consume. If the accelerator consumed successfully, the FIFO will pop the data. The data reader wll keep get new data from the real memory system (pre-fetch) until the FIFO is full or all the input data has been fetched.

The data writer obtains valid data from the FIFO output (written by the accelerator) and send write request to the memory system. When there is a valid output from the accelerator (and the FIFO is not full), the valid output will be pushed to the FIFO. When the FIFO is not empty, the data writer will keep sending write request. When one write transaction is success, the data will be pop from the FIFO.

The StreamerTop module has a csrManage to manage the CSR read/write operations from the SNAX core. The Streamer has its own CSRs to store the configuration for the address generation and transaction number etc. The configuration, such as the temporal loop bound and strides, is written in the CSRs via a CsrManager when all the CSR configurations are valid. When doing the current transaction, the configuration for the next transaction operation can already be written into the CsrManager. When the current transaction finishes, the SNAX core can send the configuration valid signal then the CSR value in the CsrManager will be loaded in to the Streamer.

<p align="center">
  <img src="./docs/microarch.svg" alt="">
</p>

## Parameters
All the parameters for the Streamer is listed at `Parameters.scala`. Each hardware module has its own parameter class. A description of all the parameter class and the parameter format for the Streamer is listed at the below table.

|Class| Parameters | Description |
| - | - | - |
|CommonParams | addrWidth |  The bit width of the memory system address. |
| | tcdmDataWidth |  Data width for each TCDM* port. |
|TemporalAddrGenUnitParams | loopDim |  The dimension of the temporal loops = the number of for loops. |
| | loopBoundWidth |  The bit width of the loop bounds. |
| | addrWidth |   The bit width of the memory system address. |
|SpatialAddrGenUnitParams | loopDim |  The number of nested spatial(parfor) loops. |
| | loopBounds |  The bounds of each spatial(parfor) loop dimension. |
| | addrWidth |   The bit width of the memory system address. |
| DataMoverParams | tcdmPortsNum | The number of TCDM ports connected to each data mover. |
| | spatialBounds | Spatial unrolling factors (your parfor) for each data mover. |
| | spatialDim | The dimension of spatial unrolling factors (your parfor) for each data mover. |
| | elementWidth | Single data element width for each data mover, useful for generating spatial addresses. |
| | fifoWidth |FIFO width. |
| StreamerParams | temporalAddrGenUnitParams | The parameters for the temporal address generation unit. Even though there are independent temporal address generation unit for each data mover, each data mover actually share the same temporal address generation unit parameters. |
|  | stationarity | The parameters for stationarity for each data mover. If the stationarity bit is set, the innermost loop for that data mover is set to 1.|
|  | dataReaderParams | A sequence of the parameters for dataReader. Each dataReader parameter can be different. The number of the parameters can also be any, meaning there is no constraint on how many the dataReader should be. |
|  | dataWriterParams | Similar as dataReaderParams. A sequence of the parameters for dataWriter. Each dataWriter parameter can be different. The number of the parameters can also be any, meaning there is no constraint on how many the dataWriter should be. |
| | fifoReaderParams | A sequence of the parameters for the FIFOs for dataReaders. The sequence length should matches the length of dataReaderParams.|
| | fifoWriterParams | A sequence of the parameters for the FIFOs for dataWriters. The sequence length should matches the length of dataWriterParams.|

*TCDM is the memory that the Streamer interacts to.

### Instantiate Streamer for there accelerators
`StreamerInstanceParameters.scala` lists three Streamer parameter configuration for three different accelerators.

#### Example: Streamer parameter configuration for a simple MAC Engine
```
object MacStreamerParameters extends CommonParams {

  def MacScalingFactor = 4

  def temporalAddrGenUnitParams: TemporalAddrGenUnitParams =
    TemporalAddrGenUnitParams(
      loopDim = 1,
      loopBoundWidth = 8,
      addrWidth
    )

  def fifoReaderParams: Seq[FIFOParams] = Seq(
    FIFOParams(64 * MacScalingFactor, 2),
    FIFOParams(64 * MacScalingFactor, 2),
    FIFOParams(64, 2)
  )

  def fifoWriterParams: Seq[FIFOParams] = Seq(
    FIFOParams(64 * MacScalingFactor, 2)
  )

  def stationarity = Seq(0, 0, 1, 1)

  def dataReaderParams: Seq[DataMoverParams] = Seq(
    DataMoverParams(
      tcdmPortsNum = 1 * MacScalingFactor,
      spatialBounds = Seq(2 * MacScalingFactor),
      spatialDim = 1,
      elementWidth = 32,
      fifoWidth = fifoReaderParams(0).width
    ),
    DataMoverParams(
      tcdmPortsNum = 1 * MacScalingFactor,
      spatialBounds = Seq(2 * MacScalingFactor),
      spatialDim = 1,
      elementWidth = 32,
      fifoWidth = fifoReaderParams(1).width
    ),
    DataMoverParams(
      tcdmPortsNum = 1,
      spatialBounds = Seq(2),
      spatialDim = 1,
      elementWidth = 32,
      fifoWidth = fifoReaderParams(2).width
    )
  )

  def dataWriterParams: Seq[DataMoverParams] = Seq(
    DataMoverParams(
      tcdmPortsNum = 1 * MacScalingFactor,
      spatialBounds = Seq(2 * MacScalingFactor),
      spatialDim = 1,
      elementWidth = 32,
      fifoWidth = fifoWriterParams(0).width
    )
  )

}
```
## Functional description

Based on the definition of [tiled-strided-layout](https://github.com/KULeuven-MICAS/snax-mlir/tree/main/compiler/ir/tsl), the address generation is base on two parts, the temporal address and spatial address. The temporal address is based on the temporal loop counters and the temporal strides configuration. The spatial address is based on the spatial unrolling factor of the accelerator and the spatial strides configuration. As the accelerator has spatial unrolling, the address generation unit will generate the address for each data element. But when sending the request to the real memory system, these addresses will be merged for data alignment for the memory bank width.

When formulate the address generation formula below:
```
for(;;temporalbound_n-1)
 for(;;temporalbound_n-2)
   …
    for(;;temporalbound_0)
     parfor(;;spatialbound_m-1)
      parfor(;;spatialbound_m-2)
       …
        parfor(;;spatialbound_0)
          add_o = base_ptr + temporal_address + spatial_address
          temporal_address = (temporal_unrolling_loop_counters.zip(tempStride).map { case (a, b) => a * b }).reduce(_ +& _)
          spatial_address = (spatial_unrolling_loop_counters.zip(spatialStride).map { case (a, b) => a * b }).reduce(_ +& _)
```

Take a SIMD accelerator as an example (1 temporal loop and 1 spatial loop), Vu = 8, the address generation is:
```
for (ti = 0 to VEC_LEN/Vu – 1):
    parfor (si = 0 to Vu -1):
      addr_o[Vu-1:0] = base_ptr + ti*(tempStride) +  [Vu-1:0]*(spatialStride)
```
Which actually translates to:
```
addr_o[0] = base_ptr + ti*(tempStride) +  [0]*(spatialStride)
addr_o[1] = base_ptr + ti*(tempStride) +  [1]*(spatialStride)
addr_o[2] = base_ptr + ti*(tempStride) +  [2]*(spatialStride)
addr_o[3] = base_ptr + ti*(tempStride) +  [3]*(spatialStride)
addr_o[4] = base_ptr + ti*(tempStride) +  [4]*(spatialStride)
addr_o[5] = base_ptr + ti*(tempStride) +  [5]*(spatialStride)
addr_o[6] = base_ptr + ti*(tempStride) +  [6]*(spatialStride)
addr_o[7] = base_ptr + ti*(tempStride) +  [7]*(spatialStride)
```
Take GEMM Accelerator input data A as an example(3 temporal loop and 2 spatial loop), the address generation is:

```
for (m1 = 0 to M’/Mu-1)
    for (n1 = 0 to N’/Nu-1)
        for (k1 = 0 to K’/Ku-1)
            parfor(0 to Mu)
            parfor(0 to Ku)
              addr_o[Mu-1:0][Ku-1:0] = ptr_o + temporal_address + spatial_address
              temporal_address = m1*t_str_m1 + n1*t_str_n1 + k1*t_str_k1
              spatial_address = [Mu-1:0]*s_str_m + [Ku-1:0]*str_str_k

```
where t_str_* - are temporal strides and s_str_* - are spatial strides.

Which actually translates to:
```
addr_o[0][0] = ptr_o + m1*t_str_m1 + n1*t_str_n1 + k1*t_str_k1 + [0]*s_str_m + [0]*str_str_k
addr_o[0][1] = ptr_o + m1*t_str_m1 + n1*t_str_n1 + k1*t_str_k1 + [0]*s_str_m + [1]*str_str_k
addr_o[0][2] = ptr_o + m1*t_str_m1 + n1*t_str_n1 + k1*t_str_k1 + [0]*s_str_m + [2]*str_str_k
…
addr_o[m][k] = ptr_o + m1*t_str_m1 + n1*t_str_n1 + k1*t_str_k1 + [m]*s_str_m + [k]*str_str_k
…
addr_o[Mu-1][Ku-3] = ptr_o + m1*t_str_m1 + n1*t_str_n1 + k1*t_str_k1 + [Mu-1]*s_str_m + [Ku-3]*str_str_k
addr_o[Mu-1][Ku-2] = ptr_o + m1*t_str_m1 + n1*t_str_n1 + k1*t_str_k1 + [Mu-1]*s_str_m + [Ku-2]*str_str_k
addr_o[Mu-1][Ku-1] = ptr_o + m1*t_str_m1 + n1*t_str_n1 + k1*t_str_k1 + [Mu-1]*s_str_m + [Ku-1]*str_str_k
```


## IO ports
The input and output ports of the Streamer is shown at the table below.

The Streamer uses simplified CSR request/response interface for CSR write/read operation. A more detailed description of the CSR operation interface can be found at [here](https://kuleuven-micas.github.io/snitch_cluster/rm/snax_cluster.html).

The Streamer uses simplified TCDM request/response interface to read and write data from/to the TCDM. A more detailed description of the TCDM request/response interface can be found at [here](https://kuleuven-micas.github.io/snitch_cluster/rm/snax_cluster.html).

The Streamer uses the Decoupled interface for accelerator input and output data. A more detailed description of the Decoupled interface can be found at [here](https://www.chisel-lang.org/docs/explanations/interfaces-and-connections#the-standard-ready-valid-interface-readyvalidio--decoupled).

The simplified interface for the CSR and TCDM request/response contains the core signals and can be found at `TypeDefine.scala`. The complete signals for the CSR request/response interface and TCDM request/response interface will be added in the SystemVerilog wrapper.

|Signal bundle| Signals | Signal name in generated SV | Width | Dir | Description |
| - | - | - | - | - | - |
| csr.req | data | io_csr_req_bits_data | 32| In| The write data from CSR request |
|  | addr | io_csr_req_bits_addr | 32| In| The address indicating which CSR to be wrote or read |
|  | write | io_csr_req_bits_write | 32| In| The signal indicates this request is for CSR write or read |
|  | valid | io_csr_req_valid | 32| In| The signal indicates if this request is valid |
|  | ready | io_csr_req_ready | 32| Out| The signal indicates if the accelerator is ready for this CSR operation|
| csr.rsp | data | io_csr_rsp_bits_data | 32| Out| The response data for CSR read operation |
|  | valid | io_csr_rsp_valid | 32| Out| The signal indicates if this response is valid |
|  | ready | io_csr_rsp_ready | 32| In| The signal indicates if the SNAX core is ready for this CSR response |
| tcdm_req | data | io_data_tcdm_req_0_bits_data | 64| Out| The data from TCDM request. This data only valuable when it is a write request. |
| tcdm_req | addr | io_data_tcdm_req_0_bits_addr | 32| Out| The address from TCDM request. |
| tcdm_req | write | io_data_tcdm_req_0_bits_write | 1| Out| The signal indicates this request is for CSR write or read |
| tcdm_req | valid | io_data_tcdm_req_0_valid | 1| Out| The signal indicates if this request is valid |
| tcdm_req | ready | io_data_tcdm_req_0_ready | 1| Int| The signal indicates if the TCDM is ready for this CSR request |
| | . | . | . | . | There can be a large number of tcdm_req ports depending on the spatial unrolling factors for the data readers and the data writers. tcdm_req ports for readers have lower index number. A detailed mapping for the tcdm_req ports and the data mover ports can be found at `Streamer.scala` line 269-311.|
| tcdm_rsp | data | io_data_tcdm_rsp_0_bits_data | 64| In| The response data from the read request |
| tcdm_rsp | valid | io_data_tcdm_rsp_0_valid | 1| In| The signal indicates if this response is valid |
| | . | . | . | . | The tcdm_rsp ports number is the same as tcdm_req. |
| streamer2accelerator | data | io_data_streamer2accelerator_data_0_bits | First read FIFO width| Out | The data for the acceleratorX input |
| streamer2accelerator | valid | io_data_streamer2accelerator_data_0_valid | 1| Out| The signal indicates if this data is valid |
| streamer2accelerator | ready | io_data_streamer2accelerator_data_0_ready | 1| In| The signal indicates if the acceleratorX is ready for this data (has used this data already) |
| | . | . | . | . | The streamer2accelerator ports number is the same as data reader number. The index 0 corresponds to the first input data and 2 for second input data and so on. |
| accelerator2streamer | data | io_data_accelerator2streamer_data_0_bits | First write FIFO width| In| The data from the acceleratorX output |
| accelerator2streamer | valid | io_data_accelerator2streamer_data_0_valid | 1| In| The signal indicates if this data is valid |
| accelerator2streamer | ready | io_data_accelerator2streamer_data_0_ready | 1| In| The signal indicates if the data writer is ready for taking in this data (not full) |
| | . | . | . | . | The accelerator2streamer ports number is the same as data writer number. The index 0 corresponds to the first output data and 2 for second output data (if any) and so on.|
| | | | | | | |

### CSR definition
| Address | CSR name             | Notes                               |
|---------|--------------------------|-------------------------------------|
| offset* + [0..temporalDim - 1]   |    temporalLoopBoundCSRs     | temporal loop bound for each temporal dimension           |
| offset + temporalDim + [0..temdataMoverNum * temporalDimporalDim - 1]   |    temporalLoopSrtidesCSRs     | temporal loop strides for each temporal dimension and for each data mover          |
| offset + temporalDim +  dataMoverNum * temporalDim + [0..spatialDim.sum - 1]   |    spatialLoopSrtidesCSRs     | spatial loop strides for each data mover  and for corresponding spatial dimension. The  spatial dimension for each data mover can be different. It depends on the accelerator.        |
| offset* + temporalDim +  dataMoverNum * temporalDim + spatialDim.sum + [0..dataMoverNum - 1]   |    basePtrCSRs     |  base pointers for each data mover.          |

*offset is defined by the SNAX core. More detailed explanation of what are these configurations can be found at `StreamerTop.scala`.

## Quick start
### Set up Chisel environment
The instruction for setting up Chisel compilation and simulation environment can be found [here](https://github.com/KULeuven-MICAS/snax-gemm?tab=readme-ov-file#set-up-chisel-environment).

## Run tests
There are unit tests for each hardware generator.

To run all the Streamer accelerator tests, use:
```
sbt test
```
To run specific test, use:

```
sbt "testOnly streamer.${chisel_test_name}"
```
where `chisel_test_name` is the class name of the specific test. For instance, use:
```
sbt "testOnly streamer.StreamerTopTest"
```
to run the unit test for Streamer top module.
## Generate System Verilog file
To generate the corresponding system verilog file for specific Chisel module, use:
```
sbt "runMain streamer.${chisel_module_name}"
```
For instance, to generate the system verilog file for Streamer top module with the testing parameters, use:
```
sbt "runMain streamer.StreamerTopTester"
```
