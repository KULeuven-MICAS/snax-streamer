# Flexible Streamer for SNAX

This repository contains the RTL code to build a parallel data reader and writer -- streamer for an accelerator X, integrating into the [SNAX core](https://github.com/KULeuven-micas/snitch_cluster). This repository contains the chisel sources and unit tests for the GEMM accelerator.
It is written in CHISEL 5.0.0 and is intended to be connected to the SNAX accelerator RISC-V manager core through a SystemVerilog wrapper.

It is highly flexible and parameterizable which means it is suitable for a range of accelerators with agreed constraints.
