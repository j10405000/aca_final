# aca_final

Add direct map cache (write through) + write buffer to improve the performance.
Baseline every load/store instruction need to go through memory and freeze 50 cycles.

How to build:
  1.Clone repo
    $git clone https://github.com/j10405000/aca_final.git
    $cd riscv-sodor
    $git submodule update --init --recursive

  2.Compiling front-end server (will install RISC-V front-end server library under ${RISCV_FESV})
    $cd riscv-fesvr
    $./configure --prefix=/usr/local 
    $make install

  3.Build the sodor emulators
    $cd ..
    $ ./configure --with-riscv=/usr/local 
    $ make
    All processors are installed under the directory “riscv-sodor/emulator”

  4.Run all processors
    $make run-emulator
    $Run a specific processor
    $cd emulator/${PROCESSOR_NAME}
    $make run
