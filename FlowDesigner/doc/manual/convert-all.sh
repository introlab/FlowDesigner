#!/bin/sh
export VFLOW_PATH=
convert vflow.png vflow.ps
convert codegen.png codegen.ps
convert overflow_IO.png overflow_IO.ps
convert overflow_if.png overflow_if.ps
convert overflow_hello.png overflow_hello.ps
texflow | sed 's/_/\\_/g' > nodes.tex
