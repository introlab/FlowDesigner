#!/bin/sh
export VFLOW_PATH=
convert vflow.png vflow.ps
convert codegen.png codegen.ps
convert overflow_IO.png overflow_IO.ps
texflow | sed 's/_/\\_/g' > nodes.tex
