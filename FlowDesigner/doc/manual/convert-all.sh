#!/bin/sh
export VFLOW_PATH=
convert vflow.png vflow.ps
convert codegen.png codegen.ps
convert overflow_IO.png overflow_IO.ps
convert overflow_if.png overflow_if.ps
convert overflow_hello.png overflow_hello.ps
convert overflow_min.png overflow_min.ps
convert overflow_textProbe.png overflow_textProbe.ps
convert overflow_matrix.png overflow_matrix.ps
convert overflow_vector.png overflow_vector.ps
convert overflow_main.png overflow_main.ps
convert overflow_subnet.png overflow_subnet.ps
convert overflow_iterator2.png overflow_iterator2.ps
convert overflow_main3.png overflow_main3.ps
convert overflow_thread.png overflow_thread.ps
convert overflow_feedback.png overflow_feedback.ps
texflow | sed 's/_/\\_/g' > nodes.tex
