lib_LTLIBRARIES = liblapackflow.la 

install:
	(cd $(libdir); rm -f lapackflow.tlb; ln -s liblapackflow.so lapackflow.tlb)
	mkdir -p $(datadir)/toolbox/lapackflow
	(cd $(top_srcdir); perl ../tools/src/info2def.pl src/*.cc include/*.h > $(datadir)/toolbox/lapackflow/lapackflow.def)

liblapackflow_la_SOURCES = lapackflow.cc \
	flapack.f \
	LinSolve.cc \
	SolveLS.cc \
	Eig.cc \
	SVD.cc

liblapackflow_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/../data-flow/include -I$(top_srcdir)/include 

LDADD = $(top_srcdir)/../data-flow/src/libflow.la -llapack -lblas


