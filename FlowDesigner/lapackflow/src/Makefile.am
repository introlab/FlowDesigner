lib_LTLIBRARIES = liblapackflow.la 

install:
	(cd $(libdir); rm -f lapackflow.tlb; ln -s liblapackflow.so lapackflow.tlb)
	mkdir -p $(datadir)/toolbox/lapackflow
	perl $(top_srcdir)/../tools/src/info2def.pl $(top_srcdir)/src/*.cc > $(datadir)/toolbox/lapackflow/lapackflow.def

liblapackflow_la_SOURCES = lapackflow.cc \
	flapack.f \
	LinSolve.cc \
	Eig.cc

liblapackflow_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/../data-flow/include -I$(top_srcdir)/include 

LDADD = $(top_srcdir)/../data-flow/src/libflow.la -llapack -lblas


