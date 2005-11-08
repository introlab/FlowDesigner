override includedir=${prefix}/lib/flowdesigner/toolbox/lapackflow
override datadir=${prefix}/lib/flowdesigner/toolbox/lapackflow
override libdir=${prefix}/lib/flowdesigner/toolbox/lapackflow
override bindir=${prefix}/lib/flowdesigner/toolbox/lapackflow

lib_LTLIBRARIES = liblapackflow.la 

liblapackflow_la_SOURCES = lapackflow.cc \
	flapack.f \
	LinSolve.cc \
	SolveLS.cc \
	Eig.cc \
	SVD.cc

install-data-local:
	mkdir -p $(datadir)
if WITH_CYGWIN
	(cd $(libdir); rm -f lapackflow.tlb; ln -s liblapackflow.dll.a lapackflow.tlb)
else
	(cd $(libdir); rm -f lapackflow.tlb; ln -s liblapackflow.so lapackflow.tlb)
endif
	(perl $(top_srcdir)/../tools/src/info2def.pl $(liblapackflow_la_SOURCES) > $(datadir)/lapackflow.def)


liblapackflow_la_LDFLAGS = -release $(LT_RELEASE) $(CYGWIN_LDFLAGS) \
	$(top_srcdir)/../data-flow/src/libflow.la -llapack -lblas -lm

INCLUDES = -I$(top_srcdir)/../data-flow/include -I$(top_srcdir)/include 

LDADD = $(top_srcdir)/../data-flow/src/libflow.la -llapack -lblas -lm


