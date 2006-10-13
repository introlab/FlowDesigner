## Process this file with automake to produce Makefile.in

override includedir=${prefix}/include/flowdesigner
override datadir=${prefix}/lib/flowdesigner/toolbox/qtflow
override libdir=${prefix}/lib/flowdesigner/toolbox/qtflow
#default bindir is OK


INCLUDES = @PACKAGE_CFLAGS@ \
	$(GNOME_PIXBUF_INCLUDE) \
	-I$(top_srcdir)/../data-flow/include 


bin_PROGRAMS = flowdesigner-qt 

lib_LTLIBRARIES = libbaseProbeQt.la 

flowdesigner_qt_SOURCES = flowdesigner.cc

libbaseProbeQt_la_SOURCES = 

install-data-local:
	mkdir -p $(datadir)
if WITH_CYGWIN		
	(cd $(libdir); rm -f baseProbeQt.tlb; \
	ln -s $(prefix)/lib/flowdesigner/toolbox/bin/cygbaseProbeQt-$(FLOWDESIGNER_MAJOR_VERSION)-$(FLOWDESIGNER_MINOR_VERSION).dll baseProbeQt.tlb)
else
	(cd $(libdir); rm -f baseProbQte.tlb; ln -s libbaseProbeQt.so baseProbeQt.tlb)	
endif
	(perl $(top_srcdir)/../tools/src/info2def.pl $(libbaseProbeQt_la_SOURCES) > $(datadir)/qtflow.def)


libbaseProbeQt_la_LDFLAGS = -release $(LT_RELEASE) $(CYGWIN_LDFLAGS) \
	$(top_srcdir)/../data-flow/src/libflow.la \
	$(PACKAGE_CFLAGS) 


flowdesigner_qt_LDFLAGS = $(FFTW_LDFLAGS) $(CYGWIN_LDFLAGS) $(PACKAGE_LIBS) $(top_srcdir)/../data-flow/src/libflow.la

