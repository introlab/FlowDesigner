## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.7 2006/01/18 16:50:54 maestro Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

override includedir=${prefix}/lib/flowdesigner/toolbox/Octave
override datadir=${prefix}/lib/flowdesigner/toolbox/Octave
override libdir=${prefix}/lib/flowdesigner/toolbox/Octave
override bindir=${prefix}/lib/flowdesigner/toolbox/Octave

lib_LTLIBRARIES = libOctave.la

# Sources for compilation in the library
libOctave_la_SOURCES = OctaveParser.cc \
	OctaveFullScriptPath.cc

install-data-local:
	echo "octave lib : -L $(OCTAVE_LIB)"

	mkdir -p $(datadir)
if WITH_CYGWIN
	(cd $(libdir); rm -f Octave.tlb; ln -s libOctave.dll.a Octave.tlb)
else
	(cd $(libdir); rm -f Octave.tlb; ln -s libOctave.so Octave.tlb)
endif
	(perl $(top_srcdir)/../tools/src/info2def.pl $(libOctave_la_SOURCES) > $(datadir)/Octave.def)

libOctave_la_LDFLAGS = -release $(LT_RELEASE) $(CYGWIN_LDFLAGS)  $(top_srcdir)/../data-flow/src/libflow.la -L$(OCTAVE_LIB) -loctave -loctinterp -lcruft \
        -ltermcap -lreadline -ldl -lg2c $(XML2_LIB) -lncurses

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/../data-flow/include -I$(OCTAVE_INCLUDE) -I$(OCTAVE_INCLUDE)/octave


bin_PROGRAMS = test

test_SOURCES = test.cc

test_LDADD = -L$(OCTAVE_LIB) -loctave -loctinterp -lcruft \
        -ltermcap -lreadline -ldl -lg2c $(XML2_LIB) -lncurses

