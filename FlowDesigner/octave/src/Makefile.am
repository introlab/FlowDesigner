## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.43 2004/02/06 15:42:31 maestro Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

override includedir=${prefix}/lib/flowdesigner/toolbox/Octave
override datadir=${prefix}/lib/flowdesigner/toolbox/Octave
override libdir=${prefix}/lib/flowdesigner/toolbox/Octave

lib_LTLIBRARIES = libOctave.la

install-data-local:
	(cd $(libdir); rm -f Octave.tlb; ln -s libOctave.so Octave.tlb)
	(cd $(top_srcdir); perl ../tools/src/info2def.pl src/*.cc include/*.h > $(datadir)/Octave.def)

# Sources for compilation in the library
libOctave_la_SOURCES = OctaveParser.cc


libOctave_la_LDFLAGS = -release $(LT_RELEASE) -L$(OCTAVE_LIB) -loctave -loctinterp -lcruft \
        -ltermcap -lreadline -ldl -lg2c $(XML2_LIB) -lncurses

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/../data-flow/include -I$(OCTAVE_INCLUDE) -I$(OCTAVE_INCLUDE)/octave

LDADD = $(top_srcdir)/src/libOctave.la \
        $(top_srcdir)/../data-flow/src/libflow.la

bin_PROGRAMS = test

test_SOURCES = test.cc

test_LDADD = -L$(OCTAVE_LIB) -loctave -loctinterp -lcruft \
        -ltermcap -lreadline -ldl -lg2c $(XML2_LIB) -lncurses

LDFLAGS = 
