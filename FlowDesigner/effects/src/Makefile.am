override includedir=${prefix}/lib/flowdesigner/toolbox/effects
override datadir=${prefix}/lib/flowdesigner/toolbox/effects
override libdir=${prefix}/lib/flowdesigner/toolbox/effects

## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.16 2004/01/13 22:12:42 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libeffects.la 

install-data-local:
	mkdir -p $(libdir)
	(cd $(libdir); rm -f effects.tlb; ln -s libeffects.so effects.tlb)
	(cd $(top_srcdir); perl ../tools/src/info2def.pl src/*.cc include/*.h > $(datadir)/effects.def)

libeffects_la_SOURCES =	Saturate.cc rev.cc Reverb.cc

libeffects_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/../data-flow/include -I$(top_srcdir)/include 

LDADD = $(top_srcdir)/../data-flow/src/libflow.la 


