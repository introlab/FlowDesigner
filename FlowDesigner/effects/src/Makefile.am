override includedir=${prefix}/include/overflow
override datadir=${prefix}/share/overflow
override libdir=${prefix}/share/overflow/lib

## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.14 2003/10/01 23:25:38 maestro Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libeffects.la 

install-data-local:
	mkdir -p $(datadir)/toolbox/effects
	(cd $(libdir); rm -f effects.tlb; ln -s libeffects.so effects.tlb)
	(cd $(top_srcdir); perl ../tools/src/info2def.pl src/*.cc include/*.h > $(datadir)/toolbox/effects/effects.def)

libeffects_la_SOURCES =	Saturate.cc rev.cc Reverb.cc

libeffects_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/../data-flow/include -I$(top_srcdir)/include 

LDADD = $(top_srcdir)/../data-flow/src/libflow.la 


