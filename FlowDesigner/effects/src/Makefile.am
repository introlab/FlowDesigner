## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.12 2001/08/08 22:41:48 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libeffects.la 

install-data-local:
	(cd $(libdir); rm -f effects.tlb; ln -s libeffects.so effects.tlb)
	mkdir -p $(datadir)/toolbox/effects
	(cd $(top_srcdir); perl ../tools/src/info2def.pl src/*.cc include/*.h > $(datadir)/toolbox/effects/effects.def)

libeffects_la_SOURCES =	Saturate.cc rev.cc Reverb.cc

libeffects_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/../data-flow/include -I$(top_srcdir)/include 

LDADD = $(top_srcdir)/../data-flow/src/libflow.la 


