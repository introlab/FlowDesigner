## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.8 2001/03/31 03:58:48 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = liblapackflow.la 

install:
	(cd $(prefix)/lib; rm -f lapackflow.tlb; ln -s liblapackflow.so lapackflow.tlb)
	mkdir -p $(prefix)/toolbox/lapackflow
	perl $(top_srcdir)/../tools/src/info2def.pl $(top_srcdir)/src/*.cc > $(prefix)/toolbox/lapackflow/lapackflow.def

liblapackflow_la_SOURCES = lapackflow.cc

liblapackflow_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/../data-flow/include -I$(top_srcdir)/include 

LDADD = $(top_srcdir)/../data-flow/src/libflow.la 


