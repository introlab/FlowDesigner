## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.18 2002/04/10 19:03:21 maestro Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libfuzzy.la

install-data-local:
	(cd $(libdir); rm -f fuzzy.tlb; ln -s libfuzzy.so fuzzy.tlb)
	mkdir -p $(datadir)/toolbox/fuzzy
	(cd $(top_srcdir); perl ../tools/src/info2def.pl src/*.cc include/*.h> $(datadir)/toolbox/fuzzy/fuzzy.def)

# Sources for compilation in the library
libfuzzy_la_SOURCES = FuzzyFunction.cc \
	FuzzyModel.cc \
	FuzzyRule.cc \
	FuzzySet.cc \
	GenericModel.cc \
	TrapezoidalFunction.cc \
	TriangularFunction.cc \
	FuzzyOperators.cc \
	FuzzyModelExec.cc \
	Tokenizer.cc \
	InferenceModel.cc

#libvq_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libfuzzy_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/../data-flow/include

LDADD = $(top_srcdir)/src/libfuzzy.la \
        $(top_srcdir)/../data-flow/src/libflow.la

