## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.21 2000/07/14 22:34:01 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

install:
	mkdir -p $(prefix)/toolbox/data-flow
	perl $(top_srcdir)/tools/src/info2def.pl $(top_srcdir)/data-flow/src/* > $(prefix)/toolbox/data-flow/data-flow.def

lib_LTLIBRARIES = libnetwork.la

# Sources for compilation in the library
libnetwork_la_SOURCES = Node.cc \
	BufferedNode.cc \
        Object.cc \
	Load.cc \
        ParameterSet.cc \
        Constant.cc \
        Network.cc \
        DLManager.cc \
        Collector.cc \
        Switch.cc \
	NotDone.cc \
        Mux.cc \
        NetExec.cc \
        PathList.cc \
        Iterator.cc \
        InputStream.cc \
        OutputStream.cc \
        Save.cc \
        Sum.cc \
        VSum.cc \
        VNSum.cc \
        OR.cc \
	IsValid.cc \
        AND.cc \
        NOT.cc \
        List.cc \
        Pack.cc \
        UnPack.cc \
	ConstantVector.cc \
	Discard.cc \
	ExecStream.cc \
	ThreadedIterator.cc \
	NOP.cc \
	Vector.cc \
	variables.cc \
	VarLoad.cc \
	FrameOperation.cc \
	VarStore.cc \
	Send.cc \
	Receive.cc \
	ExternalApp.cc \
	IterCount.cc \
	Greater.cc


#libnetwork_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libnetwork_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/data-flow/include
