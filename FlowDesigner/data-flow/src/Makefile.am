## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.28 2000/11/16 05:32:13 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

install:
	mkdir -p $(prefix)/toolbox/data-flow
	perl $(top_srcdir)/tools/src/info2def.pl $(top_srcdir)/data-flow/src/*.cc > $(prefix)/toolbox/data-flow/data-flow.def

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
	Greater.cc \
	Delay.cc \
	Iterate.cc \
	MatProduct.cc \
	Matrix.cc \
	DoubleDispatch.cc \
	operators.cc \
	Add.cc \
	Sub.cc \
	Mul.cc \
	Div.cc \
	FrameAccumulator.cc


#libnetwork_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libnetwork_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/data-flow/include
