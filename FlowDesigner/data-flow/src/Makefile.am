## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.15 2000/03/01 21:30:59 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libnetwork.la

# Sources for compilation in the library
libnetwork_la_SOURCES = Node.cc \
	BufferedNode.cc \
        Object.cc \
	Load.cc \
        ParameterSet.cc \
        NetworkLogger.cc \
        Constant.cc \
        FileParserException.cc \
        Network.cc \
        DLManager.cc \
        Collector.cc \
        Switch.cc \
        FileParser.cc \
        NetworkBuilder.cc \
        FP_node_info_container.cc \
        FP_network_info_container.cc \
        NetworkBuilderException.cc \
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
	Receive.cc


#libnetwork_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libnetwork_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/data-flow/include
