## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.2 1999/09/05 03:13:31 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libnetwork.la

# Sources for compilation in the library
libnetwork_la_SOURCES = Node.cc \
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
        MuxNode.cc \
        NetExecNode.cc \
        NetPathList.cc \
        Iterator.cc \
        InputStream.cc \
        OutputStream.cc \
        Save.cc \
        Sum.cc \
        VSum.cc \
        VNSum.cc \
        ORNode.cc \
        ANDNode.cc \
        NOTNode.cc \
        Object.cc \
        List.cc \
        Pack.cc \
        UnPack.cc 


#libnetwork_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libnetwork_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/data-flow/include
