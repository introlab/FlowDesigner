## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.101 2002/01/16 14:48:58 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

install:
	mkdir -p $(datadir)/toolbox/data-flow
	(cd $(top_srcdir); perl tools/src/info2def.pl data-flow/src/*.cc data-flow/include/*.h > $(datadir)/toolbox/data-flow/data-flow.def)

lib_LTLIBRARIES = libflow.la

# Sources for compilation in the library
libflow_la_SOURCES = Node.cc \
	BufferedNode.cc \
	Object.cc \
	Load.cc \
	ParameterSet.cc \
	Constant.cc \
	Network.cc \
	DLManager.cc \
        Collector.cc \
        NetExec.cc \
	PathList.cc \
	Iterator.cc \
        InputStream.cc \
        OutputStream.cc \
        Save.cc \
	Sum.cc \
	OR.cc \
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
	VarStore.cc \
	ExternalApp.cc \
	IterCount.cc \
	Greater.cc \
	Smaller.cc \
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
	Length.cc \
	UIDocument.cc \
	UILink.cc \
	UINetTerminal.cc \
	UINetwork.cc \
	UINode.cc \
	UINodeParameters.cc \
	UITerminal.cc \
	IF.cc \
	Feedback.cc \
	Throw.cc \
	Catch.cc \
	Action.cc \
	net_types.cc \
	Max.cc \
	Min.cc \
	IterWall.cc \
	fmath.cc \
	Float2Vect.cc \
	StrCat.cc \
	vmeth.cc \
	SerialThread.cc \
	ParallelThread.cc \
	ThreadJoin.cc \
	path.cc \
	IntfNode.cc \
	wrapper.cc \
	cwrapper.cc \
	NewAccumulator.cc \
	Accumulate.cc \
	Equal.cc \
	mathfunc.cc \
	VMethod.cc \
	ReadInt.cc \
	DCVector.cc \
	Trace.cc \
	Index.cc \
	Concat.cc \
	object_param.cc \
	binio.cc \
	Serialize.cc \
	stream_wrap.cc \
	Stdin.cc \
	Stdout.cc \
	Stderr.cc \
	Recover.cc \
	LoadDoc.cc \
	BuildDoc.cc \
	SocketStream.cc \
	Socket.cc \
	Listen.cc \
	Connect.cc \
	Accept.cc \
	SampleAndHold.cc \
	iextensions.cc \
	UINodeRepository.cc \
	DCMatrix.cc \
	flow_pref.cc \
	flow_version.cc \
	Round.cc \
	Sleep.cc \
	compile_object.cc \
	VectorCode.cc \
	CompositeType.cc \
	MakeComposite.cc \
	GetComposite.cc \
	rtc.cc



#libflow_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libflow_la_LDFLAGS = -release $(LT_RELEASE)
#libflow_LDADD = $(GNOME_XML_LIB)

bin_PROGRAMS = batchflow flowdoc codeflow texflow

batchflow_SOURCES = \
	batchflow.cc

texflow_SOURCES = \
	texflow.cc

flowdoc_SOURCES = \
	flowdoc.cc

codeflow_SOURCES = codeflow.cc

batchflow_LDADD = $(GNOME_XML_LIB) libflow.la $(LIB_FFTW)

texflow_LDADD = $(GNOME_XML_LIB) libflow.la 

flowdoc_LDADD = $(GNOME_XML_LIB) libflow.la 

codeflow_LDADD = $(GNOME_XML_LIB) libflow.la 

batchflow_LDFLAGS = $(FFTW_LDFLAGS)

texflow_LDFLAGS = $(FFTW_LDFLAGS)

INCLUDES = -I$(top_srcdir)/data-flow/include $(GNOME_XML_INCLUDE) 
