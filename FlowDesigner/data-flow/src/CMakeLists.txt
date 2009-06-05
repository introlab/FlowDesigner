SET (HEADERS
../include/Array.h
../include/BaseException.h
../include/BinaryAND.h
../include/BinaryOR.h
../include/Buffer.h
../include/BufferedNode.h
../include/Collector.h
../include/Complex.h
../include/CompositeType.h
../include/Constant.h
../include/DLManager.h
../include/DoubleDispatch.h
../include/Exception.h
../include/ExceptionObject.h
../include/ExternalApp.h
../include/FlowException.h
../include/IntfNode.h
../include/Iterator.h
../include/JitterMatrix.h
../include/Matrix.h
../include/MailBox.h
../include/Network.h
../include/Node.h
../include/NodeFactory.h
../include/Object.h
../include/ObjectParser.h
../include/ObjectPool.h
../include/ObjectRef.h
../include/Pack.h
../include/ParameterSet.h
../include/PosixMutexLocker.h
../include/Power.h
../include/SocketStream.h
../include/Stream.h
../include/ThreadedIterator.h
../include/Timer.h
../include/UIDocument.h
../include/UILink.h
../include/UINetTerminal.h
../include/UINetwork.h
../include/UINode.h
../include/UINodeParameters.h
../include/UINodeRepository.h
../include/UINote.h
../include/UIProbeLink.h
../include/UITerminal.h
../include/URLHandler.h
../include/UnPack.h
../include/UserException.h
../include/Vector.h
../include/VectorPool.h
../include/binio.h
../include/compile_object.h
../include/conversion.h
../include/cwrapper.h
../include/flow_pref.h
../include/flow_version.h
../include/fmath.h
../include/iextensions.h
../include/misc.h
../include/multithread.h
../include/net_types.h
../include/object_param.h
../include/operators.h
../include/path.h
../include/pseudosem.h
../include/rc_ptrs.h
../include/rtc.h
../include/stream_io.h
../include/stream_wrap.h
../include/sync.h
../include/typemap.h
../include/typetraits.h
../include/variables.h
../include/vec.h
../include/vec_3dnow.h
../include/vec_sse.h
../include/vmethod.h
../include/wrapper.h
)	

SET(DATATYPES_SRCS
	Object.cc
	Vector.cc
	Matrix.cc
	net_types.cc
	conversion.cc 
	operators.cc 
	add_operators.cc
	sub_operators.cc
	mul_operators.cc
	div_operators.cc
	smaller_operators.cc
	equal_operators.cc
	max_operators.cc
	min_operators.cc
	concat_operators.cc
	vmeth.cc
	DoubleDispatch.cc
	binio.cc
	stream_wrap.cc
	variables.cc
	CompositeType.cc
    JitterMatrix.cc
    VMethod.cc
)	

SET(NODES_SRCS
	Node.cc
	BufferedNode.cc
	ParameterSet.cc
	Add.cc
	Sub.cc
	Mul.cc
	Div.cc
	Load.cc
	Constant.cc
	Collector.cc
	InputStream.cc
	OutputStream.cc
	Save.cc
	Sum.cc
	OR.cc
	AND.cc
	NOT.cc
	List.cc
	Pack.cc
	UnPack.cc
	ConstantVector.cc
	Discard.cc
	NOP.cc
	VectorSetIndex.cc
	VectorGetIndex.cc
	VarLoad.cc
	VarStore.cc
	Greater.cc
	Smaller.cc
	Delay.cc
	MatProduct.cc
	Length.cc
	IF.cc
	Feedback.cc
	Throw.cc
	Catch.cc
	Action.cc
	Max.cc
	Min.cc
	Float2Vect.cc
	StrCat.cc
	IntfNode.cc
	NewAccumulator.cc
	Accumulate.cc
	Equal.cc
	ReadInt.cc
	ReadString.cc
	DCVector.cc
	Trace.cc
	Index.cc
	Index2D.cc
	Concat.cc
	Serialize.cc
	Stdin.cc
	Stdout.cc
	Stderr.cc
	Recover.cc
	SampleAndHold.cc
	DCMatrix.cc
	Round.cc
	MakeComposite.cc
	GetComposite.cc
	CompositeEmpty.cc
	AppendComposite.cc
	isNil.cc
	PushBack.cc
	BinaryAND.cc
	BinaryOR.cc
	Power.cc

)



SET(NETWORK_SRCS
	Network.cc
	Iterator.cc
	IterCount.cc
	IterWall.cc
	Iterate.cc
	LoadDoc.cc
	BuildDoc.cc
)




SET(UI_SRCS
	UIDocument.cc	
	UILink.cc
	UINetTerminal.cc
	UINetwork.cc
	UINode.cc
	UINodeParameters.cc
	UITerminal.cc
	UINodeRepository.cc
	UINote.cc
	UIProbeLink.cc
	flow_pref.cc	
)

SET (UTILS_SRCS
	ExternalApp.cc	
	path.cc
	fmath.cc
	flow_version.cc
	object_param.cc
	iextensions.cc
        
)


IF(UNIX)
      #ADDITIONNAL FILES FOR COMPILATION UNDER UNIX/LINUX/OSX
      SET(NETWORK_SRCS 
         ${NETWORK_SRCS}
         ThreadedIterator.cc
         ThreadJoin.cc
         SerialThread.cc
         ParallelThread.cc
	     ExecStream.cc
	     USleep.cc
	     Timer.cc
         URLHandler.cc
         URLStream.cc
         TCPConnect.cc
      )

      SET (UTILS_SRCS
         ${UTILS_SRCS}
         DLManager.cc
		 GetMessageNode.cc
		 PushToMailBoxNode.cc
	     PopFromMailBoxNode.cc
	     SetMessageNode.cc
		 MailBox.cc
      )
   
      SET (UNIX_ADDITIONAL_LIBRARIES
         -lpthread
      )

ENDIF(UNIX)

IF (WIN32)

	find_package(Qt4 REQUIRED)

	SET(QT_USE_QTNETWORK TRUE)
	SET(QT_USE_QTSVG TRUE)
	SET(QT_USE_QTXML TRUE)
	SET(QT_USE_QTWEBKIT TRUE)

	include(${QT_USE_FILE})

	SET (WIN32_ADDITIONAL_LIBRARIES
		${QT_LIBRARIES}
	)
ENDIF (WIN32)

INCLUDE_DIRECTORIES(../include ${LIBXML2_INCLUDE_DIR})
	
set(FULL_SRCS ${DATATYPES_SRCS} ${NODES_SRCS} ${NETWORK_SRCS} ${UI_SRCS} ${UTILS_SRCS} ${HEADERS})

add_library(libflow-dynamic SHARED ${FULL_SRCS} )
	
set_target_properties(libflow-dynamic PROPERTIES VERSION ${CPACK_PACKAGE_VERSION} SOVERSION ${CPACK_PACKAGE_VERSION} OUTPUT_NAME flow)

target_link_libraries(libflow-dynamic ${LIBXML2_LIBRARIES} ${UNIX_ADDITIONAL_LIBRARIES} ${WIN32_ADDITIONAL_LIBRARIES})

add_executable(batchflow batchflow.cc)
target_link_libraries(batchflow libflow-dynamic ${LIBXML2_LIBRARIES} ${UNIX_ADDITIONAL_LIBRARIES} ${WIN32_ADDITIONAL_LIBRARIES})



install(TARGETS libflow-dynamic DESTINATION lib
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib 
 )
  
install(TARGETS batchflow DESTINATION bin
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib  
  )
  

#create directory
INSTALL( CODE "FILE(MAKE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FD_TOOLBOX_DIR}/LIBFLOW)")
#INSTALLING .def file 
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/data-flow/src OUTPUT_FILE libflow.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${FULL_SRCS})" )
INSTALL( FILES libflow.def DESTINATION ${FD_TOOLBOX_DIR}/LIBFLOW/)

