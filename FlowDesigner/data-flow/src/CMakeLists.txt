
SET(LIBFLOW_SRCS 
	Node.cc 
	BufferedNode.cc 
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
	Object.cc
	Load.cc
	ParameterSet.cc
	Constant.cc
	Network.cc
	Collector.cc
	Iterator.cc
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
	Vector.cc
	VectorSetIndex.cc
	VectorGetIndex.cc
	variables.cc
	VarLoad.cc
	VarStore.cc
	ExternalApp.cc
	IterCount.cc
	Greater.cc
	Smaller.cc
	Delay.cc
	Iterate.cc
	MatProduct.cc
	Matrix.cc
	DoubleDispatch.cc
	Add.cc
	Sub.cc
	Mul.cc
	Div.cc
	Length.cc
	UIDocument.cc
	UILink.cc
	UINetTerminal.cc
	UINetwork.cc
	UINode.cc
	UINodeParameters.cc
	UITerminal.cc
	IF.cc
	Feedback.cc
	Throw.cc
	Catch.cc
	Action.cc
	net_types.cc
	Max.cc
	Min.cc
	IterWall.cc
	fmath.cc
	Float2Vect.cc
	StrCat.cc
	vmeth.cc
	IntfNode.cc
	NewAccumulator.cc
	Accumulate.cc
	Equal.cc
	mathfunc.cc
	VMethod.cc
	ReadInt.cc
	ReadString.cc
	DCVector.cc
	Trace.cc
	Index.cc
	Index2D.cc
	Concat.cc
	object_param.cc
	binio.cc
	Serialize.cc
	stream_wrap.cc
	Stdin.cc
	Stdout.cc
	Stderr.cc
	Recover.cc
	LoadDoc.cc
	BuildDoc.cc
	SampleAndHold.cc
	UINodeRepository.cc
	DCMatrix.cc
	flow_pref.cc
	flow_version.cc
	Round.cc
	CompositeType.cc
	MakeComposite.cc
	GetComposite.cc
	CompositeEmpty.cc
	AppendComposite.cc
	isNil.cc
	XPMTest.cc
	PushBack.cc
	BinaryAND.cc
	BinaryOR.cc
	Power.cc
	UINote.cc
	path.cc
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
	XPMTest.cc	
)

SET(NETWORK_SRCS
	Network.cc
	Iterator.cc
	IterCount.cc
	IterWall.cc
	Iterate.cc
#	LoadDoc.cc
#	BuildDoc.cc
)

SET(UI_SRCS
#	UIDocument.cc
#	UILink.cc
#	UINetTerminal.cc
#	UINetwork.cc
#	UINode.cc
#	UINodeParameters.cc
#	UITerminal.cc
#	UINodeRepository.cc
#	UINote.cc
#	flow_pref.cc	
)

SET (UTILS_SRCS
	ExternalApp.cc	
	path.cc
	fmath.cc
	flow_version.cc
	object_param.cc
)


INCLUDE_DIRECTORIES(../include ${LIBXML2_INCLUDE})
	
#add_library(libflow-dynamic STATIC ${LIBFLOW_SRCS})

set(FULL_SRCS ${DATATYPES_SRCS} ${NODES_SRCS} ${NETWORK_SRCS} ${UI_SRCS} ${UTILS_SRCS} )

add_library(libflow-dynamic STATIC ${FULL_SRCS})
	
set_target_properties(libflow-dynamic PROPERTIES OUTPUT_NAME libflow)
target_link_libraries(libflow-dynamic ${LIBXML2_LIBRARIES})


#baseflow (test)
#add_executable(baseflow main.cc ${DATATYPES_SRCS} ${NODES_SRCS} ${NETWORK_SRCS} ${UI_SRCS} ${UTILS_SRCS})
add_executable(baseflow main.cc)
target_link_libraries(baseflow libflow-dynamic ${LIBXML2_LIBRARIES})



install(TARGETS libflow-dynamic DESTINATION bin)
install(TARGETS baseflow DESTINATION bin
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib  
  )


  
#EXECUTE_PROCESS(COMMAND ${PERL} info2def.pl ${LIBFLOW_SRCS} > ${CMAKE_CURRENT_BINARY_DIR}/libflow.def)

