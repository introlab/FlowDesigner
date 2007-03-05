	
SET(DATAFLOW_NFILES
	LoadFile.n 
	SaveAs.n 
	SerializeAs.n
	Print.n
	ToInt.n 
	ToFloat.n 
	ToBool.n 
	ToDouble.n 
	ToString.n 
	ToVect.n 
	Modulo.n 
	Step.n 
	Trigger.n 
	NilObject.n
)

INSTALL(FILES ${DATAFLOW_NFILES} DESTINATION ${FD_TOOLBOX_DIR}/LIBFLOW)