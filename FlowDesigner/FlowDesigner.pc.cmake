prefix=@CMAKE_INSTALL_PREFIX@

Name: @PACKAGE@
Description: FlowDesigner
Version: @VERSION@
Requires:
Libs: -L${prefix}/lib -lflow
Cflags: -I${prefix}/include/flowdesigner

