prefix=@CMAKE_INSTALL_PREFIX@

Name: @CPACK_PACKAGE_NAME@
Description: FlowDesigner
Version: @CPACK_PACKAGE_VERSION@
Requires:
Libs: -L${prefix}/lib -lflow
Cflags: -I${prefix}/include/flowdesigner

