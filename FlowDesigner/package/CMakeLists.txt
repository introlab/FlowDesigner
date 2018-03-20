IF (WIN32)
	#copy libxml
	SET(QTBIN_DIR ${QT_LIBRARY_DIR}/../bin)
	
	SET(DEVCPP_DLL
	C:/MinGW/bin/libxml2.dll
 	C:/MinGW/bin/iconv.dll
 	C:/MinGW/bin/mingwm10.dll
 	C:/MinGW/bin/zlib1.dll
	${QTBIN_DIR}/Qt3Support4.dll
	${QTBIN_DIR}/QtAssistantClient4.dll
	${QTBIN_DIR}/QtCore4.dll
	${QTBIN_DIR}/QtDesigner4.dll
	${QTBIN_DIR}/QtDesignerComponents4.dll
	${QTBIN_DIR}/QtGui4.dll
	${QTBIN_DIR}/QtNetwork4.dll
	${QTBIN_DIR}/QtOpenGL4.dll
	${QTBIN_DIR}/QtScript4.dll
	${QTBIN_DIR}/QtSql4.dll
	${QTBIN_DIR}/QtSvg4.dll
	${QTBIN_DIR}/QtTest4.dll
	${QTBIN_DIR}/QtXml4.dll
	${QTBIN_DIR}/QtWebKit4.dll
	)
	INSTALL(FILES ${DEVCPP_DLL} DESTINATION bin)
ENDIF(WIN32)




IF(APPLE)
	SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/license.txt")

SET(CPACK_GENERATOR Bundle)

# CPACK_PACKAGE_FILE_NAME - provides the name of the final compressed disk image (the name of the file that is distributed).
# CPACK_PACKAGE_ICON - provides the icon for the mounted disk image (appears after the user mounts the disk image).
# CPACK_BUNDLE_NAME - provides the bundle name (displayed in the finder underneath the bundle icon).
# CPACK_BUNDLE_ICON - provides the bundle icon (displayed in the /Applications folder, on the dock, etc).
# CPACK_BUNDLE_PLIST - path to a file that will become the bundle plist.
# CPACK_BUNDLE_STARTUP_COMMAND - path to a file that will be executed when the user opens the bundle. Could be a shell-script or a binary. 

SET(CPACK_PACKAGE_INSTALL_DIRECTORY "/Applications")
SET(MACOSX_BUNDLE_INFO_STRING "FlowDesigner")
SET(CPACK_PACKAGE_FILE_NAME "FlowDesigner-${VERSION}")
SET(CPACK_BUNDLE_NAME "FlowDesigner")
SET(CPACK_PACKAGE_DEFAULT_LOCATION "/Applications")
SET(CPACK_BUNDLE_PLIST "FlowDesigner.plist")
SET(CPACK_BUNDLE_ICON "FlowDesigner.icns")
SET(CPACK_PACKAGE_ICON "FlowDesigner.icns")
SET(CPACK_BUNDLE_STARTUP_COMMAND "startup.sh")







ELSE(APPLE)
	SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/COPYING.LGPL")
ENDIF(APPLE)


    SET(CPACK_PACKAGE_NAME "FlowDesigner")
    SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "FlowDesigner")
    SET(CPACK_PACKAGE_VENDOR "Dominic Letourneau, Jean-Marc Valin")
    SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")
    SET(CPACK_PACKAGE_CONTACT "flowdesigner-devel@lists.sourceforge.net")

IF(WIN32)
  SET(CPACK_PACKAGE_INSTALL_DIRECTORY "FlowDesigner")
  # There is a bug in NSI that does not handle full unix paths properly. Make
  # sure there is at least one set of four (4) backlasshes.
  # SET(CPACK_PACKAGE_ICON "${CMake_SOURCE_DIR}/Utilities/Release\\\\InstallIcon.bmp")
  # SET(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\FDSetup.exe")
  # SET(CPACK_NSIS_DISPLAY_NAME "${CPACK_PACKAGE_INSTALL_DIRECTORY} FlowDesigner")
  # SET(CPACK_NSIS_HELP_LINK "http:\\\\\\\\flowdesigner.sourceforge.net")
  SET(CPACK_NSIS_URL_INFO_ABOUT "http:\\\\\\\\flowdesigner.sourceforge.net")
  SET(CPACK_NSIS_CONTACT "flowdesigner-devel@lists.sourceforge.net")
  SET(CPACK_NSIS_MODIFY_PATH ON)
  SET(CPACK_PACKAGE_EXECUTABLES "flowdesigner" "FlowDesigner" "batchflow" "batchflow" "qtflow" "qtflow")
ENDIF(WIN32)



INSTALL( FILES ${INSTALLED_EXAMPLES} DESTINATION share/flowdesigner/examples)
INSTALL( FILES ${INSTALLED_TUTORIALS} DESTINATION share/flowdesigner/examples/tutorials)

MESSAGE(STATUS "Writing pkg-config file...")
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/FlowDesigner.pc.cmake ${CMAKE_BINARY_DIR}/FlowDesigner.pc @ONLY)
INSTALL( FILES ${CMAKE_BINARY_DIR}/FlowDesigner.pc DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/pkgconfig )

MESSAGE(STATUS "Writing flowdesigner-config file...")
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/flowdesigner-config.cmake ${CMAKE_BINARY_DIR}/flowdesigner-config @ONLY)
INSTALL( FILES ${CMAKE_BINARY_DIR}/flowdesigner-config DESTINATION ${CMAKE_INSTALL_PREFIX}/bin PERMISSIONS OWNER_READ GROUP_READ WORLD_READ WORLD_EXECUTE)

SET(CPACK_DEBIAN_PACKAGE_NAME "flowdesigner")
SET(CPACK_DEBIAN_PACKAGE_VERSION "snapshot-23_05_2008")
SET(CPACK_DEBIAN_ARCHITECTURE i386)
SET(CPACK_DEBIAN_PACKAGE_DEPENDS "libc6 (>= 2.6-1), libfontconfig1 (>= 2.4.0), libfreetype6 (>= 2.3.5), libgcc1 (>= 1:4.2.1), libice6 (>= 1:1.0.0), libpng12-0 (>= 1.2.13-4), libqt4-core (>= 4.3.2), libqt4-gui (>= 4.3.2), libqt4-network (>= 4.3.2), libqt4-svg (>= 4.3.2), libsm6, libstdc++6 (>= 4.2.1), libx11-6, libxcursor1 (>> 1.1.2), libxext6, libxi6, libxinerama1, libxml2 (>= 2.6.29), libxrandr2 (>= 2:1.2.0), libxrender1, zlib1g (>= 1:1.2.3.3.dfsg-1)")
SET(CPACK_DEBIAN_PACKAGE_MAINTAINER "flowdesigner-devel@lists.sourceforge.net")
SET(CPACK_DEBIAN_PACKAGE_DESCRIPTION "FlowDesigner description not available right now.")
SET(CPACK_DEBIAN_PACKAGE_SECTION "devel")
SET(CPACK_DEBIAN_PACKAGE_PRIORITY "optional")
SET(CPACK_DEBIAN_PACKAGE_RECOMMENDS "")
SET(CPACK_DEBIAN_PACKAGE_SUGGESTS "")

INCLUDE(CPack)
INCLUDE(UseDebian)
 IF(DEBIAN_FOUND)
  ADD_DEBIAN_TARGETS(GDCM)
ENDIF(DEBIAN_FOUND)
