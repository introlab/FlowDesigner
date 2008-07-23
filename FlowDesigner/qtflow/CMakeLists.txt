#finding qt (last version from CVS)
INCLUDE (  ${FLOWDESIGNER_SOURCE_DIR}/macros/FindQt4.cmake  )
find_package(Qt4 REQUIRED)


#Adding Qt includes
include_directories(${QT_INCLUDE_DIR})

#Adding src subdirectory
add_subdirectory(src)
add_subdirectory(icons)
