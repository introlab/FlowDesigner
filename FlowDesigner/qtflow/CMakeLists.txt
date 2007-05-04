#finding qt
find_package(Qt4 REQUIRED)


#Adding Qt includes
include_directories(${QT_INCLUDE_DIR})

#Adding src subdirectory
add_subdirectory(src)
