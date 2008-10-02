#finding qt (last version from CVS)
find_package(Qt4 REQUIRED)


#Adding Qt includes
include_directories(${QT_INCLUDE_DIR})

#Adding src subdirectory
add_subdirectory(src)
add_subdirectory(include)
add_subdirectory(icons)
