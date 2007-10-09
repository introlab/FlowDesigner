## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

EXTRA_DIST = CMakeLists.txt n-files/CMakeLists.txt src/CMakeLists.txt include/CMakeLists.txt

SUBDIRS = src include n-files

check: 
	(cd testcase; ./check.sh)
