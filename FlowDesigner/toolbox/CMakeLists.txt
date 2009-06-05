### FuzzyEngine toolbox ###
ADD_SUBDIRECTORY(FuzzyEngine)

### NNet toolbox ###
ADD_SUBDIRECTORY(NNet)

### VQ toolbox ###
ADD_SUBDIRECTORY(VQ)

### HMM toolbox ###
ADD_SUBDIRECTORY(HMM)

### audio_blocks toolbox ###
ADD_SUBDIRECTORY(audio_blocks)

### effects toolbox ###
ADD_SUBDIRECTORY(effects)

### FDOpenCV toolbox ###
find_package(OpenCV)
IF (OpenCV_FOUND)
 #   ADD_SUBDIRECTORY(FDOpenCV)
ELSE (OpenCV_FOUND)
    MESSAGE (STATUS "OpenCV library wasn't found, it won't be compiled.")
ENDIF (OpenCV_FOUND)


### lapackflow toolbox ###
find_package(LAPACK)
IF (HAVE_LAPACK AND NOT APPLE)
    ADD_SUBDIRECTORY(lapackflow)
ELSE (HAVE_LAPACK AND NOT APPLE)
    MESSAGE (STATUS "LAPACK library wasn't found, it won't be compiled.")
ENDIF (HAVE_LAPACK AND NOT APPLE)

### octave toolbox ###
#find_package(Octave)
#IF (HAVE_OCTAVE)
#    ADD_SUBDIRECTORY(octave)
#ELSE (HAVE_OCTAVE)
#    MESSAGE (STATUS "OCTAVE library wasn't found, it won't be compiled.")
#ENDIF (HAVE_OCTAVE)


