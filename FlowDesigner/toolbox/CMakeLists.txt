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
ADD_SUBDIRECTORY(FDOpenCV)

### lapackflow toolbox ###
find_package(LAPACK)
IF (HAVE_LAPACK)
    ADD_SUBDIRECTORY(lapackflow)
ELSE (HAVE_LAPACK)
    MESSAGE (STATUS "LAPACK library wasn't found, it won't be compiled.")
ENDIF (HAVE_LAPACK)
