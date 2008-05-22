##------------------------------------------------------------------------
## $Id:: FindOctave.cmake 2008-05-22 9:27:48Z matlabbe                   $
##------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Check for the presence of Octave
##
## The following variables are set when Octave is found:
##  HAVE_OCTAVE     - Set to true, if all components of Octave have been found.
##  OCTAVE_LIBRARIES - Link these to use Octave
##  OCTAVE_INCLUDES  - Location of the Octave header files

## -----------------------------------------------------------------------------
## Find octave-config program

set (bin_locations
 /usr/bin
 /usr/local/bin
)

find_file( OCTAVE_CONFIG_PROGRAM "octave-config" PATHS bin_locations )

## -----------------------------------------------------------------------------
## Use octave-config to get libraries and includes directories

IF (OCTAVE_CONFIG_PROGRAM)
    ## Find libraries
    exec_program(${OCTAVE_CONFIG_PROGRAM}
                  ARGS -p OCTLIBDIR
                  OUTPUT_VARIABLE OCTLIBDIR)
    IF (OCTLIBDIR)
        SET (OCTAVE_LIBRARIES "${OCTLIBDIR}/libcruft.so" "${OCTLIBDIR}/liboctave.so" "${OCTLIBDIR}/liboctinterp.so")
    ENDIF (OCTLIBDIR)

    ## Find includes
    exec_program(${OCTAVE_CONFIG_PROGRAM}
                  ARGS -p OCTINCLUDEDIR
                  OUTPUT_VARIABLE OCTAVE_INCLUDES)

ENDIF (OCTAVE_CONFIG_PROGRAM)

## -----------------------------------------------------------------------------
## Actions taken when all components have been found

IF (OCTAVE_LIBRARIES)
  SET (HAVE_OCTAVE TRUE)
ELSE (OCTAVE_LIBRARIES)
  IF (NOT OCTAVE_FIND_QUIETLY)
    IF (NOT OCTAVE_LIBRARIES)
      MESSAGE (STATUS "Unable to find Octave library files!")
    ENDIF (NOT OCTAVE_LIBRARIES)
  ENDIF (NOT OCTAVE_FIND_QUIETLY)
ENDIF (OCTAVE_LIBRARIES)

IF (HAVE_OCTAVE)
  IF (NOT OCTAVE_FIND_QUIETLY)
    MESSAGE (STATUS "Found components for Octave")
    MESSAGE (STATUS "OCTAVE_INCLUDES  = ${OCTAVE_INCLUDES}")
    MESSAGE (STATUS "OCTAVE_LIBRARIES = ${OCTAVE_LIBRARIES}")
  ENDIF (NOT OCTAVE_FIND_QUIETLY)
ELSE (HAVE_OCTAVE)
  IF (OCTAVE_FIND_REQUIRED)
    MESSAGE (FATAL_ERROR "Could not find Octave!")
  ENDIF (OCTAVE_FIND_REQUIRED)
ENDIF (HAVE_OCTAVE)

