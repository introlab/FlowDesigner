# Disable automatic dependency tracking if using other tools than gcc and gmake
AUTOMAKE_OPTIONS = no-dependencies

bin_PROGRAMS = pcolor

pcolor_SOURCES = pcolor.c

INCLUDES = -D_REENTRANT

LDFLAGS = -L/usr/X11R6/lib 

LDADD = -lMesaGL -lX11 -lXi -lpthread -lm
