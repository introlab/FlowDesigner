# Disable automatic dependency tracking if using other tools than gcc and gmake
AUTOMAKE_OPTIONS = no-dependencies

#bin_PROGRAMS = pcolor

#pcolor_SOURCES = pcolor.c

#INCLUDES = -D_REENTRANT

#LDFLAGS = -L/usr/X11R6/lib 

#LDADD = -lMesaGL -lX11 -lXi -lpthread -lm


INCLUDES = -I$(top_srcdir)/data-flow/include 

bin_PROGRAMS = AudioNetwork

AudioNetwork_SOURCES = AudioNetwork.cc 

LDFLAGS=$(FFTW_LDFLAGS)

LDADD = $(top_srcdir)/audio_blocks/src/libablocks.la \
        $(top_srcdir)/HMM/src/libhmm.la \
        $(top_srcdir)/VQ/src/libvq.la \
        $(top_srcdir)/data-flow/src/libnetwork.la \
        $(LIB_FFTW)

