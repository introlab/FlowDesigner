#!/bin/sh
# flowdesigner-config.  Generated from flowdesigner-config.in by configure.

#Adapted from gnome-config

prefix=@CMAKE_INSTALL_PREFIX@
exec_prefix=${prefix}

bindir=${exec_prefix}/bin
sbindir=${exec_prefix}/sbin
libexecdir=${exec_prefix}/libexec
datadir=${prefix}/lib/flowdesigner/toolbox
sysconfdir=${prefix}/etc
sharedstatedir=${prefix}/com
localstatedir=${prefix}/var
libdir=${exec_prefix}/lib
infodir=${prefix}/share/info
mandir=${prefix}/share/man
includedir=${prefix}/include/flowdesigner
toolboxdir=${exec_prefix}/lib/flowdesigner/toolbox
pkgdatadir=${datadir}/@PACKAGE@
pkglibdir=${libdir}/@PACKAGE@
pkgincludedir=${includedir}/@PACKAGE@
MODULE_VERSION=@PACKAGE@-@VERSION@
module_dirs="$libdir"

DEFS="-DPACKAGE_NAME=\"\" -DPACKAGE_TARNAME=\"\" -DPACKAGE_VERSION=\"\" -DPACKAGE_STRING=\"\" -DPACKAGE_BUGREPORT=\"\" -DSTDC_HEADERS=1 -DHAVE_SYS_TYPES_H=1 -DHAVE_SYS_STAT_H=1 -DHAVE_STDLIB_H=1 -DHAVE_STRING_H=1 -DHAVE_MEMORY_H=1 -DHAVE_STRINGS_H=1 -DHAVE_INTTYPES_H=1 -DHAVE_STDINT_H=1 -DHAVE_UNISTD_H=1 -DHAVE_DLFCN_H=1 -DPROTOTYPES=1 -D__PROTOTYPES=1 -DFLOWDESIGNER_VERSION=\"@VERSION@\" -DHAVE_STRING_H=1 -DHAVE_DLFCN_H=1 -DHAVE_FLOAT_H=1 -DHAVE_VALUES_H=1 -DHAVE_SEMAPHORE_H=1 -DHAVE_SYS_SOUNDCARD_H=1 -DHAVE_LINUX_RTC_H=1 -DHAVE_EXT_HASH_MAP=1 -DHAVE_LIBM=1 -DHAVE_LIBDL=1 -D_REENTRANT=1 -DHAVE_FFTW=1 -DINSTALL_PREFIX=\"@CMAKE_INSTALL_PREFIX@\" -DTOOLBOX_PATH=\"@CMAKE_INSTALL_PREFIX@/lib/flowdesigner/toolbox\""
LIBS="-ldl -lm  -lpthread "
LDFLAGS=""

# stolen from autoconf
if (echo "testing\c"; echo 1,2,3) | grep c >/dev/null; then
  if (echo -n testing; echo 1,2,3) | sed s/-n/xn/ | grep xn >/dev/null; then
    ac_n= ac_c='
' ac_t='	'
  else
    ac_n=-n ac_c= ac_t=
  fi
else
  ac_n= ac_c='\c' ac_t=
fi

usage()
{
    exit_code=$1

    cat <<EOF
Usage: flowdesigner-config [OPTION]... [LIBRARY]...

Generic options
  --version	output FlowDesigner version information.
  --modversion  output the module version information.
  --help	display this help and exit.

Compilation support options
  --cflags	print pre-processor and compiler flags
  --libs	print library linking information
  --libs-only-L	only print the -L/-R part of --libs
  --libs-only-l only print the -l part of --libs

Install directories FlowDesigner was configured to
  --prefix  --exec-prefix  --bindir  --sbindir  --libexecdir  --datadir
  --sysconfdir  --sharedstatedir  --localstatedir  --libdir  --infodir
  --mandir  --includedir --toolboxdir

Known values for LIBRARY are:

    libflow        (Base FlowDesigner lib)
    audio_blocks   (DSP toolkit)
    effects        (effects toolkit)
    fuzzy          (fuzzy toolkit)
    vflow          (GUI)
    hmm            (HMM toolkit)
    nnet           (NNet toolkit)
    vq             (VQ toolkit)
    config         (Stuff found by autoconf)

EOF
    exit $exit_code
}

if test $# -eq 0; then
    usage 1
fi

cflags=false
libs_L=false
libs_l=false
modversion=false

while test $# -gt 0; do
    case "$1" in
    -*=*) optarg=`echo "$1" | sed 's/[-_a-zA-Z0-9]*=//'` ;;
    *) optarg= ;;
    esac

    case $1 in
    --version)
	echo @PACKAGE@ @VERSION@
	exit 0
	;;
    --exec-prefix)
	echo $exec_prefix
	exit 0
	;;
    --prefix)
	echo $prefix
	exit 0
	;;
    --*dir)
	dirname=\$`echo $1 | sed -e 's,^--,,'`
	dirname=`eval echo $dirname`
	test -z "$dirname" && exit 1
	echo $dirname
	exit 0
	;;
    --help)
	usage 0
	;;
    --cflags)
       	cflags=true
       	;;
    --libs)
       	libs_L=true
	libs_l=true
       	;;
    --libs-only-L)
	libs_L=true
	;;
    --libs-only-l)
	libs_l=true
	;;
    --modversion)
        modversion=true
	;;
    --*)
	usage 1
	;;
    libflow)
	the_libs="$the_libs -L$libdir -lflow"
	the_flags="$the_flags -I$includedir"
	;;
    effects)
	the_libs="$the_libs -L$libdir -L$toolboxdir/effects -lflow -leffects"
	the_flags="$the_flags -I$includedir -I$toolboxdir/effects"
	;;
    hmm)
	the_libs="$the_libs -L$libdir -L$toolboxdir/HMM -lflow -lhmm"
	the_flags="$the_flags -I$includedir -I$toolboxdir/HMM "
	;;
    nnet)
	the_libs="$the_libs -L$libdir -L$toolboxdir/NNet -lflow -lnnet"
	the_flags="$the_flags -I$includedir -I$toolboxdir/NNet"
	;;
    audio_blocks)
	the_libs="$the_libs -L$libdir -L$toolboxdir/audio_blocks -lflow -lablocks"
	the_flags="$the_flags -I$includedir -I$toolboxdir/audio_blocks"
	;;
    fuzzy)
	the_libs="$the_libs -L$libdir -L$toolboxdir/FuzzyEngine -lflow -lfuzzy"
	the_flags="$the_flags -I$includedir -I$toolboxdir/FuzzyEngine"
	;;
    vq)
	the_libs="$the_libs -L$libdir -L$toolboxdir/VQ -lflow -lvq"
	the_flags="$the_flags -I$includedir -I$toolboxdir/VQ "
	;;
    vflow)
	the_libs="$the_libs -L$libdir -L$toolboxdir/vflow -lflow"
	the_flags="$the_flags -I$includedir -I$toolboxdir/vflow"
	;;
    octave)
	the_libs="$the_libs -L$libdir -L$toolboxdir/Octave -lflow -lOctave"
	the_flags="$the_flags -I$includedir -I$toolboxdir/Octave"
	;;
    lapack)
	the_libs="$the_libs -L$libdir -L$toolboxdir/lapackflow -lflow -llapackflow"
	the_flags="$the_flags -I$includedir -I$toolboxdir/lapackflow"
	;;
    config)
	the_libs="$the_libs $LD_FLAGS $LIBS"
	the_flags="$the_flags $DEFS"
	;;
    *)
        error=true
        for dir in `echo $module_dirs | sed 's/:/ /g'`; do
   	    cnf_sh=${dir}/${1}Conf.sh
	    if test -f ${cnf_sh}; then
    		. ${cnf_sh}
    		up_name=`echo $1 | tr a-z A-Z`
    		cnf_libdir=\$${up_name}_LIBDIR
    		cnf_libs=\$${up_name}_LIBS
    		cnf_flags=\$${up_name}_INCLUDEDIR
    		the_libs="$the_libs `eval echo $cnf_libdir` `eval echo $cnf_libs`"
    		the_flags="$the_flags `eval echo $cnf_flags`"
		error=false
		break
    	    fi
        done
	if $error; then
	    echo Unknown library \`$1\' 1>&2
	fi
	;;
    esac
    shift
done

if $cflags; then
    all_flags="$the_flags"
fi

if $libs_L || $libs_l; then
    all_flags="$all_flags $the_libs"
fi

if $modversion; then
    echo $MODULE_VERSION
    exit 0
fi

if test -z "$all_flags" || test "x$all_flags" = "x "; then
    exit 1
fi

# Straight out any possible duplicates, but be careful to
# get `-lfoo -lbar -lbaz' for `-lfoo -lbaz -lbar -lbaz'
other_flags=
lib_L_flags=
rev_libs=
for i in $all_flags; do
    case "$i" in
    # a library, save it for later, in reverse order
    -l*) rev_libs="$i $rev_libs" ;;
    -L*|-R*)
	if $libs_L; then
	    case " $lib_L_flags " in
	    *\ $i\ *) ;;			# already there
	    *) lib_L_flags="$lib_L_flags $i" ;;	# add it to output
	    esac 
	fi;;
    *)
	case " $other_flags " in
	*\ $i\ *) ;;				# already there
	*) other_flags="$other_flags $i" ;;	# add it to output
        esac ;;
    esac
done

ord_libs=
if $libs_l; then
    for i in $rev_libs; do
	case " $ord_libs " in
	*\ $i\ *) ;;			# already there
	*) ord_libs="$i $ord_libs" ;;	# add it to output in reverse order
	esac
    done
fi


echo $other_flags $lib_L_flags $ord_libs

exit 0
