AC_DEFUN(AC_MODULE_OPT,
[
echo do we want $1
AC_ARG_ENABLE($1, [  --enable-$1   enable module], [$1=$1; if test "$enableval" = no; then $1=; fi], [if test -d $1; then $1=$1; else $1=; fi ])
])

AC_DEFUN(AC_FIND_FILE,
[
$3=NO
for i in $2;
do
  for j in $1;
  do
    if test -r "$i/$j"; then
      $3=$i
      break 2
    fi
  done
done
])

AC_DEFUN(AC_LIBTOOL_ACC_KLUDGE,
[
AC_ARG_WITH(libtool-acc-kludge,
    [  --with-libtool-acc-kludge    tell libtool to use aCC instead of ld to link shared libraries],
    [mv libtool libtool-bak  
cat libtool-bak | perl -ne 's/\+h /\\\${wl}\+h/; s/ \+b / \\\${wl}\+b/; s/\"\/.*\/ld\"/\"aCC\"/; print' > libtool
    ])
])

AC_DEFUN(AC_PATH_FFTW,
[
LIBFFTW="-lfftw -lrfftw"

AC_MSG_CHECKING([for fftw])

LIBFFTW="$LIBFFTW"
ac_fftw_includes=NO ac_fftw_libraries=NO ac_fftw_bindir=NO
fftw_libraries=""
fftw_includes=""
AC_ARG_WITH(fftw-dir,
    [  --with-fftw-dir=DIR       where the root of FFTW is installed ],
    [  ac_fftw_includes="$withval"/include
       ac_fftw_libraries="$withval"/lib
       ac_fftw_bindir="$withval"/bin
    ])

AC_ARG_WITH(fftw-includes,
    [  --with-fftw-includes=DIR  where the FFTW includes are. ],
    [  
       ac_fftw_includes="$withval"
    ])
    
fftw_libs_given=no

AC_ARG_WITH(fftw-libraries,
    [  --with-fftw-libraries=DIR where the FFTW library is installed.],
    [  ac_fftw_libraries="$withval"
       fftw_libs_given=yes
    ])
AC_CACHE_VAL(ac_cv_have_fftw,
[#try to guess FFTW locations

fftw_incdirs="/usr/lib/fftw/include /opt/include /usr/local/fftw/include /usr/include/fftw /usr/include /usr/local/include $FFTWINC"
test -n "$FFTWDIR" && fftw_incdirs="$FFTWDIR/include $FFTWDIR $fftw_incdirs"
fftw_incdirs="$ac_fftw_includes $fftw_incdirs"
AC_FIND_FILE(fftw.h, $fftw_incdirs, fftw_incdir)
ac_fftw_includes="$fftw_incdir"

fftw_libdirs="/usr/lib/fftw/lib /usr/lib /opt/lib /usr/local/fftw/lib /usr/local/lib /usr/lib/fftw /usr/local/lib $FFTWLIB"
test -n "$FFTWDIR" && fftw_libdirs="$FFTWDIR/lib $FFTWDIR $fftw_libdirs"
if test ! "$ac_fftw_libraries" = "NO"; then
  fftw_libdirs="$ac_fftw_libraries $fftw_libdirs"
fi

test=NONE
fftw_libdir=NONE
for dir in $fftw_libdirs; do
  try="ls -1 $dir/libfftw*"
  if test=`eval $try 2> /dev/null`; then fftw_libdir=$dir; break; else echo "tried $dir" >&AC_FD_CC ; fi
done

ac_fftw_libraries="$fftw_libdir"

ac_cxxflags_safe="$CXXFLAGS"
ac_ldflags_safe="$LDFLAGS"
ac_libs_safe="$LIBS"

#CXXFLAGS="$CXXFLAGS -I$fftw_incdir $all_includes"
INCLUDE="$INCLUDE -I$fftw_incdir $all_includes"
LDFLAGS="-L$fftw_libdir $all_libraries"
LIBS="$LIBS $LIBFFTW"

CXXFLAGS="$ac_cxxflags_safe"
LDFLAGS="$ac_ldflags_safe"
LIBS="$ac_libs_safe"

if test "$ac_fftw_includes" = NO || test "$ac_fftw_libraries" = NO; then
  ac_cv_have_fftw="have_fftw=no"
  ac_fftw_notfound=""
  if test "$ac_fftw_includes" = NO; then
    if test "$ac_fftw_libraries" = NO; then
      ac_fftw_notfound="(headers and libraries)";
    else
      ac_fftw_notfound="(headers)";
    fi
  else
    ac_fftw_notfound="(libraries)";
  fi

  AC_MSG_ERROR([FFTW not found. Please check your installation! ]);
else
  have_fftw="yes"
fi
])

eval "$ac_cv_have_fftw"

if test "$have_fftw" != yes; then
  AC_MSG_RESULT([$have_fftw]);
else
  ac_cv_have_fftw="have_fftw=yes \
    ac_fftw_includes=$ac_fftw_includes ac_fftw_libraries=$ac_fftw_libraries"
  AC_MSG_RESULT([libraries $ac_fftw_libraries, headers $ac_fftw_includes])
  
  fftw_libraries="$ac_fftw_libraries"
  fftw_includes="$ac_fftw_includes"
fi

dnl if test ! "$fftw_libs_given" = "yes"; then
dnl CHECK_FFTW_DIRECT(fftw_libraries= ,[])
dnl fi

AC_SUBST(fftw_libraries)
AC_SUBST(fftw_includes)

if test "$fftw_includes" = "$x_includes" || test -z "$fftw_includes"; then
 FFTW_INCLUDES="";
else
 FFTW_INCLUDES="-I$fftw_includes"
 all_includes="$FFTW_INCLUDES $all_includes"
fi

if test "$fftw_libraries" = "$x_libraries" || test -z "$fftw_libraries"; then
 FFTW_LDFLAGS=""
else
 FFTW_LDFLAGS="-L$fftw_libraries"
 all_libraries="$FFTW_LDFLAGS $all_libraries"
fi

AC_SUBST(FFTW_INCLUDES)
AC_SUBST(FFTW_LDFLAGS)

LIB_FFTW='-lfftw -lrfftw'
AC_SUBST(LIB_FFTW)

])

