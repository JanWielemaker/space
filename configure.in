dnl Process this file with autoconf to produce a configure script.

AC_INIT(pack.pl)
AC_PREREQ([2.50])
AC_CONFIG_HEADER(config.h)

AC_PROG_CXX
AC_LANG_CPLUSPLUS

AC_CHECK_LIB(geos, main)
AC_CHECK_LIB(spatialindex, main)

AC_CHECK_HEADERS(spatialindex/SpatialIndex.h,[si=1],[si=0])
if test "$si" = 0; then
   AC_ERROR("Cannot find spatialindex. Spatialindex must be installed first (http://trac.gispython.org/spatialindex/)")
fi

AC_CHECK_HEADERS(geos_c.h,[geos=1],[geos=0])
if test "$geos" = 0; then
   AC_ERROR("Cannot find GEOS library. GEOS must be installed first (http://trac.osgeo.org/geos/)")
fi

AC_CHECK_FUNCS(bzero)

AC_OUTPUT(Makefile)








