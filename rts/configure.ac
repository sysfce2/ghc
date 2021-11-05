# Configure script template for the Run-time System of GHC
#
# Process with 'autoreconf' to get a working configure script.
#
# For the generated configure script, do "./configure --help" to
# see what flags are available. (Better yet, read the documentation!)
#

AC_INIT([GHC run-time system], [1.0.2], [libraries@haskell.org], [rts])

AC_CONFIG_MACRO_DIRS([../m4])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/rts/Constants.h])

dnl * We require autoconf version 2.69 due to
dnl   https://bugs.ruby-lang.org/issues/8179. Also see #14910.
dnl * We need 2.50 due to the use of AC_SYS_LARGEFILE and AC_MSG_NOTICE.
dnl * We need 2.52 due to the use of AS_TR_CPP and AS_TR_SH.
dnl * Using autoconf 2.59 started to give nonsense like this
dnl     #define SIZEOF_CHAR 0
dnl   recently.
AC_PREREQ([2.69])

AC_CONFIG_HEADERS([ghcautoconf.h.autoconf])

# We have to run these unconditionally, but we may discard their
# results in the following code
AC_CANONICAL_BUILD
AC_CANONICAL_HOST

GHC_CONVERT_PLATFORM_PARTS([host], [Host])
FPTOOLS_SET_PLATFORM_VARS([host], [Host])
FPTOOLS_SET_HASKELL_PLATFORM_VARS([Host])

dnl ** Other RTS features
dnl --------------------------------------------------------------
AC_DEFINE_UNQUOTED([USE_LIBDW], [$CABAL_FLAG_libdw], [Set to 1 to use libdw])

AC_DEFINE_UNQUOTED([HAVE_LIBNUMA], [$CABAL_FLAG_libnuma], [Define to 1 if you have libnuma])

dnl ** Write config files
dnl --------------------------------------------------------------

AC_OUTPUT

dnl ######################################################################
dnl Generate ghcautoconf.h
dnl ######################################################################

[
mkdir -p include
touch include/ghcautoconf.h
> include/ghcautoconf.h

echo "#if !defined(__GHCAUTOCONF_H__)" >> include/ghcautoconf.h
echo "#define __GHCAUTOCONF_H__" >> include/ghcautoconf.h
# Copy the contents of $srcdir/../mk/config.h, turning '#define PACKAGE_FOO
# "blah"' into '/* #undef PACKAGE_FOO */' to avoid clashes.
cat $srcdir/../mk/config.h ghcautoconf.h.autoconf | sed \
   -e 's,^\([	 ]*\)#[	 ]*define[	 ][	 ]*\(PACKAGE_[A-Z]*\)[	 ][ 	]*".*".*$,\1/* #undef \2 */,' \
   -e '/__GLASGOW_HASKELL/d' \
   -e '/REMOVE ME/d' \
   >> include/ghcautoconf.h
echo "#endif /* __GHCAUTOCONF_H__ */" >> include/ghcautoconf.h
]

dnl ######################################################################
dnl Generate external symbol flags (-Wl,-u...)
dnl ######################################################################

cat $srcdir/external-symbols.list.in \
    | "$CC" -E -P -traditional -Iinclude - -o - \
    | sed '/^$/d' \
    > external-symbols.list \
    || exit 1

mv external-symbols.list external-symbols.tmp
if [[ -n "$LeadingUnderscore" ]]; then
    sed 's/^/  -Wl,-u_/' external-symbols.tmp > external-symbols.list
else
    sed 's/^/  -Wl,-u/' external-symbols.tmp > external-symbols.list
fi
rm -f external-symbols.tmp

dnl ######################################################################
dnl Generate build-info
dnl ######################################################################

cat $srcdir/rts.buildinfo.in | \
    sed -e 's/^  *//' | \
    "$CC" -E -P -traditional - -o - \
    > rts.buildinfo
echo "" >> rts.buildinfo
rm -f external-symbols.list