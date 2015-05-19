AC_INIT([Haskell simple sendfile interface], [0.2.x], [kazu@iij.ad.jp], [simple-sendfile])

dnl ** Modeled after Haskell's "unix" package **

AC_CONFIG_SRCDIR([Network/Sendfile.hs])

AC_CONFIG_HEADERS([Network/Sendfile/HsConfig.h])

AC_C_CONST

dnl ** Enable large file support.  NB. do this before testing the type of
dnl    off_t, because it will affect the result of that test.
AC_SYS_LARGEFILE

AC_OUTPUT
