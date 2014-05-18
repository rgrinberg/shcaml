# Autoconf script for Caml-Shcaml

# Don't run just anywhere
AC_INIT(shtop.mli)

# Packages we depend on
PACKAGES="camlp4 unix pcre dynlink"

# Our version number
VERSION="0.1.3"

# Our version name
CODENAME="Shmeer"

# shlock shmaltz shadken shanda shaygets shayner shiksa
# shlemiel shlep shmendrick shmegegge
# shmutzik shnorrer shtik shtetl shtunk
# shvitz 

# Used:
# 0.1.0 shmatta
# 0.1.1 shmooz
# 0.1.2 shlimazl
# 0.1.3 shmeer

# Check for findlib libraries:
for lib in $PACKAGES; do
    AC_MSG_CHECKING(for findlib package: $lib)
    if ocamlfind query $lib >/dev/null 2>&1; then
        AC_MSG_RESULT(ok)
    else
    AC_MSG_ERROR(Cannot find findlib package: $lib)
    fi
done

AC_SUBST(PACKAGES)
AC_SUBST(VERSION)
AC_SUBST(CODENAME)

AC_OUTPUT(Makefile syntax/Makefile META lib/version.ml)
chmod a-w Makefile syntax/Makefile META lib/version.ml
