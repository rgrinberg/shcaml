# This file was automatically generated by godiva version 0.9.7
.include "../../mk/bsd.prefs.mk"

VERSION=        0.1.3
PKGNAME=        godi-shcaml-${VERSION}
PKGREVISION=    0
DISTNAME=       shcaml-0.1.3
DISTFILES=      shcaml-0.1.3.tar.gz
CATEGORIES=     godi
MASTER_SITES=   http://www.ccs.neu.edu/home/tov/code/shcaml/
MAINTAINER=     Jesse Tov <tov@ccs.neu.edu>
HOMEPAGE=       http://www.ccs.neu.edu/home/tov/code/shcaml/
COMMENT=        An Ocaml library for Unix shell programming

# confopt defaults:


AUTOGENERATE_PLIST = yes
PKG  =          godi-shcaml
MAKE_FLAGS=     PREFIX=${PREFIX} LIBDIR=${PREFIX}/lib BINDIR=${PREFIX}/bin MANDIR=${PREFIX}/man 
DEPENDS+= godi-core-mk>=1.2.14:../../godi/godi-core-mk
DEPENDS+= godi-ocaml>=3.12:../../godi/godi-ocaml
DEPENDS+= godi-pcre-[0-9]*:../../godi/godi-pcre
BUILD_DEPENDS+= godi-findlib-[0-9]*:../../godi/godi-findlib

HAS_CONFIGURE = yes
CONFIGURE_ARGS+= --prefix ${PREFIX}
CONFIGURE_ENV+= ${BUILD_OCAMLFIND_ENV}

USE_GMAKE = yes

MAKE_ENV+=  ${BUILD_OCAMLFIND_ENV} PKGBASE=${PKGBASE:Q}

pre-configure-copy:
.	if exists(files)
	    cd files && ${PAX} -rw -pp . ${WRKSRC}
.	endif

pre-configure: pre-configure-copy

ALL_TARGET=     all
.if ${GODI_HAVE_OCAMLOPT} == "yes"
# ALL_TARGET+= opt
.endif

post-install:
	mkdir -p ${PREFIX}/doc/${PKG}
.	for DOC in 
	    if [ -d ${WRKSRC}/${DOC} ]; then cd ${WRKSRC} && ${PAX} -rw -pp ${DOC} ${PREFIX}/doc/${PKG}; else install -m 0644 ${WRKSRC}/${DOC} ${PREFIX}/doc/${PKG}; fi
.	endfor

.include "../../mk/bsd.pkg.mk"
