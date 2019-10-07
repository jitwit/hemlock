package = chez-hemlock
version = 0.1
chez := scheme
install = install -D

prefix = /usr/local
bindir = ${prefix}/bin
libdir = ${prefix}/lib
include-dir = ${prefix}/include

chezversion ::= $(shell echo '(call-with-values scheme-version-number (lambda (a b c) (format \#t "~d.~d" a b)))' | ${chez} -q)
schemedir = ${libdir}/csv${chezversion}-site

build:
	${chez} --program compile-all.ss

install:
	find . -type f -regex ".*.so" -exec sh -c '${INSTALL} -t ${LIBS}/$$(dirname $$1) $$1' _ {} \;

install-src:
	find . -type f -regex ".*.sls" -exec sh -c '${INSTALL} -t ${LIBS}/$$(dirname $$1) $$1' _ {} \;
	find . -type f -regex ".*.scm" -exec sh -c '${INSTALL} -t ${LIBS}/$$(dirname $$1) $$1' _ {} \;

clean:
	find . -name "*.so" -exec rm {} \;
	find . -name "*.html" -exec rm {} \;
	find . -name "*~" -exec rm {} \;



