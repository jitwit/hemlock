PACKAGE = chez-hemlock
VERSION = 0.1

CHEZ = scheme
INSTALL = install -D

PREFIX = ~/.chez.d
EXEC_PREFIX = ${PREFIX}
LIBDIR = ${EXEC_PREFIX}/lib

chezversion ::= $(shell echo '(call-with-values scheme-version-number (lambda (a b c) (format \#t "~d.~d" a b)))' | ${CHEZ} -q)
schemedir = ${LIBDIR}/csv${chezversion}-site

build:
	echo "(compile-library \"chez/kd.sls\"))" | ${CHEZ} -q
	echo "(compile-library \"chez/patricia.sls\"))" | ${CHEZ} -q
	echo "(compile-library \"chez/patricia-set.sls\"))" | ${CHEZ} -q
	echo "(compile-library \"chez/batched-queue.sls\"))" | ${CHEZ} -q

install:
	find . -type f -regex ".*.so" -exec sh -c '${INSTALL} -t ${schemedir}/$$(dirname $$1) $$1' _ {} \;

clean:
	find . -name "*.so" -exec rm {} \;
	find . -name "*.html" -exec rm {} \;
	find . -name "*~" -exec rm {} \;



