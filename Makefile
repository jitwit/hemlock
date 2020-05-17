package = chez-hemlock
version = 0.1
chez := scheme

prefix = 
libdir = ${prefix}/lib

chezversion ::= $(shell echo '(call-with-values scheme-version-number (lambda (a b c) (format \#t "~d.~d" a b)))' | ${chez} -q)
schemedir = ${libdir}/csv${chezversion}-site

build:
	${chez} --program compile.ss

clean:
	find . -name "*.so" -exec rm {} \;
	find . -name "*.html" -exec rm {} \;
	find . -name "*~" -exec rm {} \;



