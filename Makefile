package = chez-hemlock
version = 0.1
chez = scheme
out =

build :
	$(chez) --program compile.ss

install :
	mkdir -p $(out)
	cp -r *.so $(out)

clean:
	find . -name "*.so" -exec rm {} \;
	find . -name "*~" -exec rm {} \;



