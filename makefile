CXX := clang
CFLAGS := -m32 -Oz

dwc: *.c *.h
	$(CXX) $(CFLAGS) -o dwc *.c
	ls -l dwc

clean:
	rm -f dwc

test: dwc
	./dwc tests.fth

run: dwc
	./dwc

bin: dwc
	cp -u -p dwc ~/bin/
	cp -u -p dwc-boot.fth ~/bin/
