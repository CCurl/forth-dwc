CXX := clang
CFLAGS := -m32 -Oz

dwc: *.c *.h
	$(CXX) $(CFLAGS) -o dwc *.c
	ls -l dwc

clean:
	rm -f dwc

test: dwc
	./dwc base.fth

run: dwc
	./dwc

bin: dwc
	cp -u -p dwc ~/bin/
