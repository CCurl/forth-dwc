CXX := clang
CFLAGS := -m32 -Oz

m4: *.c *.h
	$(CXX) $(CFLAGS) -o m4 *.c
	ls -l m4

clean:
	rm -f m4

test: m4
	./m4 tests.fth

run: m4
	./m4

bin: m4
	cp -u -p m4 ~/bin/

boot:
	cp -u -p m4-boot.fth ~/bin/
