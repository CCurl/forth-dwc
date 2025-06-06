app := dwc

CXX := clang
CFLAGS := -m32 -O3 -D IS_LINUX

srcfiles := $(shell find . -name "*.c")
incfiles := $(shell find . -name "*.h")
LDLIBS   := -lm

all: $(app) $(app32)

$(app): $(srcfiles) $(incfiles)
	$(CXX) $(CFLAGS) $(LDFLAGS) -o $(app) $(srcfiles) $(LDLIBS)
	ls -l $(app)

clean:
	rm -f $(app)

test: $(app)
	./$(app) test.fth

run: $(app)
	./$(app)

bin: $(app)
	cp -u -p $(app) ~/bin/
