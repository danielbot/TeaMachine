.PHONY: all clean

name=tea

optbase=-Wno-sign-compare

opt = -O3 $(optbase)

ifdef DEBUG
opt = -g -O0 $(optbase)
else
DEBUG = 0
endif

opt += -Wall -Wno-unused-variable -Wno-unused-function -rdynamic -fPIC
lib = Shardmap/shardmap.so Shardmap/options.o -lbacktrace -ldl

all: $(name)
	@: # quiet make when nothing to do

$(name): Makefile debug.h teamachine.cc tpcb.cc
	g++ -DDEBUG=$(DEBUG) $(opt) teamachine.cc tpcb.cc $(lib) -o$(name)

clean:
	rm -f $(name) *.o *.so a.out
