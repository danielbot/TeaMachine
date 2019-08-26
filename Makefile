.PHONY: all clean

name=tea

opt = -O3

ifdef DEBUG
opt = -g -O0
else
DEBUG = 0
endif

opt += -Wall -Wno-unused-variable -Wno-unused-function -rdynamic
lib = shardmap/shardmap.so shardmap/options.o -lbacktrace -ldl

all: $(name)
	@: # quiet make when nothing to do

$(name): Makefile debug.h teamachine.cc tpcb.cc
	g++ -DDEBUG=$(DEBUG) $(opt) teamachine.cc tpcb.cc $(lib) -o$(name)

clean:
	rm -f $(name) *.o *.so a.out
