BUILD_DIR ?= ./out

COMPILE = cobble compile $(COBBLE_OPTS)

all: ./out/Control/Functor.cbi ./out/Data/List.cbi

.PHONY: clean

clean:
	rm -r out

./out/Control/Functor.cbi: ./src/Control/Functor.cb
	mkdir -p ./out/Control/
	$(COMPILE) ./src/Control/Functor.cb -o ./out/Control/Functor.rkto

./out/Data/List.cbi: ./src/Data/List.cb ./out/Control/Functor.cbi
	mkdir -p ./out/Data
	$(COMPILE) ./src/Data/List.cb ./out/Control/Functor.cbi -o ./out/Data/List.rkto

