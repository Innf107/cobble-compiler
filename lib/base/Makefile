.PHONY: all
all: ./out/Control/Functor.cbi ./out/Data/Semigroup.cbi ./out/Data/Arith.cbi ./out/Data/Function.cbi ./out/Data/Eq.cbi ./out/Data/Ord.cbi ./out/Data/Foldable.cbi ./out/Data/List.cbi ./out/Data/Bool.cbi ./out/Data/Maybe.cbi ./out/Data/Tuple.cbi ./out/Prelude.cbi

./out/Control/Functor.cbi: ./src/Control/Functor.cb
	mkdir -p ./out/Control
	cobble compile  ./src/Control/Functor.cb -o ./out/Control/Functor.rkt

./out/Data/Semigroup.cbi: ./src/Data/Semigroup.cb
	mkdir -p ./out/Data
	cobble compile  ./src/Data/Semigroup.cb -o ./out/Data/Semigroup.rkt

./out/Data/Arith.cbi: ./src/Data/Arith.cb
	mkdir -p ./out/Data
	cobble compile  ./src/Data/Arith.cb -o ./out/Data/Arith.rkt

./out/Data/Function.cbi: ./src/Data/Function.cb
	mkdir -p ./out/Data
	cobble compile  ./src/Data/Function.cb -o ./out/Data/Function.rkt

./out/Data/Eq.cbi: ./src/Data/Eq.cb ./out/Data/Bool.cbi
	mkdir -p ./out/Data
	cobble compile  ./src/Data/Eq.cb ./out/Data/Bool.cbi -o ./out/Data/Eq.rkt

./out/Data/Ord.cbi: ./src/Data/Ord.cb ./out/Data/Bool.cbi
	mkdir -p ./out/Data
	cobble compile  ./src/Data/Ord.cb ./out/Data/Bool.cbi -o ./out/Data/Ord.rkt

./out/Data/Foldable.cbi: ./src/Data/Foldable.cb
	mkdir -p ./out/Data
	cobble compile  ./src/Data/Foldable.cb -o ./out/Data/Foldable.rkt

./out/Data/List.cbi: ./src/Data/List.cb ./out/Control/Functor.cbi ./out/Data/Bool.cbi ./out/Data/Tuple.cbi ./out/Data/Maybe.cbi ./out/Data/Function.cbi ./out/Data/Arith.cbi ./out/Data/Ord.cbi ./out/Data/Foldable.cbi
	mkdir -p ./out/Data
	cobble compile  ./src/Data/List.cb ./out/Control/Functor.cbi ./out/Data/Bool.cbi ./out/Data/Tuple.cbi ./out/Data/Maybe.cbi ./out/Data/Function.cbi ./out/Data/Arith.cbi ./out/Data/Ord.cbi ./out/Data/Foldable.cbi -o ./out/Data/List.rkt

./out/Data/Bool.cbi: ./src/Data/Bool.cb
	mkdir -p ./out/Data
	cobble compile  ./src/Data/Bool.cb -o ./out/Data/Bool.rkt

./out/Data/Maybe.cbi: ./src/Data/Maybe.cb
	mkdir -p ./out/Data
	cobble compile  ./src/Data/Maybe.cb -o ./out/Data/Maybe.rkt

./out/Data/Tuple.cbi: ./src/Data/Tuple.cb
	mkdir -p ./out/Data
	cobble compile  ./src/Data/Tuple.cb -o ./out/Data/Tuple.rkt

./out/Prelude.cbi: ./src/Prelude.cb ./out/Data/Bool.cbi ./out/Data/Eq.cbi ./out/Data/Ord.cbi ./out/Data/Semigroup.cbi ./out/Data/List.cbi ./out/Data/Tuple.cbi
	mkdir -p ./out
	cobble compile  ./src/Prelude.cb ./out/Data/Bool.cbi ./out/Data/Eq.cbi ./out/Data/Ord.cbi ./out/Data/Semigroup.cbi ./out/Data/List.cbi ./out/Data/Tuple.cbi -o ./out/Prelude.rkt


.PHONY: clean
clean:
	rm -r ./out

