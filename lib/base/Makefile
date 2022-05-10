.PHONY: all
all: .cobble/out/Control/Functor.cbi .cobble/out/Data/Semigroup.cbi .cobble/out/Data/Unit.cbi .cobble/out/Data/Arith.cbi .cobble/out/Data/Function.cbi .cobble/out/Data/Eq.cbi .cobble/out/Data/Ord.cbi .cobble/out/Data/Foldable.cbi .cobble/out/Data/List.cbi .cobble/out/Data/Bool.cbi .cobble/out/Data/Maybe.cbi .cobble/out/Data/Tuple.cbi .cobble/out/Prelude.cbi

.cobble/out/Control/Functor.cbi: ./src/Control/Functor.cb
	mkdir -p .cobble/out/Control
	cobble compile   ./src/Control/Functor.cb -o .cobble/out/Control/Functor.rkt

.cobble/out/Data/Semigroup.cbi: ./src/Data/Semigroup.cb
	mkdir -p .cobble/out/Data
	cobble compile   ./src/Data/Semigroup.cb -o .cobble/out/Data/Semigroup.rkt

.cobble/out/Data/Unit.cbi: ./src/Data/Unit.cb
	mkdir -p .cobble/out/Data
	cobble compile   ./src/Data/Unit.cb -o .cobble/out/Data/Unit.rkt

.cobble/out/Data/Arith.cbi: ./src/Data/Arith.cb
	mkdir -p .cobble/out/Data
	cobble compile   ./src/Data/Arith.cb -o .cobble/out/Data/Arith.rkt

.cobble/out/Data/Function.cbi: ./src/Data/Function.cb
	mkdir -p .cobble/out/Data
	cobble compile   ./src/Data/Function.cb -o .cobble/out/Data/Function.rkt

.cobble/out/Data/Eq.cbi: ./src/Data/Eq.cb .cobble/out/Data/Bool.cbi
	mkdir -p .cobble/out/Data
	cobble compile   ./src/Data/Eq.cb .cobble/out/Data/Bool.cbi -o .cobble/out/Data/Eq.rkt

.cobble/out/Data/Ord.cbi: ./src/Data/Ord.cb .cobble/out/Data/Bool.cbi
	mkdir -p .cobble/out/Data
	cobble compile   ./src/Data/Ord.cb .cobble/out/Data/Bool.cbi -o .cobble/out/Data/Ord.rkt

.cobble/out/Data/Foldable.cbi: ./src/Data/Foldable.cb
	mkdir -p .cobble/out/Data
	cobble compile   ./src/Data/Foldable.cb -o .cobble/out/Data/Foldable.rkt

.cobble/out/Data/List.cbi: ./src/Data/List.cb .cobble/out/Control/Functor.cbi .cobble/out/Data/Bool.cbi .cobble/out/Data/Tuple.cbi .cobble/out/Data/Maybe.cbi .cobble/out/Data/Function.cbi .cobble/out/Data/Arith.cbi .cobble/out/Data/Ord.cbi .cobble/out/Data/Foldable.cbi
	mkdir -p .cobble/out/Data
	cobble compile  --ddump-core ./src/Data/List.cb .cobble/out/Control/Functor.cbi .cobble/out/Data/Bool.cbi .cobble/out/Data/Tuple.cbi .cobble/out/Data/Maybe.cbi .cobble/out/Data/Function.cbi .cobble/out/Data/Arith.cbi .cobble/out/Data/Ord.cbi .cobble/out/Data/Foldable.cbi -o .cobble/out/Data/List.rkt

.cobble/out/Data/Bool.cbi: ./src/Data/Bool.cb
	mkdir -p .cobble/out/Data
	cobble compile   ./src/Data/Bool.cb -o .cobble/out/Data/Bool.rkt

.cobble/out/Data/Maybe.cbi: ./src/Data/Maybe.cb
	mkdir -p .cobble/out/Data
	cobble compile   ./src/Data/Maybe.cb -o .cobble/out/Data/Maybe.rkt

.cobble/out/Data/Tuple.cbi: ./src/Data/Tuple.cb
	mkdir -p .cobble/out/Data
	cobble compile   ./src/Data/Tuple.cb -o .cobble/out/Data/Tuple.rkt

.cobble/out/Prelude.cbi: ./src/Prelude.cb .cobble/out/Control/Functor.cbi .cobble/out/Data/Arith.cbi .cobble/out/Data/Bool.cbi .cobble/out/Data/Eq.cbi .cobble/out/Data/Foldable.cbi .cobble/out/Data/Function.cbi .cobble/out/Data/List.cbi .cobble/out/Data/Maybe.cbi .cobble/out/Data/Ord.cbi .cobble/out/Data/Semigroup.cbi .cobble/out/Data/Tuple.cbi .cobble/out/Data/Unit.cbi
	mkdir -p .cobble/out
	cobble compile   ./src/Prelude.cb .cobble/out/Control/Functor.cbi .cobble/out/Data/Arith.cbi .cobble/out/Data/Bool.cbi .cobble/out/Data/Eq.cbi .cobble/out/Data/Foldable.cbi .cobble/out/Data/Function.cbi .cobble/out/Data/List.cbi .cobble/out/Data/Maybe.cbi .cobble/out/Data/Ord.cbi .cobble/out/Data/Semigroup.cbi .cobble/out/Data/Tuple.cbi .cobble/out/Data/Unit.cbi -o .cobble/out/Prelude.rkt


.PHONY: clean
clean:
	-rm -r .cobble/out
	-rm -r .cobble/dependencies

