# juste pour la production

RVM=../new_rvm.ecl
ECLAT_FLAGS=-relax -moore
NS=200000000
SRC=test/fibo.scm
GEN_CODE=misc/generate_bytecode.py
GSI=../gambit/gsi/gsi
BYTECODE=bytecode.ecl

bytecode:
	(cat max.scm $(SRC) | $(GSI) rsc.scm | python $(GEN_CODE) > $(BYTECODE))

build:
	(cd eclat-compiler; ./eclat $(ECLAT_FLAGS) ../$(BYTECODE) $(RVM))

run:
	(cd eclat-compiler; ./eclat $(ECLAT_FLAGS) ../$(BYTECODE) $(RVM); make simul NS=$(NS))

run_ml:
	(ocamlc -o t ml_rvm.ml && ./t)

clean:
	(cd target; make clean)

clean_ml:
	(rm *.cm* t)
