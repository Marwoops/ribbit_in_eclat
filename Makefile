# juste pour la production

build:
	(cd eclat-compiler; ./eclat -relax -notyB ../rvm.ecl -arg="0;1;2")

run:
	(cd eclat-compiler; ./eclat -relax -notyB ../rvm.ecl -arg="0;1;2"; make simul NS=200000000)

run_ml:
	(ocamlc -o t ml_rvm.ml && ./t)

clean:
	(cd target; make clean)

clean_ml:
	(rm *.cm* t)
