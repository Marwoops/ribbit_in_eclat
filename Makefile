# juste pour la production

run:
	(cd eclat-compiler; ./eclat -relax ../vm.ecl -args="0;1;2"; make simul NS=200)

clean:
	(cd target; make clean)
