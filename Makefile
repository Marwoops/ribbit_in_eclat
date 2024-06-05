# juste pour la production

build:
	(cd eclat-compiler; ./eclat -relax ../vm.ecl -arg="0;1;2")

run:
	(cd eclat-compiler; ./eclat -relax ../vm.ecl -arg="0;1;2"; make simul NS=20)

clean:
	(cd target; make clean)
