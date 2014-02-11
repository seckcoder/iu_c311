clean:
	rm -f stst *.s *.out test
scheme:
	gcc -O1 -o stst start.c stst.s
ctest:ctest.c
	gcc -S ctest.c
	cat ctest.s
