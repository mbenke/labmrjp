all: runtest4 runtest1 runtest2 runtest3

%.e32: %.S main.c
	gcc -m32 -o $@ $^

%: %.hs Asm.hs
	ghc $<

Test2: Test2.hs Asm.hs
	ghc $<

clean:
	-rm -f *.hi *.o *.dyn_hi *.dyn_o
	-rm -f *.e32 *~ Test?
	-rm -f test?.s test?.S

runtest1: Test1 test1.e32
	./test1.e32

test1.S: Test1
	./Test1 > test1.S

runtest2: Test2 test2.e32
	./test2.e32

test2.S: Test2
	./Test2 > test2.S

runtest3: Test3 test3.e32
	./test3.e32

test3.S: Test3
	./Test3 > test3.S

runtest4: Test4 test4.e32
	./test4.e32

test4.S: Test4
	./Test4 > test4.S
