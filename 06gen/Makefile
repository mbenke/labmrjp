TOP=../../..
BINFILES=i3l i3j runtime.bc
BNFCRUST:= DocInstant.* SharedString.* Instant.dtd XMLInstant.* SkelInstant.* TestInstant*
GENERATED:= $(BNFCRUST) LexInstant.* ParInstant.* LayoutInstant.*  AbsInstant.* ErrM.* PrintInstant.*

include $(TOP)/mk/std.mk

i3l: GenLlvm.hs ParInstant.hs LexInstant.hs LlvmAsm.hs LlvmState.hs Fresh.hs
	ghc --make $< -o $@

i3j: GenJvm.hs ParInstant.hs LexInstant.hs Jasmin.hs JvmState.hs OptJvm.hs
	ghc --make $< -o $@

testj: tjcond

tjcond: i3j
	./i3j cond2.i3
	jasmin out.j
	java Instant

tjwhile: i3j
	echo "i=5;r=1;while(i>1){r=r*i;i=i-1}r" | ./i3j
	jasmin out.j
	java Instant

testl: i3l runtime.bc
	echo "i=5;r=1;while(i>1){r=r*i;i=i-1}r" | ./i3l
	llvm-as out.ll
	llvm-link -o a.out.bc out.bc runtime.bc
	lli a.out.bc

runtime.bc: runtime.ll
	llvm-as $<

TestInstant: TestInstant.hs ParInstant.hs LexInstant.hs
	ghc --make TestInstant.hs -o TestInstant

%.hs: %.y
	happy -gca $<

%.hs: %.x
	alex -g $<

ParInstant.hs: ParInstant.y
LexInstant.hs: LexInstant.x

TestInstant.hs: Instant.cf
	bnfc Instant.cf

ParInstant.y: Instant.cf
	bnfc Instant.cf

bnfclean:
	-rm -f $(BNFCRUST)

clean::
	-rm -f *.log *.aux *.hi *.o *.dvi *~ *.bak
	-rm -f DocInstant.ps
	-rm -f a.out a.out.bc out.ll out.bc out.j *.class

veryclean:: clean codeclean

distclean: clean
	-rm -f $(GENERATED)
