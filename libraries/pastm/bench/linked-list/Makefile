

THREADED = -threaded -optc-DTHREADED_RTS
GHC = ghc ../../examples/cbits/gettime.c

DEBUG = -rtsopts

ORDERED_FILES = ../../Control/Ordered/CmmHooks.cmm ../../Control/Ordered/CCode.c ../../Control/Ordered/STM.hs -I../../../../rts
ORDEREDREMSET_FILES = ../../Control/OrderedRemSet/CmmHooks.cmm ../../Control/OrderedRemSet/CCode.c ../../Control/OrderedRemSet/STM.hs -I../../../../rts
FULL_FILES = ../../Control/Full/CmmHooks.cmm ../../Control/Full/CCode.c ../../Control/Full/STM.hs -I../../../../rts
PARTIAL_FILES = ../../Control/Partial/CmmHooks.cmm ../../Control/Partial/CCode.c ../../Control/Partial/STM.hs -I../../../../rts
CPSFULL_FILES = ../../Control/CPSFull/CmmHooks.cmm ../../Control/CPSFull/CCode.c ../../Control/CPSFull/STM.hs -I../../../../rts
PTL2_FILES = ../../Control/PartialTL2/CmmHooks.cmm ../../Control/PartialTL2/CCode.c ../../Control/PartialTL2/STM.hs -I../../../../rts

OPT = 

.PHONY:	partial full stmHaskell ordered clean partialTL2

all:	partial full stmHaskell ordered

partial:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(PARTIAL_FILES) -fforce-recomp ordered-linked-list.hs Main.hs -o partial$(OPT)

full:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(FULL_FILES) -DFABORT -fforce-recomp ordered-linked-list.hs Main.hs -o full$(OPT)

stmHaskell:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) ../../Control/STMHaskell/STM.hs -DSTMHASKELL -fforce-recomp ordered-linked-list.hs Main.hs -o stmHaskell$(OPT)

ordered:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(ORDEREDREMSET_FILES) -DORDERED -fforce-recomp ordered-linked-list.hs Main.hs -o ordered$(OPT)

partialTL2:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(PTL2_FILES) -DPTL2 -fforce-recomp ordered-linked-list.hs Main.hs -o tl2Partial$(OPT)

clean:	
	rm -f *.o *.hi ordered ordered-O* stmHaskell* full* partial* tl2*

