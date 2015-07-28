

THREADED = -threaded -optc-DTHREADED_RTS
GHC = ghc ../../examples/cbits/gettime.c

DEBUG = -rtsopts

ORDERED_FILES = ../../Control/Ordered/CmmHooks.cmm ../../Control/Ordered/CCode.c ../../Control/Ordered/STM.hs -I../../../../rts
ORDEREDREMSET_FILES = ../../Control/OrderedRemSet/CmmHooks.cmm ../../Control/OrderedRemSet/CCode.c ../../Control/OrderedRemSet/STM.hs -I../../../../rts
FULL_FILES = ../../Control/Full/CmmHooks.cmm ../../Control/Full/CCode.c ../../Control/Full/STM.hs -I../../../../rts
PARTIAL_FILES = ../../Control/Partial/CmmHooks.cmm ../../Control/Partial/CCode.c ../../Control/Partial/STM.hs -I../../../../rts
CPSFULL_FILES = ../../Control/CPSFull/CmmHooks.cmm ../../Control/CPSFull/CCode.c ../../Control/CPSFull/STM.hs -I../../../../rts
TL2_FILES = ../../Control/PartialTL2/CmmHooks.cmm ../../Control/PartialTL2/CCode.c ../../Control/PartialTL2/STM.hs -I../../../../rts

OPT = -optc-O2

.PHONY:	llPartial

all:	llPartial llFull llSTMHaskell

llPartial:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(PARTIAL_FILES) -fforce-recomp ordered-linked-list.hs Main.hs -o partial

llFull:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -optc-g $(FULL_FILES) -DFABORT -fforce-recomp ordered-linked-list.hs Main.hs -o full

llSTMHaskell:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -optc-g ../../Control/STMHaskell/STM.hs -DSTMHASKELL -fforce-recomp ordered-linked-list.hs Main.hs -o stmHaskell

llOrdered:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -optc-g $(ORDEREDREMSET_FILES) -DORDERED -fforce-recomp ordered-linked-list.hs Main.hs -o ordered

