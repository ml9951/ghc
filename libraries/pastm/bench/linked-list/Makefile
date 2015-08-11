

THREADED = -threaded -optc-DTHREADED_RTS
GHC = ghc ../../examples/cbits/gettime.c

DEBUG = -rtsopts



ORDEREDREMSET_FILES = ../../Control/OrderedRemSet/CmmHooks.cmm ../../Control/OrderedRemSet/CCode.c ../../Control/OrderedRemSet/STM.hs -I../../../../rts ../../Control/STM_IO/STM.hs ../../Control/STM_IO/CmmHooks.cmm
FULL_FILES = ../../Control/Full/CmmHooks.cmm ../../Control/Full/CCode.c ../../Control/Full/STM.hs -I../../../../rts ../../Control/STM_IO/STM.hs ../../Control/STM_IO/CmmHooks.cmm
PARTIAL_FILES = ../../Control/Partial/CmmHooks.cmm ../../Control/Partial/CCode.c ../../Control/Partial/STM.hs -I../../../../rts ../../Control/STM_IO/STM.hs ../../Control/STM_IO/CmmHooks.cmm
CPSFULL_FILES = ../../Control/CPSFull/CmmHooks.cmm ../../Control/CPSFull/CCode.c ../../Control/CPSFull/STM.hs -I../../../../rts ../../Control/STM_IO/STM.hs ../../Control/STM_IO/CmmHooks.cmm
PTL2_FILES = ../../Control/PartialTL2/CmmHooks.cmm ../../Control/PartialTL2/CCode.c ../../Control/PartialTL2/STM.hs -I../../../../rts ../../Control/STM_IO/STM.hs ../../Control/STM_IO/CmmHooks.cmm
CHUNKED_FILES = ../../Control/Chunked/CmmHooks.cmm ../../Control/Chunked/CCode.c ../../Control/Chunked/STM.hs -I../../../../rts ../../Control/STM_IO/STM.hs ../../Control/STM_IO/CmmHooks.cmm
FF_FILES = ../../Control/FastForward/CmmHooks.cmm ../../Control/FastForward/CCode.c ../../Control/FastForward/STM.hs -I../../../../rts ../../Control/STM_IO/STM.hs ../../Control/STM_IO/CmmHooks.cmm
STMHASKELL_FILES = ../../Control/STM_IO/STM.hs ../../Control/STM_IO/CmmHooks.cmm ../../Control/STMHaskell/STM.hs


OPT = 

STATS = -optc-DSTATS

.PHONY:	partial full stmHaskell ordered clean partialTL2

all:	partial full stmHaskell ordered partialTL2 cpsFull chunked ff

partial:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(STATS) $(PARTIAL_FILES) -fforce-recomp ordered-linked-list.hs Main.hs -o partial$(OPT)

full:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(STATS) $(FULL_FILES) -DFABORT -fforce-recomp ordered-linked-list.hs Main.hs -o full$(OPT)

stmHaskell:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(STATS) $(OPT) $(STMHASKELL_FILES) -DSTMHASKELL -fforce-recomp ordered-linked-list.hs Main.hs -o stmHaskell$(OPT)

ordered:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(STATS) $(ORDEREDREMSET_FILES) -DORDERED -fforce-recomp ordered-linked-list.hs Main.hs -o ordered$(OPT)

partialTL2:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(STATS) $(PTL2_FILES) -DPTL2 -fforce-recomp ordered-linked-list.hs Main.hs -o tl2Partial$(OPT)

cpsFull:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(STATS) $(OPT) $(CPSFULL_FILES) -DCPSFULL -fforce-recomp ordered-linked-list.hs Main.hs -o cpsFull$(OPT)

chunked:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(STATS) $(OPT) $(CHUNKED_FILES) -DCHUNKED -fforce-recomp ordered-linked-list.hs Main.hs -o chunked$(OPT)

ff:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(STATS) $(OPT) $(FF_FILES) -DFF -fforce-recomp ordered-linked-list.hs Main.hs -o ff$(OPT)

clean:	
	rm -f *.o *.hi ordered ordered-O* stmHaskell* full* partial* tl2* cpsFull* chunked* ff*

