

THREADED = -threaded -optc-DTHREADED_RTS
GHC = ghc ../../examples/cbits/gettime.c

DEBUG = -rtsopts



ORDEREDREMSET_FILES = ../../Control/OrderedRemSet/CmmHooks.cmm ../../Control/OrderedRemSet/CCode.c ../../Control/OrderedRemSet/STM.hs -I../../../../rts ../../Control/Common/STM.hs ../../Control/Common/CmmHooks.cmm
PTL2_FILES = ../../Control/PartialTL2/CmmHooks.cmm ../../Control/PartialTL2/CCode.c ../../Control/PartialTL2/STM.hs -I../../../../rts ../../Control/Common/STM.hs ../../Control/Common/CmmHooks.cmm
CHUNKED_FILES = ../../Control/Chunked/CmmHooks.cmm ../../Control/Chunked/CCode.c ../../Control/Chunked/STM.hs -I../../../../rts ../../Control/Common/STM.hs ../../Control/Common/CmmHooks.cmm
FF_FILES = ../../Control/FastForward/CmmHooks.cmm ../../Control/FastForward/CCode.c ../../Control/FastForward/STM.hs -I../../../../rts ../../Control/Common/STM.hs ../../Control/Common/CmmHooks.cmm
STMHASKELL_FILES = ../../Control/Common/STM.hs ../../Control/Common/CmmHooks.cmm ../../Control/STMHaskell/STM.hs

NOREC_FILES = ../../Control/NoRec/CmmHooks.cmm ../../Control/NoRec/CCode.c ../../Control/NoRec/STM.hs -I../../../../rts ../../Control/Common/STM.hs ../../Control/Common/CmmHooks.cmm

OPT = 

STATS = -optc-DSTATS

.PHONY:	partial full stmHaskell ordered clean partialTL2 cpsFull

all:	partial full stmHaskell ordered partialTL2 cpsFull chunked ff

partial:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(STATS) $(NOREC_FILES) -DPABORT -optc-DPABORT -fforce-recomp ordered-linked-list.hs Main.hs -o partial$(OPT)

full:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(STATS) $(NOREC_FILES) -DFABORT -optc-DFABORT -fforce-recomp ordered-linked-list.hs Main.hs -o full$(OPT)

stmHaskell:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(STATS) $(OPT) $(STMHASKELL_FILES) -DSTMHASKELL -fforce-recomp ordered-linked-list.hs Main.hs -o stmHaskell$(OPT)

ordered:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(STATS) $(ORDEREDREMSET_FILES) -DORDERED -fforce-recomp ordered-linked-list.hs Main.hs -o ordered$(OPT)

partialTL2:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(STATS) $(PTL2_FILES) -DPTL2 -fforce-recomp ordered-linked-list.hs Main.hs -o tl2Partial$(OPT)

cpsFull:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(STATS) $(OPT) $(NOREC_FILES) -DCPSFULL -optc-DCPSFULL -fforce-recomp ordered-linked-list.hs Main.hs -o cpsFull$(OPT)

chunked:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(STATS) $(OPT) $(CHUNKED_FILES) -DCHUNKED -fforce-recomp ordered-linked-list.hs Main.hs -o chunked$(OPT)

ff:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(STATS) $(OPT) $(FF_FILES) -DFF -fforce-recomp ordered-linked-list.hs Main.hs -o ff$(OPT)

partialNoForall:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(STATS) $(OPT) $(PARTIALNOFORALL_FILES) -DPABORTNOFORALL -fforce-recomp ordered-linked-list.hs Main.hs -o partialNoForall$(OPT)	

clean:	
	rm -f *.o *.hi ordered ordered-O* stmHaskell* full* partial* tl2* cpsFull* chunked* ff*

