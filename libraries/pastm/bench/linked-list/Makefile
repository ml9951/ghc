

THREADED = -threaded -optc-DTHREADED_RTS
GHC = ghc ../../examples/cbits/gettime.c

DEBUG = -rtsopts


COMMON_FILES = -I../../../../rts ../../Control/Common/CmmHooks.cmm ../../Control/Common/STM.hs
TL2_FILES = ../../Control/TL2/CmmHooks.cmm ../../Control/TL2/CCode.c ../../Control/TL2/STM.hs 
PTL2_FILES = ../../Control/PartialTL2/CmmHooks.cmm ../../Control/PartialTL2/CCode.c ../../Control/PartialTL2/STM.hs
NOREC_FILES = ../../Control/NoRec/CmmHooks.cmm ../../Control/NoRec/CCode.c ../../Control/NoRec/STM.hs
ORDERED_NOREC_FILES = ../../Control/OrderedNoRec/CmmHooks.cmm ../../Control/OrderedNoRec/CCode.c ../../Control/OrderedNoRec/STM.hs
CHUNKED_TL2_FILES = ../../Control/ChunkedTL2/CmmHooks.cmm ../../Control/ChunkedTL2/CCode.c ../../Control/ChunkedTL2/STM.hs

OPT = 

STATS = -optc-DSTATS

.PHONY:	tl2 ptl2 norec orderedNoRec chunkedTL2

all:	ptl2 tl2 norec

chunkedTL2:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(STATS) $(CHUNKED_TL2_FILES) $(COMMON_FILES) -DCHUNKED_TL2 -optc-DCHUNKED_Tl2 -fforce-recomp ordered-linked-list.hs Main.hs -o chunkedTL2$(OPT)

orderedNoRec:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(STATS) $(ORDERED_NOREC_FILES) $(COMMON_FILES) -DORDERED_NOREC -optc-DORDERED_NOREC -fforce-recomp ordered-linked-list.hs Main.hs -o orderedNoRec$(OPT)

norec:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(STATS) $(NOREC_FILES) $(COMMON_FILES) -DNOREC -optc-DNOREC -fforce-recomp ordered-linked-list.hs Main.hs -o norec$(OPT)


ptl2:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(STATS) $(PTL2_FILES) $(COMMON_FILES) -DPTL2 -optc-DPTL2 -fforce-recomp ordered-linked-list.hs Main.hs -o ptl2$(OPT)


tl2:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(STATS) $(TL2_FILES) $(COMMON_FILES) -DFTL2 -optc-DFTL2 -fforce-recomp ordered-linked-list.hs Main.hs -o tl2$(OPT)

clean:	
	rm -f *.o *.hi ordered ordered-O* stmHaskell* full* partial* tl2* cpsFull* chunked* ff*

cabal-pkgs:
	cabal install optparse-applicative
	git clone https://github.com/cchalmers/pcg-random.git
	cd pcg-random; git checkout pure; cabal install


