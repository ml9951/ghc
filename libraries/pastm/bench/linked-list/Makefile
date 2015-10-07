

THREADED = -threaded -optc-DTHREADED_RTS
GHC = ghc ../../examples/cbits/gettime.c

DEBUG = -rtsopts


COMMON_FILES = -I../../../../rts ../../Control/Common/CmmHooks.cmm ../../Control/Common/STM.hs
TL2_FILES = ../../Control/TL2/CmmHooks.cmm ../../Control/TL2/CCode.c ../../Control/TL2/STM.hs 
PTL2_FILES = ../../Control/PartialTL2/CmmHooks.cmm ../../Control/PartialTL2/CCode.c ../../Control/PartialTL2/STM.hs

OPT = 

STATS = -optc-DSTATS

.PHONY:	tl2 ptl2

all:	ptl2 tl2

ptl2:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(STATS) $(PTL2_FILES) $(COMMON_FILES) -DPTL2 -optc-DPTL2 -fforce-recomp ordered-linked-list.hs Main.hs -o ptl2$(OPT)


tl2:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(STATS) $(TL2_FILES) $(COMMON_FILES) -DTL2 -optc-DTL2 -fforce-recomp ordered-linked-list.hs Main.hs -o tl2$(OPT)

clean:	
	rm -f *.o *.hi ordered ordered-O* stmHaskell* full* partial* tl2* cpsFull* chunked* ff*

cabal-pkgs:
	cabal install optparse-applicative 


