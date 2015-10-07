

THREADED = -threaded -optc-DTHREADED_RTS
GHC = ghc ../../examples/cbits/gettime.c

DEBUG = -rtsopts


COMMON_FILES = -I../../../../rts ../../Control/Common/CmmHooks.cmm ../../Control/Common/STM.hs
TL2_FILES = ../../Control/TL2/CmmHooks.cmm ../../Control/TL2/CCode.c ../../Control/TL2/STM.hs -I../../../../rts 

OPT = 

STATS = -optc-DSTATS

.PHONY:	partial

all:	partial

partial:	ordered-linked-list.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(STATS) $(TL2_FILES) $(COMMON_FILES) -DTL2 -optc-DTL2 -fforce-recomp ordered-linked-list.hs Main.hs -o tl2$(OPT)

clean:	
	rm -f *.o *.hi ordered ordered-O* stmHaskell* full* partial* tl2* cpsFull* chunked* ff*

cabal-pkgs:
	cabal install optparse-applicative 


