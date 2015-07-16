

THREADED = -threaded
GHC = ghc
DEBUG = 

all:	llSTMHaskell llFull llPartial

llSTMHaskell:	ll.hs cbits/gettime.c 
	$(GHC) $(THREADED) $(DEBUG) -DSTMHASKELL -fforce-recomp cbits/gettime.c ll.hs -o llSTMHaskell

llFull:	ll.hs cbits/gettime.c
	$(GHC) $(THREADED) $(DEBUG) -DFABORT -fforce-recomp cbits/gettime.c ll.hs -o llFull

llPartial:	ll.hs cbits/gettime.c
	$(GHC) $(THREADED) $(DEBUG) -fforce-recomp cbits/gettime.c ll.hs -o llPartial

clean:
	rm *.hi *.o llSTMHaskell llFull llPartial



