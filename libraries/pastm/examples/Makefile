

THREADED = -threaded
GHC = ghc
DEBUG = 

all:	ll counter

ll:	llSTMHaskell llFull llPartial llOrdered

.PHONY:	llOrdered
llOrdered:	ll.hs cbits/gettime.c 
	$(GHC) $(THREADED) $(DEBUG) -DORDERED -optl-g -optc-g -fforce-recomp cbits/gettime.c ll.hs -o llOrdered

.PHONY:	llSTMHaskell
llSTMHaskell:	ll.hs cbits/gettime.c 
	$(GHC) $(THREADED) $(DEBUG) -DSTMHASKELL -fforce-recomp cbits/gettime.c ll.hs -o llSTMHaskell

llFull:	ll.hs cbits/gettime.c
	$(GHC) $(THREADED) $(DEBUG) -DFABORT -fforce-recomp cbits/gettime.c ll.hs -o llFull

.PHONY:	llPartial
llPartial:	ll.hs cbits/gettime.c
	$(GHC) $(THREADED) $(DEBUG) -optc-g3 -fforce-recomp cbits/gettime.c ll.hs -o llPartial

counter:	counterFull counterPartial counterSTMHaskell

counterSTMHaskell:	counter.hs
	$(GHC) $(THREADED) $(DEBUG) -DSTMHASKELL -fforce-recomp counter.hs -o counterSTMHaskell

counterFull:	counter.hs 
	$(GHC) $(THREADED) $(DEBUG) -DFABORT -fforce-recomp counter.hs -o counterFull

counterPartial:	counter.hs
	$(GHC) $(THREADED) $(DEBUG) -fforce-recomp counter.hs -o counterPartial

full:	llFull counterFull

clean:
	rm *.hi *.o llSTMHaskell llFull llPartial counterFull counterPartial counterSTMHaskell




