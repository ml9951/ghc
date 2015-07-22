

THREADED = -threaded
GHC = ghc
DEBUG = 

all:	ll counter

ll:	llSTMHaskell llFull llPartial llOrdered

ORDERED_FILES = ../Control/Ordered/CmmHooks.cmm ../Control/Ordered/CCode.c ../Control/Ordered/STM.hs -I../../../rts
ORDEREDDC_FILES = ../Control/Ordered_Dirty_Clean/CmmHooks.cmm ../Control/Ordered_Dirty_Clean/CCode.c ../Control/Ordered_Dirty_Clean/STM.hs -I../../../rts
OPT =

.PHONY:	llOrdered
llOrdered:	ll.hs cbits/gettime.c 
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DORDERED -optc-g $(ORDERED_FILES) -fforce-recomp cbits/gettime.c ll.hs -o llOrdered

.PHONY:	llSTMHaskell
llSTMHaskell:	ll.hs cbits/gettime.c 
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DSTMHASKELL -fforce-recomp cbits/gettime.c ll.hs -o llSTMHaskell

.PHONY:	llFull
llFull:	ll.hs cbits/gettime.c
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DFABORT -fforce-recomp cbits/gettime.c ll.hs -o llFull

.PHONY:	llPartial
llPartial:	ll.hs cbits/gettime.c
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -fforce-recomp cbits/gettime.c ll.hs -o llPartial

.PHONY:	llOrderedDC
llOrderedDC:	ll.hs cbits/gettime.c 
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DORDERED -optc-g $(ORDEREDDC_FILES) -fforce-recomp cbits/gettime.c ll.hs -o llOrderedDC


counter:	counterFull counterPartial counterSTMHaskell counterOrdered

counterSTMHaskell:	counter.hs
	$(GHC) $(THREADED) $(DEBUG) -DSTMHASKELL -fforce-recomp counter.hs -o counterSTMHaskell

counterFull:	counter.hs 
	$(GHC) $(THREADED) $(DEBUG) -DFABORT -fforce-recomp counter.hs -o counterFull

counterPartial:	counter.hs
	$(GHC) $(THREADED) $(DEBUG) -fforce-recomp counter.hs -o counterPartial

.PHONY:	counterOrdered
counterOrdered:	counter.hs cbits/gettime.c 
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DORDERED -optc-g $(ORDERED_FILES) -fforce-recomp cbits/gettime.c counter.hs -o counterOrdered


full:	llFull counterFull

clean:
	rm *.hi *.o llSTMHaskell llFull llPartial counterFull counterPartial counterSTMHaskell llOrdered




