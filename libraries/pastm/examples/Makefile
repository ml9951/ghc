

THREADED = -threaded -optc-DTHREADED_RTS
GHC = ghc

DEBUG = -rtsopts

all:	ll counter

ll:	llSTMHaskell llFull llPartial llOrdered llOrderedRemSet llCPSFull

ORDERED_FILES = ../Control/OrderedRemSet/CmmHooks.cmm ../Control/OrderedRemSet/CCode.c ../Control/OrderedRemSet/STM.hs -I../../../rts
FULL_FILES = ../Control/Full/CmmHooks.cmm ../Control/Full/CCode.c ../Control/Full/STM.hs -I../../../rts
PARTIAL_FILES = ../Control/Partial/CmmHooks.cmm ../Control/Partial/CCode.c ../Control/Partial/STM.hs -I../../../rts
CPSFULL_FILES = ../Control/CPSFull/CmmHooks.cmm ../Control/CPSFull/CCode.c ../Control/CPSFull/STM.hs -I../../../rts
TL2_FILES = ../Control/TL2/CmmHooks.cmm ../Control/TL2/CCode.c ../Control/TL2/STM.hs -I../../../rts
PartialTL2_FILES = ../Control/PartialTL2/CmmHooks.cmm ../Control/PartialTL2/CCode.c ../Control/PartialTL2/STM.hs -I../../../rts
CHUNKED_FILES = ../Control/Chunked/CmmHooks.cmm ../Control/Chunked/CCode.c ../Control/Chunked/STM.hs -I../../../rts
FF_FILES = ../Control/FastForward/CmmHooks.cmm ../Control/FastForward/CCode.c ../Control/FastForward/STM.hs -I../../../rts 

OPT =

.PHONY:	llOrdered
llOrdered:	ll.hs cbits/gettime.c 
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DORDERED -optc-g $(ORDERED_FILES) -fforce-recomp cbits/gettime.c ll.hs -o llOrdered$(OPT)

.PHONY:	llSTMHaskell
llSTMHaskell:	ll.hs cbits/gettime.c 
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DSTMHASKELL ../Control/STMHaskell/STM.hs -fforce-recomp cbits/gettime.c ll.hs -o llSTMHaskell$(OPT)

.PHONY:	llFull
llFull:	ll.hs cbits/gettime.c
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DFABORT -optc-g $(FULL_FILES) -fforce-recomp cbits/gettime.c ll.hs -o llFull$(OPT)

.PHONY:	llPartial
llPartial:	ll.hs cbits/gettime.c
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(PARTIAL_FILES) -DPABORT -fforce-recomp cbits/gettime.c ll.hs -o llPartial$(OPT)

.PHONY:	llCPSFull
llCPSFull:	ll.hs cbits/gettime.c
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DCPSFULL -optc-g $(CPSFULL_FILES) -fforce-recomp cbits/gettime.c ll.hs -o llCPSFull$(OPT)

.PHONY:	llPartialTL2
llPartialTL2:	ll.hs cbits/gettime.c
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DPTL2 -optc-g $(PartialTL2_FILES) -fforce-recomp cbits/gettime.c ll.hs -o llPTL2$(OPT)

.PHONY:	llTL2
llTL2:	ll.hs cbits/gettime.c
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DTL2 -optc-g $(TL2_FILES) -fforce-recomp cbits/gettime.c ll.hs -o llTL2$(OPT)

.PHONY:	llChunked
llChunked:	ll.hs cbits/gettime.c
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DCHUNKED -optc-g $(CHUNKED_FILES) -fforce-recomp cbits/gettime.c ll.hs -o llChunked$(OPT)

.PHONY:	llFF
llFF:	ll.hs cbits/gettime.c
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DFF -optc-g $(FF_FILES) -fforce-recomp cbits/gettime.c ll.hs -o llFF$(OPT)

counter:	counterFull counterPartial counterSTMHaskell counterOrdered

counterSTMHaskell:	counter.hs
	$(GHC) $(THREADED) $(DEBUG) -DSTMHASKELL ../Control/STMHaskell/STM.hs -fforce-recomp counter.hs -o counterSTMHaskell$(OPT)

counterFull:	counter.hs 
	$(GHC) $(THREADED) $(DEBUG) -DFABORT $(FULL_FILES) -fforce-recomp counter.hs -o counterFull$(OPT)

.PHONY:	counterPartial
counterPartial:	counter.hs
	$(GHC) $(THREADED) $(DEBUG) $(PARTIAL_FILES) -fforce-recomp counter.hs -o counterPartial$(OPT)

.PHONY:	counterOrdered
counterOrdered:	counter.hs cbits/gettime.c 
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DORDERED $(ORDERED_FILES) -optc-g -fforce-recomp cbits/gettime.c counter.hs -o counterOrdered$(OPT)

.PHONY:	counterTL2
counterTL2:	counter.hs cbits/gettime.c 
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DTL2 $(TL2_FILES) -optc-g -optc-O0 -fforce-recomp cbits/gettime.c counter.hs -o counterTL2$(OPT)

.PHONY:	counterChunked
counterChunked:	counter.hs cbits/gettime.c
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DCHUNKED $(CHUNKED_FILES) -optc-g -optc-O0 -fforce-recomp cbits/gettime.c counter.hs -o counterChunked$(OPT)

retry:	test_retry.hs 
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(PARTIAL_FILES) -fforce-recomp cbits/gettime.c test_retry.hs -o retryPartial$(OPT)

multiplePartial:	multipleTVars.hs 
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(PARTIAL_FILES) -fforce-recomp cbits/gettime.c multipleTVars.hs -o multipleTVarsPartial$(OPT)

multipleOrdered:	multipleTVars.hs 
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(ORDERED_FILES) -DORDERED -fforce-recomp cbits/gettime.c multipleTVars.hs -o multipleTVarsOrdered$(OPT)

multipleFull:	multipleTVars.hs 
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(FULL_FILES) -DFABORT -fforce-recomp cbits/gettime.c multipleTVars.hs -o multipleTVarsFull$(OPT)

multipleChunked:	multipleTVars.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(CHUNKED_FILES) -DCHUNKED -fforce-recomp cbits/gettime.c multipleTVars.hs -o multipleTVarsChunked$(OPT)


#Synthetic
.PHONY:	syntheticPartial syntheticCPSFull
syntheticPartial:	synthetic.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(PARTIAL_FILES) $(STATS) -DPABORT -optc-DABORT -fforce-recomp cbits/gettime.c synthetic.hs -o syntheticPartial$(OPT)

syntheticCPSFull:	synthetic.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(CPSFULL_FILES) $(STATS) -optc-DABORT -DCPSFULL -fforce-recomp cbits/gettime.c synthetic.hs -o syntheticCPSFull$(OPT)



clean:
	rm -f *.hi *.o *-O* 




