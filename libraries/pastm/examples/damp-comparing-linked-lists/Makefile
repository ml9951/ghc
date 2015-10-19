
THREADED = -threaded -optc-DTHREADED_RTS
GHC = ghc
DEBUG = 
OPT =
STATS = -optc-DSTATS

.PHONY:	full partial ordered stmHaskell PartialTL2 LockCoupling StraightForward Dissected CAS partialRTS

COMMON_FILES = ../../Control/Common/STM.hs ../../Control/Common/CmmHooks.cmm

full:	
	$(eval FILES := ../../Control/Full/CmmHooks.cmm ../../Control/Full/CCode.c ../../Control/Full/STM.hs -I../../../../rts)
	$(eval WHICHSTM := -DFABORT)
	$(eval STM := Full)

partial:	
	$(eval FILES := ../../Control/Partial/CmmHooks.cmm ../../Control/Partial/CCode.c ../../Control/Partial/STM.hs -I../../../../rts)
	$(eval WHICHSTM := -DPABORT)
	$(eval STM := Partial)

ordered:	
	$(eval FILES := ../../Control/OrderedRemSet/CmmHooks.cmm ../../Control/OrderedRemSet/CCode.c ../../Control/OrderedRemSet/STM.hs -I../../../../rts)
	$(eval WHICHSTM := -DORDERED)
	$(eval STM := Ordered)

stmHaskell:	
	$(eval FILES := ../../Control/STMHaskell/STM.hs -I../../../../rts)
	$(eval WHICHSTM := -DSTMHASKELL)
	$(eval STM := STMHaskell)

chunkedTL2:	
	$(eval FILES := ../../Control/ChunkedTL2/STM.hs ../../Control/ChunkedTL2/CCode.c ../../Control/ChunkedTL2/CmmHooks.cmm -I../../../../rts)
	$(eval WHICHSTM := -DCHUNKED_TL2)
	$(eval STM := ChunkedTL2)

chunked:	
	$(eval FILES := ../../Control/Chunked/CmmHooks.cmm ../../Control/Chunked/CCode.c ../../Control/Chunked/STM.hs -I../../../../rts)
	$(eval WHICHSTM := -DCHUNKED)
	$(eval STM := Chunked)

partialTL2:	
	$(eval FILES := ../../Control/PartialTL2/CmmHooks.cmm ../../Control/PartialTL2/CCode.c ../../Control/PartialTL2/STM.hs -I../../../../rts)
	$(eval WHICHSTM := -DPTL2)
	$(eval STM := PTL2)



mvar:	LockCoupling.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) ../cbits/gettime.c -DLOCKCOUPLING -fforce-recomp LockCoupling.hs Main.hs -o mvar$(OPT)

straight:	StraightForwardSTM.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(COMMON_FILES) $(OPT) $(FILES) $(WHICHSTM) $(STATS) -DSTRAIGHT -fforce-recomp ../cbits/gettime.c StraightForwardSTM.hs Main.hs -o straightForward$(STM)$(OPT)

dissected:	DissectedSTM.hs
	$(GHC) $(THREADED) $(DEBUG) $(COMMON_FILES) $(OPT) $(FILES) $(WHICHSTM) $(STATS) -DDISSECTED -fforce-recomp ../cbits/gettime.c DissectedSTM.hs Main.hs -o dissected$(STM)$(OPT)

cas:	CASLL.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DCAS -fforce-recomp ../cbits/gettime.c CASLL.hs Main.hs -o cas$(OPT)


clean:	
	rm -f  *.o *.hi lockCoupling* straightForward* cas* dissected*

