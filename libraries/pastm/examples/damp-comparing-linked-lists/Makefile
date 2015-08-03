
THREADED = -threaded -optc-DTHREADED_RTS
GHC = ghc
DEBUG = 
OPT =

.PHONY:	full partial ordered stmHaskell PartialTL2 LockCoupling StraightForward Dissected CAS

full:	
	$(eval FILES := ../../Control/Full/CmmHooks.cmm ../../Control/Full/CCode.c ../../Control/Full/STM.hs -I../../../../rts)
	$(eval WHICHSTM := -DFABORT)

partial:	
	$(eval FILES := ../../Control/Partial/CmmHooks.cmm ../../Control/Partial/CCode.c ../../Control/Partial/STM.hs -I../../../../rts)
	$(eval WHICHSTM := -DPABORT)

ordered:	
	$(eval FILES := ../../Control/OrderedRemSet/CmmHooks.cmm ../../Control/OrderedRemSet/CCode.c ../../Control/OrderedRemSet/STM.hs -I../../../../rts)
	$(eval WHICHSTM := -DORDERED)

stmHaskell:	
	$(eval FILES := ../../Control/STMHaskell/STM.hs -I../../../../rts)
	$(eval WHICHSTM := -DSTMHASKELL)

PartialTL2:	
	$(eval FILES := ../../Control/PartialTL2/CmmHooks.cmm ../../Control/PartialTL2/CCode.c ../../Control/PartialTL2/STM.hs -I../../../../rts)
	$(eval WHICHSTM := -DPTL2)

LockCoupling:	LockCoupling.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) ../cbits/gettime.c -DLOCKCOUPLING -fforce-recomp LockCoupling.hs Main.hs -o lockCoupling

StraightForward:	StraightForwardSTM.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(FILES) $(WHICHSTM) -DSTRAIGHT -fforce-recomp ../cbits/gettime.c StraightForwardSTM.hs Main.hs -o straightForward

Dissected:	DissectedSTM.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(FILES) $(WHICHSTM) -DDISSECTED -fforce-recomp ../cbits/gettime.c DissectedSTM.hs Main.hs -o dissected

CAS:	CASLL.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DCAS -fforce-recomp ../cbits/gettime.c CASLL.hs Main.hs -o cas


clean:	
	rm -f  *.o *.hi lockCoupling straightForward cas dissected

