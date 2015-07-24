
THREADED = -threaded -optc-DTHREADED_RTS
GHC = ghc

DEBUG = 

ORDERED_FILES = ../../Control/Ordered/CmmHooks.cmm ../../Control/Ordered/CCode.c ../../Control/Ordered/STM.hs -I../../../../rts
ORDEREDREMSET_FILES = ../../Control/OrderedRemSet/CmmHooks.cmm ../../Control/OrderedRemSet/CCode.c ../../Control/OrderedRemSet/STM.hs -I../../../../rts
FULL_FILES = ../../Control/Full/CmmHooks.cmm ../../Control/Full/CCode.c ../../Control/Full/STM.hs -I../../../../rts
PARTIAL_FILES = ../../Control/Partial/CmmHooks.cmm ../../Control/Partial/CCode.c ../../Control/Partial/STM.hs -I../../../../rts
CPSFULL_FILES = ../../Control/CPSFull/CmmHooks.cmm ../../Control/CPSFull/CCode.c ../../Control/CPSFull/STM.hs -I../../../../rts
STMHASKELL_FILES = ../../Control/STMHaskell/STM.hs -I../../../../rts

OPT =

.PHONY:	full
full:	
	$(eval FILES := ../../Control/Full/CmmHooks.cmm ../../Control/Full/CCode.c ../../Control/Full/STM.hs -I../../../../rts)
	$(eval WHICHSTM := -DFABORT)

.PHONY:	partial 
partial:	
	$(eval FILES := ../../Control/Partial/CmmHooks.cmm ../../Control/Partial/CCode.c ../../Control/Partial/STM.hs -I../../../../rts)
	$(eval WHICHSTM := -DPABORT)

.PHONY:	ordered 
ordered:	
	$(eval FILES := ../../Control/Ordered/CmmHooks.cmm ../../Control/Ordered/CCode.c ../../Control/Ordered/STM.hs -I../../../../rts)
	$(eval WHICHSTM := -DORDERED)

.PHONY:	stmHaskell
stmHaskell:	
	$(eval FILES := ../../Control/STMHaskell/STM.hs -I../../../../rts)
	$(eval WHICHSTM := -DSTMHASKELL)

LockCoupling:	LockCoupling.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) ../cbits/gettime.c -DLOCKCOUPLING -fforce-recomp LockCoupling.hs Main.hs -o lockCoupling

.PHONY:	StraightForward
StraightForward:	StraightForwardSTM.hs Main.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(FILES) $(WHICHSTM) -DSTRAIGHT -fforce-recomp ../cbits/gettime.c StraightForwardSTM.hs Main.hs -o straightForward

Dissected:	DissectedSTM.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) $(PARTIAL_FILES) -DDISSECTED -fforce-recomp ../cbits/gettime.c DissectedSTM.hs Main.hs -o dissected

CAS:	CASLL.hs
	$(GHC) $(THREADED) $(DEBUG) $(OPT) -DCAS -fforce-recomp ../cbits/gettime.c CASLL.hs Main.hs -o cas
