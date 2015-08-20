#!/bin/bash

EAGER_PABORT=-1
EAGER_FABORT=-2
COMMIT_PABORT=-3
COMMIT_FABORT=-4
COMMIT=-5
VALIDATE=-6

filename="${1%%.*}"

tmp=events

ghc-events show $1 > events.shown


grep -o ".*TRANSACTIONAL MEMORY.*" events.shown | sed "s/cap//; s/://g; s/TRANSACTIONAL MEMORY//; s/Eager Partial Abort/$EAGER_PABORT/; s/Eager Full Abort/$EAGER_FABORT/; s/Commit Time Partial Abort/$COMMIT_PABORT/; s/Commit Time Full Abort/$COMMIT_FABORT/; s/Committed Transaction/$COMMIT/; s/Start TX (info = //; s/).*//g; s/Begin Commit/$VALIDATE/" | tail -n+2 > $tmp.tmp

rm events.shown

octave --eval "events = load(\"$tmp.tmp\"); EagerPA = $EAGER_PABORT; EagerFA = $EAGER_FABORT; CommitPA = $COMMIT_PABORT; CommitFA = $COMMIT_FABORT; Commit = $COMMIT; Validate=$VALIDATE; save -mat7-binary $filename.mat"

mv $filename.mat matlab_scripts


