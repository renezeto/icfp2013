#!/bin/sh

set -ev

for x in check_status get_training lookup_problems make_guess; do
   ghc --make -O2 $x.hs
done
