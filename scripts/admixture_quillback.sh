#!/bin/bash

# Read in the command line arguments
in="/project/ctb-grego/ntbsykes/copper_quillback/structure/quillback.bed"
k=$1

admixture --cv -j4 --bootstrap=100 $in $k > quillback.${k}.log
