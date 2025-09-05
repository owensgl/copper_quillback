#!/bin/bash

# Read in the command line arguments
in1="/project/ctb-grego/ntbsykes/copper_quillback/structure/copper_inside"
in2="/project/ctb-grego/ntbsykes/copper_quillback/structure/copper_outside"
in3="/project/ctb-grego/ntbsykes/copper_quillback/structure/quillback_inside"
in4="/project/ctb-grego/ntbsykes/copper_quillback/structure/quillback_outside"

k=$1

cd $in1
admixture --cv -j4 --bootstrap=100 $in1/combined.copper.inside.bed $k > $in1/copper.${k}.log
cd $in2
admixture --cv -j4 --bootstrap=100 $in2/combined.copper.outside.bed $k > $in2/copper.${k}.log
cd $in3
admixture --cv -j4 --bootstrap=100 $in3/combined.quillback.inside.bed $k > $in3/quillback.${k}.log
cd $in4
admixture --cv -j4 --bootstrap=100 $in4/combined.quillback.outside.bed $k > $in4/quillback.${k}.log


