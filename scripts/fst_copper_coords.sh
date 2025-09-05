#!/bin/bash

pair=$1
out=$(echo ${pair} | cut -d "." -f 1)
path="/project/ctb-grego/ntbsykes/copper_quillback"

bcftools view ${path}/vcf/structure/copper_combined.bcf | perl ${path}/scripts/vcf2fst.pl \
				${path}/fst/copper/coords/coord_samples.txt \
				${path}/fst/copper/coords/pair_files/${pair} \
				> ${path}/fst/copper/coords/results/${out}.fst
