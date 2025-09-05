#!/bin/bash

pair=$1
out=$(echo ${pair} | cut -d "." -f 1)
path="/project/ctb-grego/ntbsykes/copper_quillback"

bcftools view ${path}/vcf/structure/quillback_combined.bcf | perl ${path}/scripts/vcf2fst.pl \
				${path}/fst/quillback/coords/coord_samples.txt \
				${path}/fst/quillback/coords/pair_files/${pair} \
				> ${path}/fst/quillback/coords/results/${out}.fst
