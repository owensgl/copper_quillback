#!/bin/bash

pair=$1
out=$(echo ${pair} | cut -d "." -f 1)
path="/project/ctb-grego/ntbsykes/copper_quillback"

bcftools view ${path}/vcf/structure/quillback_combined.bcf | perl ${path}/scripts/vcf2fst.pl \
				${path}/fst/quillback/locale/locale_samples.txt \
				${path}/fst/quillback/locale/pair_files/${pair} \
				> ${path}/fst/quillback/locale/${out}.fst
