#!/bin/bash

chr=$1
out=$(echo ${chr} | cut -d "." -f 1)

path_in="/project/ctb-grego/ntbsykes/copper_quillback/vcf/unfiltered"
path_out="/project/ctb-grego/ntbsykes/copper_quillback/lostruct/quillback/bcf"
path_meta="/project/ctb-grego/ntbsykes/copper_quillback/meta"

# subset quality individuals
bcftools view ${path_in}/${chr} \
	-S ${path_meta}/sample_list_quillback.txt \
	-O z > temp_quillback_${out}_lostruct_1

# do the bulk of the filtering with vcftools
vcftools --gzvcf temp_quillback_${out}_lostruct_1 \
	--remove-indels \
	--maf 0.05 \
	--min-alleles 2 \
	--max-alleles 2 \
	--max-missing 1 \
	--minDP 5 \
	--maxDP 50 \
	--recode --stdout | gzip -c > temp_quillback_${out}_lostruct_2

# convert to bcf and index
bcftools view temp_quillback_${out}_lostruct_2 -O b > ${path_out}/quillback_${out}_lostruct.bcf

bcftools index ${path_out}/quillback_${out}_lostruct.bcf

rm temp_quillback_${out}_lostruct_1 temp_quillback_${out}_lostruct_2
