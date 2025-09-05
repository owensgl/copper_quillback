#!/bin/bash

chr=$1
out=$(echo ${chr} | cut -d "." -f 1)

path_in="/project/ctb-grego/ntbsykes/copper_quillback/vcf/unfiltered"
path_out="/project/ctb-grego/ntbsykes/copper_quillback/vcf/inside_only"
path_meta="/project/ctb-grego/ntbsykes/copper_quillback/meta"

# subset species
bcftools view ${path_in}/${chr} \
	-S ${path_meta}/copper_only_inside.txt \
	-O z > temp_copper_${out}_1

# do the bulk of the filtering with vcftools
vcftools --gzvcf temp_copper_${out}_1 \
	--remove-indels \
	--maf 0.05 \
	--min-alleles 2 \
	--max-alleles 2 \
	--max-missing 0.75 \
	--minQ 20 \
	--minDP 5 \
	--maxDP 50 \
	--recode --stdout | gzip -c > temp_copper_${out}_2

# fill tags with bcftools
bcftools +fill-tags temp_copper_${out}_2 \
	-O z > temp_copper_${out}_3

# filter out sites with excess heterozygosity
bcftools view temp_copper_${out}_3 \
	-i 'INFO/ExcHet>0.007' \
	-O b > ${path_out}/copper_${out}_inside_only.bcf

bcftools index ${path_out}/copper_${out}_inside_only.bcf

rm temp_copper_${out}_1 temp_copper_${out}_2 temp_copper_${out}_3
