#!/bin/bash

chr=$1
out=$(echo ${chr} | cut -d "." -f 1)

path_in="/project/ctb-grego/ntbsykes/copper_quillback/vcf/unfiltered"
path_out="/project/ctb-grego/ntbsykes/copper_quillback/vcf/pixy"
path_meta="/project/ctb-grego/ntbsykes/copper_quillback/meta"

# generate invariant-only VCF
vcftools --gzvcf ${path_in}/${chr} \
	--keep ${path_meta}/sample_list_quillback.txt \
	--remove-indels \
	--max-maf 0 \
	--max-alleles 1 \
	--minDP 5 \
	--maxDP 50 \
	--max-missing 0.75 \
	--recode --stdout | bgzip -c > temp_quillback_${out}_1

# generate variant-only VCF with population genetic filters
vcftools --gzvcf ${path_in}/${chr} \
        --keep ${path_meta}/sample_list_quillback.txt \
        --remove-indels \
        --mac 3 \
        --min-alleles 2 \
        --max-alleles 2 \
        --max-missing 0.75 \
        --minQ 20 \
        --minDP 5 \
        --maxDP 50 \
        --recode --stdout | bgzip -c > temp_quillback_${out}_2

# index both vcfs using bcftools
bcftools index temp_quillback_${out}_1
bcftools index temp_quillback_${out}_2

# combine the two VCFs
bcftools concat --allow-overlaps temp_quillback_${out}_1 temp_quillback_${out}_2 \
	-O z > ${path_out}/quillback_${out}_pixy.vcf.gz

# index the result
bcftools index -t ${path_out}/quillback_${out}_pixy.vcf.gz

# remove temporary files
rm temp_quillback_${out}*
