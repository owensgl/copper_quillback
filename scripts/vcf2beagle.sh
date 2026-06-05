#!/bin/bash

# Define input and output prefix
INPUT_VCF=$1
OUT_PREFIX=$(echo $INPUT_VCF | sed s/\.vcf\.gz//g)

# 1. Extract chromosome names from the VCF header
# This avoids having to manually type "genome_hic_scaffold_X"
CHRS=$(bcftools query -H -f '%CHROM\n' $INPUT_VCF | grep -v "#" | uniq)

# 2. Loop through each chromosome
for CHR in $CHRS; do
    echo "Processing: $CHR"
    
    # Run vcftools for just this chromosome
    vcftools --gzvcf $INPUT_VCF \
             --chr $CHR \
             --BEAGLE-GL \
             --out "${OUT_PREFIX}.${CHR}"
done

# 3. Combine the files
# Beagle files have a header, so we keep the header from the first file
# and skip it for all subsequent files.
FIRST_CHR=$(echo $CHRS | awk '{print $1}')
cat "${OUT_PREFIX}.${FIRST_CHR}.BEAGLE.GL" > "${OUT_PREFIX}.beagle"

# Append the rest, skipping the first line (header) of each
for CHR in $(echo $CHRS | sed "s/$FIRST_CHR//"); do
    tail -n +2 "${OUT_PREFIX}.${CHR}.BEAGLE.GL" >> "${OUT_PREFIX}.beagle"
done

# 4. Clean up intermediate files (optional)
 rm ${OUT_PREFIX}.*.BEAGLE.GL
 rm ${OUT_PREFIX}.*.log

echo "Done! Combined file is in ${OUT_PREFIX}.beagle"
