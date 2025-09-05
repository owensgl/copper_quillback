#!/bin/bash

vcf_in="/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/copper_combined.bcf"
out="/project/ctb-grego/ntbsykes/copper_quillback/structure/copper"

# Use PLINK to do some linkage pruning
plink --bcf $vcf_in \
        --double-id \
        --allow-extra-chr \
        --set-missing-var-ids @:# \
        --indep-pairwise 10 10 0.1 \
        --out $out

# Convert VCF to BED with PLINK and run a PCA
plink --bcf $vcf_in \
        --double-id \
        --allow-extra-chr \
        --set-missing-var-ids @:# \
        --extract $out.prune.in \
        --make-bed \
        --pca 10 \
        --out $out

# Change the first column in the BIM file to 0, to be read by ADMIXTURE
awk '{$1="0";print $0}' $out.bim > $out.bim.tmp
mv $out.bim.tmp $out.bim
