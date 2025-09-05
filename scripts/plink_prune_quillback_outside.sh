#!/bin/bash

vcf_in="/project/ctb-grego/ntbsykes/copper_quillback/vcf/outside_quillback/combined.quillback.outside.vcf.gz"
out="/project/ctb-grego/ntbsykes/copper_quillback/structure/quillback_outside/combined.quillback.outside"

# Use PLINK to do some linkage pruning
plink --vcf $vcf_in \
        --double-id \
        --allow-extra-chr \
        --set-missing-var-ids @:# \
        --indep-pairwise 10 10 0.1 \
        --out $out

# Convert VCF to BED with PLINK and run a PCA
plink --vcf $vcf_in \
        --double-id \
        --allow-extra-chr \
        --set-missing-var-ids @:# \
        --extract $out.prune.in \
        --make-bed \
        --out $out

# Change the first column in the BIM file to 0, to be read by ADMIXTURE
awk '{$1="0";print $0}' $out.bim > $out.bim.tmp
mv $out.bim.tmp $out.bim
