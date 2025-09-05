chr=$1
out=$chr
path_in="/project/ctb-grego/ntbsykes/copper_quillback/broad_copper/vcf"
path_out="/project/ctb-grego/ntbsykes/copper_quillback/broad_copper/vcf/filtered"
#missing data max set to <20%.

if [ -f "$path_out/${out}.done" ]; then
    echo "File exists. Stopping script."
    exit 1
fi
bcftools +setGT \
        ${path_in}/${chr} \
        -O u -- \
        -t q -i 'FORMAT/DP<5 | FORMAT/DP>60' \
        -n . | \
bcftools view \
        -c 2 \
        -m2 \
        -M2 \
        -v snps \
        -i 'QUAL>20' \
        -q 0.02:minor \
        -O u | \
bcftools +fill-tags \
        -O u -- \
        -t all | \
bcftools view \
        -i 'INFO/AN>=232' \
        -O u | \
bcftools view \
        -i 'INFO/ExcHet>0.007' \
        -O z > ${path_out}/${out}.gz

echo "done" > $path_out/${out}.done
echo $chr
