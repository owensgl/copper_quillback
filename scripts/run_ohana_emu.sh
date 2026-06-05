in=$1 #Should be a vcf.gz file
out_prefix=$(echo $in | sed s/\.vcf\.gz//g)
eigen_out=$2 #Number of eigen_vectors for emu

ohana_dir="/project/ctb-grego/ntbsykes/copper_quillback/scripts/ohana/bin"
#Run emu PCA and Ohana with a vcf
#source /project/ctb-grego/ntbsykes/copper_quillback/scripts/emu_env/bin/activate

#Run emu
bash /project/ctb-grego/ntbsykes/copper_quillback/scripts/run_emu.sh $out_prefix $eigen_out

bash /project/ctb-grego/ntbsykes/copper_quillback/scripts/vcf2beagle.sh $in

$ohana_dir/convert bgl2lgm $out_prefix.beagle $out_prefix.lgm

for k in `seq 2 10`
do
  $ohana_dir/qpas $out_prefix.lgm -k $k -qo $out_prefix.${k}.q.matrix -fo $out_prefix.${k}.f.matrix -e 0.08 -mi 450
  $ohana_dir/nemeco $out_prefix.lgm $out_prefix.${k}.f.matrix -co $out_prefix.${k}.c.matrix  -mi 50
  $ohana_dir/convert cov2nwk $out_prefix.${k}.c.matrix $out_prefix.${k}.tree.nwk
  $ohana_dir/convert nwk2svg $out_prefix.${k}.tree.nwk $out_prefix.${k}.tree.svg
done
