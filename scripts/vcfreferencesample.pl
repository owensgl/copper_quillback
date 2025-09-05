#!/usr/bin/perl
#This adds a new sample that is just the reference allele.
#Usage: cat snptable.vcf > snptable.extrasample.txt
use warnings;
use strict;

while(<STDIN>){
    chomp;
    if ($_ =~ m/^##/){print "$_\n";next;}
    if ($_ =~ m/^#/){ print "$_\tReference_sample\n"; next;}
    print "$_\t0/0:.:.:.:.:.:.:.\n";
}#!/usr/bin/perl
#This adds a new sample that is just the reference allele.
#Usage: cat snptable.vcf > snptable.extrasample.txt
use warnings;
use strict;

while(<STDIN>){
    chomp;
    if ($_ =~ m/^##/){print "$_\n";next;}
    if ($_ =~ m/^#/){ print "$_\tReference_sample\n"; next;}
    print "$_\t0/0:.:.:.:.:.:.:.\n";
}
