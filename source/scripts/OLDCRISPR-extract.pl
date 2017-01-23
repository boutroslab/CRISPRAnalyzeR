#!/usr/bin/perl
use strict;
use warnings;

#vars

my $decision;
my $comparebowtie;
my $readfastq;
my $filename;
my $pattern;
my $maschinepattern;
my $inputline;
my $fasta;
my $fastqout;
my $count;
my $targetid;
my $patternlength;
my $sequenceline;

my $target;
my %reads;


sub reverse_complement_IUPAC {
        my $dna = shift;

	# reverse the DNA sequence
        my $revcomp = reverse($dna);

	# complement the reversed DNA sequence
        $revcomp =~ tr/ABCDGHMNRSTUVWXYabcdghmnrstuvwxy/TVGHCDKNYSAABWXRtvghcdknysaabwxr/;
        return $revcomp;
}
 
# ARGS:

#		[Regular expression used to extract 20 bp target sequence]
#		[FastqFile]
#		[Reverse complement?]
#		[Regular expression InputpatternSequencer]

#		

# Open files
$filename = $ARGV[1];
open ($readfastq, "<", $filename) or die $!;

print "Open file:".$filename."\n";

if ($ARGV[1]=~m/^(.+)(\.)fastq$/)
{$filename = $1;	}
open ($fastqout, ">", $filename."_extracted.fastq") or die $!; 


########### 
#	Sequence Extraction
# go through fastq file line by line
# detect READID, then the sequence, and finally the necessary quality parameters

#	@M01100:17:000000000-A8W39:1:1101:15603:1718 1:N:0:2
#	ATAGTCTCTTGTGGAAAGGACGAAACACCGGAGACAAGGGCATCATCTAGTTTTAGAGCTAGAAATAGCAAGTTAAAATAAGGCTAGTCCGTTATCAACTT
#	+
#	AB3>ADFFFFFFGGGBFCFFGCA22A4BEEC2E2AGG2F2A1FHFEFHHHHHHHGHHHHHHHHHHGHHHHHFBGHFHHHHHHHHHHHHHHEFGEH4F4GHH


print "Extracting fastq data . . .\n";

	if ($ARGV[0] eq "default")
	{ $pattern = "ACC(.{20,21})GTTT";
	}
	else
	{
		$pattern = $ARGV[0] ;
	#$pattern = qr/$ARGV[0]/ ;
	}
	
	# if ($ARGV[3] eq "default")
# 	{ $maschinepattern = qr/.{1}M01100(.+)/;
# 	}
# 	else
# 	{
# 		$maschinepattern = qr/.{1}$ARGV[3](.+)/;
# 	}

print "Pattern used:".$pattern."\n";
$patternlength = int(length($pattern))-5;

my %fastq;

while($inputline = <$readfastq>)
{
	
	# look for the read ID
	# create new entry in @fastq that will carry all the information for the new fastq file with extracted information
	
	# look for @M01100:14:000000000-A78J1:1:1101:13277:2172 1:N:0:1 to identify read
	
	if($inputline=~m/^@([\d\w]{3,}.+)/)	
	{ 	
		# create read file, as we reached a line with a new readID
		$fastq{'readID'}=$1;
		$fastq{'valid'}=0;
		$fastq{'seqvalid'}=0;	
		$fastq{'qcfound'}=0;
		
		$fastq{'count'}=1;
	}
	else {

	#if($inputline=~m/$maschinepattern/)	
	#{ 	
	#	# create read file, as we reached a line with a new readID
	#	$fastq{'readID'}=$1;
	#	$fastq{'valid'}=0;
	#	$fastq{'seqvalid'}=0;
	#	
	#}
	
	# look for the sequence we are interested in, which consist of ACCG, followed by 20 nts as well as 2-4Ts and AGAGC
	# ACCG(20nt)T{2,4}AGAGC is the detection sequence, here the target sequence is defined within the (20nt)
	# We allow sequencing errors in multiple T region by deletion up to two Ts and in addition recognize the sequence by the last
	# nucleotides present in the vector system by the BbsI RS, which is ACCG

	
	if($inputline=~m/$pattern/ && $fastq{'valid'}==0 && $fastq{'readID'} ne "" && $fastq{'count'}==1)	
	{
		
		
		$sequenceline = $inputline;
		#$fastq[$count]{'sequence'} = $1;
		$fastq{'sequence'} = $1;
		
		$patternlength = length($1);
		
		$fastq{'seqvalid'}=1;
		$fastq{'count'}=2;
		
		$fastq{'stringstart'}= index($sequenceline, $fastq{'sequence'});
		
		if ($ARGV[2] eq "rev")
		{
		$fastq{'sequence'} = reverse_complement_IUPAC($fastq{'sequence'});
		}
		

	}
	
	if($inputline=~m/\+/ && $fastq{'seqvalid'}==1 && $fastq{'count'}==2)
	{
		# look for the + only to know that next line is QC
		$fastq{'qcfound'}=1;
		$fastq{'count'}=3;

	}
	
	# look for the quality parameters
	if ($inputline=~m/.{2,}/ && $fastq{'seqvalid'}==1 && $fastq{'qcfound'}== 1 && $fastq{'count'}==3)
	{
	
			
		$fastq{'quality'} = substr($inputline, $fastq{'stringstart'}, $patternlength);
	
		# print output in fastq format
		#	READID:		@M01100:17:000000000-A8W39:1:1101:15603:1718 1:N:0:2
		#	Sequence:	ATAGTCTCTTGTGGAAAGGACGAAACACCGGAGACAAGGGCATCATCTAGTTTTAGAGCTAGAAATAGCAAGTTAAAATAAGGCTAGTCCGTTATCAACTT
		#				+
		#	Quality:	AB3>ADFFFFFFGGGBFCFFGCA22A4BEEC2E2AGG2F2A1FHFEFHHHHHHHGHHHHHHHHHHGHHHHHFBGHFHHHHHHHHHHHHHHEFGEH4F4GHH
	
		#write to new fastqfile
		print $fastqout "@".$fastq{'readID'}."\n".$fastq{'sequence'}."\n+\n".$fastq{'quality'}."\n";

		## Free space		
		%fastq=();
		$fastq{'qcfound'}=0;
		$fastq{'count'}=0;
		
	}
	
	} #end of else
	
}


print "Fastq data extracted successfully\n";

close($readfastq);
close($fastqout);
