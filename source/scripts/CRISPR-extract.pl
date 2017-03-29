#!/usr/bin/perl
use strict;
use warnings;

#vars

my $readfastq;
my $filename;
my $pattern;
my $inputline;
my $inputline2;
my $fastqout;
my $patternlength;
my $sequenceline;
my $counttotal;
my $countextracted;
my $stats;
my $logfile;


sub reverse_complement_IUPAC {
        my $dna = shift;

	# reverse the DNA sequence
        my $revcomp = reverse($dna);

	# complement the reversed DNA sequence
        $revcomp =~ tr/ABCDGHMNRSTUVWXYabcdghmnrstuvwxy/TVGHCDKNYSAABWXRtvghcdknysaabwxr/;
        return $revcomp;
}
 
# ARGS:

#		[Regular expression used to extract target sequence]
#		[FastqFile]
#		[is it in Reverse complement order]


#		

# Open files
$filename = $ARGV[1];
open ($readfastq, "<", $filename) or die $!;

print "Open file:\t".$filename."\n";

if ($ARGV[1]=~m/^(.+)(\.)fastq$/)
{$filename = $1;	}
open ($fastqout, ">", $filename."_extracted.fastq") or die $!; 

# make logfile
if (not defined $ARGV[3])
{
	$logfile = $filename . "_log.txt";
}
else
{
	if($ARGV[3] ne "")
	{
		$logfile = $ARGV[3];
	}
	else
	{
		$logfile = $filename . "_log.txt";
	}
}

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
	{ $pattern = "ACC(.{20,21})G";
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

print "Pattern used:\t".$pattern."\n";
$patternlength = int(length($pattern))-5;

my %fastq;
$counttotal = 0;
$countextracted = 0;


while($inputline = <$readfastq>)
{

	# look for the read ID
	# create new entry in @fastq that will carry all the information for the new fastq file with extracted information
	
	# look for @M01100:14:000000000-A78J1:1:1101:13277:2172 1:N:0:1 to identify read
	
	if($inputline=~m/^@([\d\w]{3,}.+?:.+)/)	
	{ 	
		# found a read, increase counting
		$counttotal++;
	
	
		# create read file, as we reached a line with a new readID
		$fastq{'readID'}=$1;
		$fastq{'valid'}=0;
		$fastq{'seqvalid'}=0;	
		$fastq{'qcfound'}=0;
		
		$fastq{'count'}=1;
		
		# go for next Lines to get sequence
		$inputline2 = <$readfastq>;
      	if($inputline2=~m/$pattern/ )	
		{ 	
		
			# found the required pattern, count this
			$countextracted++;
			
			$sequenceline = $inputline2;
			$fastq{'sequence'} = $1;
		
			$patternlength = length($1);
		
			$fastq{'seqvalid'}=1;
			$fastq{'count'}=2;
		
			$fastq{'stringstart'}= index($sequenceline, $fastq{'sequence'});
		
			if ($ARGV[2] eq "yes")
			{
				$fastq{'sequence'} = reverse_complement_IUPAC($fastq{'sequence'});
			}
			
			# check for unwanted stuff
			$fastq{'sequence'} =~ s/[\W]/N/g;
		
		}
		# check nextline
		$inputline2 = <$readfastq>;
		
		if($inputline2=~m/^\+$/ && $fastq{'seqvalid'}==1 && $fastq{'count'}==2)
		{
			# look for the + only to know that next line is QC
			$fastq{'qcfound'}=1;
			$fastq{'count'}=3;
		}	
		# check nextline
		$inputline2 = <$readfastq>;
		if ($inputline2=~m/.{2,}/ && $fastq{'seqvalid'}==1 && $fastq{'qcfound'}== 1 && $fastq{'count'}==3)
			{
			
				$fastq{'valid'}=1;	
				$fastq{'quality'} = substr($inputline2, $fastq{'stringstart'}, $patternlength);
			
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
		
		
		
	}
	
	
}

# print out counted stats
open ($stats, ">", $logfile) or die $!; 
print $stats "Total\tExtracted\n";
print $stats $counttotal . "\t" . $countextracted . "\n";
close($stats);

print "Fastq data extracted successfully\n";
print "Total Reads in this file:\t" . $counttotal . "\n";
print "Extracted Reads in this file with the mathcing pattern:\t" . $countextracted . "\n";
print "The provided pattern worked in:\t" . ($countextracted/$counttotal)*100 . "%\n";

close($readfastq);
close($fastqout);

