#!/usr/bin/perl

###################################################################################################################################################################################################
# setup the software's infrastructure
###################################################################################################################################################################################################

use Bio::DB::Fasta;
use Bio::SeqIO;
use Bio::Tools::GFF;
use Scalar::Util qw(looks_like_number);
use Bio::SeqFeature::Generic; #important package to handle sequence formats and objects
use Bio::Location::Split; #library to make splitted location objects
use Set::IntervalTree; #library providing methods for using interval trees
use JSON::XS qw(encode_json decode_json); #library to encode perl objects in a file
use File::Slurp qw(read_file write_file); #library to write perl objects to a file
use List::MoreUtils qw{
      any all none notall true false
      firstidx first_index lastidx last_index
      insert_after insert_after_string
      apply indexes
      after after_incl before before_incl
      firstval first_value lastval last_value
      each_array each_arrayref
      pairwise natatime
      mesh zip uniq distinct minmax part}; #some math and list utilities, later needed to partition a list of entries
use List::Util qw(sum);
use Archive::Zip;
use Parallel::ForkManager; #important package to enable mutlithreading of the script
use Cwd;
use IPC::Cmd qw[can_run run];
use Getopt::Long qw(:config pass_through);	
use File::Grep qw( fgrep fmap fdo );
use Text::Wrap;
use Unix::Processors;

my %something = ();

GetOptions(
	    'output-dir=s'	=> \$something{"output-dir"},	
	    'sequence-files=s'	=> \$something{"sequence-files"},
		'databasepath=s'	=> \$something{"databasepath"},
	    'organism=s'	=> \$something{"organism"},
		'non-seed-length=s'=> \$something{"non-seed-length"},
		'mismatches-allowed=s'=> \$something{"mismatches-allowed"},
	    'version'		=> \$something{"version"},
	    'help'		=> \$something{"help"}
	);

for (my $i=0; $i<scalar(@ARGV); $i++){
			if (substr($ARGV[$i],0,1) eq '-' and $i < scalar(@ARGV)-1){
			if ($ARGV[$i] eq '-output-dir'		){$something{"output-dir"}		= int($ARGV[++$i]); }
			if ($ARGV[$i] eq '-sequence-files'		){$something{"sequence-files"}= int($ARGV[++$i]); }	
			if ($ARGV[$i] eq '-databasepath'	){$something{"databasepath"}	= int($ARGV[++$i]); }
			if ($ARGV[$i] eq '-organism'		){$something{"organism"}	= int($ARGV[++$i]); }	        
			if ($ARGV[$i] eq '-non-seed-length'			){$something{"non-seed-length"}		= int($ARGV[++$i]); }
			if ($ARGV[$i] eq '-mismatches-allowed'		){$something{"mismatches-allowed"}	= int($ARGV[++$i]); }
			}
		}

if(!defined($something{"output-dir"}) or !(-d $something{"output-dir"})){ $something{"output-dir"}="." }
if(!defined($something{"sequence-files"}) or !(-e $something{"sequence-files"})){ die "The sequence file ".$something{"sequence-files"}." could not be opened. Either the user has no rights the read it or the file does not exist." }
if(!defined($something{"databasepath"}) or !(-d $something{"databasepath"})){ $something{"databasepath"}="." }
if(!defined($something{"organism"})){ $something{"organism"}="drosophila_melanogaster" }
if(!defined($something{"non-seed-length"})){ $something{"non-seed-length"}=1 }
if(!defined($something{"mismatches-allowed"})){ $something{"mismatches-allowed"}=0 }

my ($script_name,$script_version,$script_date,$script_years) = ('reannotate-crispr','0.0.1','2016-27-07','2013-2016');

$something{"version_string"} = "$script_name, version $script_version, $script_date\nAuthor $script_years Florian Heigwer\n";
$something{"help_string"} = qq{Usage: reannotate-crispr [--options=value] ...
Options:
--output-dir=<path/to/dir>          - A working directory as unix path to directory. (default: .)
--sequence-files=<path/to/dir>      - A fasta formatted file of sgRNA spacer sequences (not-including PAM, mandatory).
--databasepath=<path/to/dir>        - Select folder where genome data is deposit  (default: .)
--organism=<string>                 - Please type the name of the reference organism as given in the database (default: drosophila_melanogaster)
--non-seed-length=<int>             - Specify the non-seed length in bp (the number of 5' basepairs to be ignore by the aligner, default: 1)
--mismatches-allowed=<int>          - Specify the number of mismatches allowed in a "valid" alignment (default: 0)
--version                           - Show version.
--help                              - Show this message.
};

if(defined($something{"version"})){ die $something{"version_string"} }elsif(defined($something{"help"})){die $something{"help_string"}}



my %trees               = ();
my $seqio_obj           = "";
my $parallel_number     = 2;

#$temp_dir = scalar localtime();
#$temp_dir =~ s/\s+/\_/ig;
#$temp_dir =~ s/\W+/\_/ig;
#$temp_dir =~ s/[^\w]/_/ig;
if ($something{"output-dir"}=~m/\/$/) {
  $temp_dir=$something{"output-dir"};#.$temp_dir;
}else{
  $temp_dir=$something{"output-dir"}."/";#.$temp_dir;
}
#mkdir($temp_dir) or die $!;
#system('chmod -R o+rwx '.$temp_dir.';');

	
my $databasepath;
if ($something{"databasepath"}!~m/\/$/) {
  $something{"databasepath"}.= "/" ;
}
$databasepath = $something{"databasepath"} . $something{"organism"} . "/" . $something{"organism"};

	
	my $temp = "";
	open my $INFILE, $something{"sequence-files"};
		while (<$INFILE>) {
			my $line = $_;
			chomp $line;
			$temp.= $line."\n";
		}
	close $INFILE;
	if (!($temp=~m/>/)) {
		die "No Sequence has been entered: FASTA Sequences need to start with \">\"\n";
	}
	open(my $SEQ, ">", $temp_dir . "/seq.fasta") or die "cannot open seq fasta\n";
	my @sequences = split( "\n", $temp );
	my $seqname="";
	foreach my $sequence (@sequences) {
		$sequence =~ s/[\s\n]//ig;
		if ($sequence=~m/^>/) {
			$seqname=$sequence;
		}elsif($sequence =~ m/([^ACGT]+)/ig){
			die "FASTA sequences may not contain the letter \"".$1."\"\n" ;
		}elsif(length($sequence)<16){
			die "CRISPR sequences may not be shorter than 16 bp\n";
		}else{
			print $SEQ $seqname."\n".substr($sequence,$something{"non-seed-length"})."AAG\n";
			print $SEQ $seqname."\n".substr($sequence,$something{"non-seed-length"})."TAG\n";
			print $SEQ $seqname."\n".substr($sequence,$something{"non-seed-length"})."CAG\n";
			print $SEQ $seqname."\n".substr($sequence,$something{"non-seed-length"})."GAG\n";
			print $SEQ $seqname."\n".substr($sequence,$something{"non-seed-length"})."GGG\n";
			print $SEQ $seqname."\n".substr($sequence,$something{"non-seed-length"})."CGG\n";
			print $SEQ $seqname."\n".substr($sequence,$something{"non-seed-length"})."AGG\n";
			print $SEQ $seqname."\n".substr($sequence,$something{"non-seed-length"})."TGG\n";
		}
	}
	close $SEQ;
		##################################################################################################################################################################################################
		###################################################################################################################################################################################################
		#system( '/usr/bin/bowtie ' . $databasepath.".genome" . ' ' . $temp_dir . "/" .'seq.fasta -f -v 3 -y -k 30 -S --sam-nohead --sam-nosq -p 4  > ' . $temp_dir .'/primary_out.bwt' );
		if (-e $databasepath.".genome.1.ebwtl") {
			system( 'bowtie ' . $databasepath.".genome" . ' ' . $temp_dir . "/" .'seq.fasta -f -v 3 -y -a -S --sam-nohead --sam-nosq --large-index -p 4  > ' . $temp_dir .'/primary_out.bwt' );
            print 'large '.'bowtie ' . $databasepath.".genome" . ' ' . $temp_dir . "/" .'seq.fasta -f -v 3 --quiet -y -a -S --sam-nohead --large-index  --sam-nosq -p 4  > ' . $temp_dir .'/primary_out.bwt'."\n";
          }else{
			system( 'bowtie ' . $databasepath.".genome" . ' ' . $temp_dir . "/" .'seq.fasta -f -v 3 -y -a -S --sam-nohead --sam-nosq -p 4  > ' . $temp_dir .'/primary_out.bwt' );
            print 'small '.'bowtie ' . $databasepath.".genome" . ' ' . $temp_dir . "/" .'seq.fasta -f -v 3 --quiet -y -a -S --sam-nohead --sam-nosq -p 4  > ' . $temp_dir .'/primary_out.bwt'."\n";
		  }
		open($bowtie,"$temp_dir/primary_out.bwt") or die $temp_dir,"cannot open primary bwt\n" ;
			while (my $line = <$bowtie>) {
                                chomp $line;
                                          $line=~m/NM:i:(\d+)/;
                                          my $edit_distance=$1;
                                          if (($edit_distance <= $something{"mismatches-allowed"})) {
                                                my @line = split( "\t", $line );
						if ($line[2] ne "*") {
							 #decide if it is forward (fw) or backward (bw) query-sequence
                                                my $direction = "rc";
                                                if ($line[1] == 0 || $line[1] == 256) {
                                                      $direction = "fw";
                                                }
                                                my $seq = $line[0];
                                                my @matchstringo=make_mismatch_string (\$line,$something{"non-seed-length"}, $direction);
                                                if ( ($direction eq "fw" && $matchstringo[scalar(@matchstringo)-1] ne "X" && $matchstringo[scalar(@matchstringo)-2] ne "X" && $matchstringo[scalar(@matchstringo)-3] ne "X")
                                                          || ($direction eq "rc" && $matchstringo[0] ne "X" && $matchstringo[1] ne "X" && $matchstringo[2] ne "X")
                                                ) {
							${$CRISPR_hash{$line[0]}}{"hits"}.=";;".$line[2]."¤¤".($line[3])."¤¤".($line[3]+@matchstringo)."¤¤".join("",@matchstringo)."¤¤".$edit_distance."¤¤".$direction."¤¤".$line[9];
                                                }
						}					
                                               
                                          }
                                    }
		close $bowtie;
		my %obj=();
		open( my $RESULTTABLE,">". $temp_dir . "/results.tab") or die $!;
			print $RESULTTABLE "Name\tTarget-chrome\(s\)\tStart\tEnd\tGene targets\tSpec-Score\tAnno-Score\tEff-Score\tMatchstring\tSequence\tDirection\tCDS_score\texon_score\tseed_GC\tdoench_score\txu_score\tdoench_30_mer\n";
		
		foreach my $query_id (keys(%CRISPR_hash)){
			my @new_score=(120,0,0,0,0,0,0,0);
			
			foreach my $hit (split(";;",${$CRISPR_hash{$query_id}}{"hits"})){
				my @hit_specs=split("¤¤",$hit);
				if($hit ne ""){
					if(0 < $new_score[0]-((20-(100/($hit_specs[2]-$hit_specs[1])*($hit_specs[4]+1))))){					
						$new_score[0]=$new_score[0]-((20-(100/($hit_specs[2]-$hit_specs[1])*($hit_specs[4]+1))));
					}else{
						$new_score[0]=0;
					}
				}
			}
			foreach my $hit (split(";;",${$CRISPR_hash{$query_id}}{"hits"})){
				if($hit ne ""){
					my %transcripts_hash;
					my %CDS_hash;
					my $crispr_seq="";
					my $doench_seq="";
					my $xu_seq="";
					$new_score[1]=0;
					$new_score[2]=0;
					$new_score[3]=0;
					$new_score[4]=0;
					$new_score[5]=0;
					$new_score[6]=0;
					$new_score[7]=0;
					my @hit_specs=split("¤¤",$hit);
					my $db	= Bio::DB::Fasta->new( $databasepath.'.dna.toplevel.fa', -makeid => \&make_my_id );
					
					if(!exists($trees{$hit_specs[0]})){$trees{$hit_specs[0]} = build_tree( $something{"databasepath"} . $something{"organism"} . "/" . $hit_specs[0] . "_indexed" )};
					if($hit_specs[5] eq "rc"){
						$crispr_seq=reverse_comp($db->seq($hit_specs[0],$hit_specs[1],($hit_specs[2]-1)));
						$doench_seq=reverse_comp($db->seq($hit_specs[0],$hit_specs[1]-3,$hit_specs[1]-4+30));
						$xu_seq=reverse_comp($db->seq($hit_specs[0],$hit_specs[1]-7,$hit_specs[1]-8+30));
					}else{
						$crispr_seq=$db->seq($hit_specs[0],$hit_specs[1]-$something{"non-seed-length"},$hit_specs[2]-1-$something{"non-seed-length"});
						$doench_seq=$db->seq($hit_specs[0],$hit_specs[1]-4-$something{"non-seed-length"},$hit_specs[1]-5-$something{"non-seed-length"}+30);
						$xu_seq=$db->seq($hit_specs[0],$hit_specs[1]-$something{"non-seed-length"},$hit_specs[1]-1-$something{"non-seed-length"}+30);
					}
					my @flank_array = find_base_count( $crispr_seq );
				
					if($crispr_seq=~m/GG$/){
						$new_score[2]++;
					}
					if($crispr_seq=~m/^G/){
						$new_score[2]++;
					}
					if(($flank_array[3]+$flank_array[1])>80){
						if($new_score[2]-1>=0){
							$new_score[2]--;
						}
					}
					@flank_array = find_base_count( substr( $crispr_seq, length($crispr_seq)-11,10) );
					$new_score[5]=($flank_array[3]+$flank_array[1])/100;
					$new_score[2]+=$new_score[5];
					$new_score[6]=calc_doench_score($doench_seq);
					$new_score[2]+=$new_score[6];
					
					$new_score[7]=calc_XU_score($xu_seq);
					$new_score[2]+=$new_score[7];
					
					my %score = calculate_CRISPR_score(\%trees, \%something, int($hit_specs[1]), int($hit_specs[2]), $hit_specs[0], 0, \@new_score);
					@new_score=@{$score{"new_score"}};
					my $gene_name="NA";
					my $gene_start=0;
					my $gene_end=0;
					my $crispr_annotations = $trees{$hit_specs[0]}->fetch( int($hit_specs[1]), int($hit_specs[2]) );
					foreach  my $anno ( @{$crispr_annotations} ) {
						if ( $anno =~ m/gene_(\S+)_([0-9]+)_([0-9]+)/ ) {
							$gene_start=$2;
							$gene_end=$3;
							$gene_name=$1;
						} 
					}
					if(($gene_start+$gene_end)!=0){
						my $annotations = $trees{$hit_specs[0]}->fetch( int($gene_start), int($gene_end) );
						foreach my $anno ( sort( @{$annotations} ) ) {
                                          if ( $anno =~ m/exon\:\:(\S+)\:\:(\d+)\:\:(\S+)\_(\d+)\_(\d+)$/) {
                                                my @pair = ( $4, $5 );
                                                ${$transcripts_hash{$1}}{$2}=\@pair;
                                          } elsif ( $anno =~ m/CDS\:\:(\S+)\:\:(\d+)\:\:(\S+)\_(\d+)\_(\d+)$/ ) {
                                                my @pair = ( $4, $5 );
                                                push @{ $CDS_hash{$1} }, \@pair;}
						}						
					}
					$new_score[1]=$new_score[1]*100/((5*(scalar(keys(%CDS_hash))))+(scalar(keys(%CDS_hash)))+(5*(scalar(keys(%transcripts_hash))))+1);
					$new_score[2]=($new_score[2]*100)/5;
					my $tmp_seq="";
					if($hit_specs[5] eq "rc"){
						$tmp_seq=reverse($hit_specs[3]);
					}else{
						$tmp_seq=$hit_specs[3];
					}
					print $RESULTTABLE $query_id."\t".$hit_specs[0]."\t".$hit_specs[1]."\t".$hit_specs[2]."\t".$gene_name."\t".$new_score[0]."\t".$new_score[1]."\t".$new_score[2]."\t".$tmp_seq."\t".$crispr_seq."\t".$hit_specs[5]."\t".$new_score[3]."\t".$new_score[4]."\t".$new_score[5]."\t".$new_score[6]."\t".$new_score[7]."\t".$doench_seq."\n";         
				}
			}	
		}		
		close $RESULTTABLE;
		
		print "\n########################################\n\n\n\nDONE\n\n\n\n########################################\n";
#########################################################################################
#name:      make_temp_fasta_file
#function:  creates a temporary fasta file for the bowtie index and builds a trees
#input:     (given id-Array, tree as referemce, something-Hashreference,
#           enzyme db, temp_dir, 1/0 if file or not)
#output:    N/A
#########################################################################################
sub make_temp_fasta_file {
      if ($_[5] == 0 && scalar(@{$_[0]})>=50) {
            print_error_html( $_[4], "Your input is more than 50 Sequences\n" );
            die;
      }
      if ($_[5] == 1 && scalar(@{$_[0]})>=500) { #@Flo die 500 sind hier Absicht?
            print_error_html( $_[4], "Your input is more than 50 lines with IDs.<br> Please shorten the list, or maybe change to option to FASTA.<br>" );
            die;
      }
      if ( !( $_[2]->{"specific_transcript"} eq "any") && scalar($_[0]) >1) {
            print_error_html( $_[4], "Transcript specificity is only defined for single gene analyses.\n" );
            die;
      }
      open (my $tempfile, ">", $_[4] . "/tempfile.fasta");
            foreach my $id (@{$_[0]}) { 
                  $id =~ s/\s//ig;
                  my $seq_obj = $_[3]->get_Seq_by_id($id); # get a PrimarySeq obj
                  if ($seq_obj) {
                        my $header = $_[3]->header($id); # get the header, or description line
                        $header =~ m/chrom:([\w\.]+):(\d+)..(\d+)/ig;
                        my $chrom = $1;
                        my $location_offset = $2;
                        my $location_end = $3;
                        if ( !exists $_[1]->{$chrom} ) {
                              $_[1]->{$chrom} = build_tree( "/var/www/E-CRISP/DATABASEFILES/" . $_[2]->{"ref_organism"} . "/" . $chrom . "_indexed" );
                        }
                        print $tempfile ">", $seq_obj->display_id(), " ", $header, "\n", $seq_obj->seq(), "\n";
                  } else {
                        print_error_html( $_[4], "No Database entry found for \\\"".substr($id,0,10)."\\\" in the \\\" ".$_[2]->{"ref_organism"}."\\\" genome.<br> Please enter a valid ensembl ID or gene symbol (case sensitive) or change the input option above to FASTA sequence.<br>" );
                        die;
                  }
            }
      close $tempfile;
}


#########################################################################################
#name:      make_pos_index
#function:  return an index of occurrences of a certain character in any given string
#input:     (string reference,character)
#output:    hash with character positions as keys
#########################################################################################
sub make_pos_index {
      my %pos     = ();
      my $result  = index( ${$_[0]}, $_[1], 0);
      while ( $result != -1 ) {
            $pos{$result}++;
            $result = index( ${$_[0]}, $_[1], ($result + 1) );
      }
      return %pos;
}

#########################################################################################
#name:      find_base_count
#function:  return an array containing the percentage each character in the string
#input:     (string)
#output:    array of percentages
#########################################################################################
sub find_base_count {
      my $seq  = $_[0];
      return      int( ($seq =~ tr/A/x/) * 100 / length($seq) ),
                  int( ($seq =~ tr/C/x/) * 100 / length($seq) ),
                  int( ($seq =~ tr/T/x/) * 100 / length($seq) ),
                  int( ($seq =~ tr/G/x/) * 100 / length($seq) );
}

#########################################################################################
#name:      reverse_comp
#function:  return the reverse complement of a DNA sequence
#input:     (string)
#output:    reverse complement string
#########################################################################################
sub reverse_comp {
      ( my $rev = reverse $_[0] ) =~ tr/ACGTacgt/TGCAtgca/;
      return $rev;
}

#########################################################################################
#name:      mean
#function:  return the mean of an array of number
#input:     (string)
#output:    mean as string
#########################################################################################
sub mean {
      return sum(@_) / @_;
}

#########################################################################################
#name:      variance
#function:  return the variance of an array of number
#input:     (string)
#output:    variance as string
#########################################################################################
sub variance {
      return ( sum( map { ( $_ - mean( @{ $_[0] } ) )**2 } @{ $_[0] } ) / @{ $_[0] } );
}

#########################################################################################
#name:      gene_label
#function:  return the last value in the features  primary tag (Bio::SeqFeature)
#input:     (Bio::SeqFeature::Generic->new)
#output:    label as string
#########################################################################################
sub gene_label {
      my $feature=shift;
      if ($feature->can("primary_tag") ) {
            my $notes=$feature->primary_tag;
            my @notes=split("::",$notes);
            $notes=pop(@notes);
            $notes;
      }else{
            my $notes="";
            $notes;
      }
}

#########################################################################################
#name:      make_my_id
#function:  return searchable ids for generation of the FASTA index
#input:     (FASTA header)
#output:    array of searchable ids (strings)
#########################################################################################
sub make_my_id {
      $_[0] =~m/^>(\S+) locus_tag= (\S+);/;
      return ( $1, $2);
}

#########################################################################################
#name:      build_tree
#function:  if a pre.build tree exists, load this tree into memory if not build it 
#           from source and save to it to file and return the tree anyways
#input:     (path to data without file ending)
#output:    augmented black/red tree (Set::IntervalTree->new)
#########################################################################################
sub build_tree {
      my  $trees = Set::IntervalTree->new;
      if (-e $_[0].".otree") {
            $trees->LoadTree($_[0].".otree");
      }elsif(-e $_[0].".mygff")  {
            open (my $infile, "<", $_[0].".mygff");
                  foreach my $line (<$infile>) {
                        chomp $line;
                        my @line=split("\t",$line);
                        my $object= $line[0] . "_" . $line[1] . "_" . $line[2];
                        $trees->insert(  $object, $line[1], $line[2] );
                  }
            close($infile);
            $trees->SaveTree($_[0].".otree");
      }
      return $trees
}


#########################################################################################
#name:      print_error_html
#function:  print any error messages as a html file for convenience
#input:     (/path/to/workdir as string, message as string) 
#output:    readable html file displaying the error message
#########################################################################################
sub print_error_html {
      my $temp_dir      = $_[0];
      my $message       = $_[1];
      open (my $errorfile, ">", $temp_dir . "/error.html");
            open(my $header, "<", "/var/www/E-CRISP/workdir/header_CRISPR.txt");
                  while(<$header>){
                        print $errorfile $_;
                  }
            close $header;
            print $errorfile '<tr><td align="left" ><span class="errormessage"><br><br><br><big>'. $message . '</big></span></td></tr>';
            open(my $footer, "<", "/var/www/E-CRISP/workdir/footer.txt");
                  while(<$footer>){
                        print $errorfile $_;
                  }
            close $footer;
      close $errorfile;
      open(my $log,">>", "/var/log/talecrisp.log");
            print $log $temp_dir."\tE-CRISP\tERROR\t$message\n";
      close($log);
      chmod 0755, $temp_dir . "/error.html";
}

#########################################################################################
#name:      round_digits
#function:  cutoff after certain number of digits
#input:     (float number as string, digits after komma <int>) 
#output:    shortened float
#########################################################################################
sub round_digits{
      if($_[0]=~m/(\d+\.\d{$_[1]}).*/){
            return $1;
      }
      return $_[0];
}

#########################################################################################
#name:      make_mismatch_string
#function:  convert a SAM file format mismatch string into an "Mismatch string" 
#           M for every match X for every mismatch D for every deletion I for every 
#           insertion
#input:     (Sequence as string-reference, $something{"non-seed-length"},
#           direction) 
#output:    shortened float
#########################################################################################
sub make_mismatch_string{
      my $mismatchstring      = "";
      my $pos                 = 0;
      my @stringarray=split("\t",${$_[0]});
      if(${$_[0]}=~m/MD:Z:(\S+)\s/){
            $mismatchstring=$1;
      }
      my @matchstring=split("",$stringarray[9]);
      my @matches=$stringarray[5]=~m/[0-9]+[MID]/g;
      foreach my $match (@matches){
            $match=~/([0-9]+)([MID])/;
            foreach (1..$1){
                  $matchstring[$pos]=$2;
                  $pos++;
            }
      }
      @matches    = $mismatchstring =~m/[0-9]+|[\^A-Z]+|[0-9]+$/g;
      $pos        = 0;
      foreach my $match (@matches){
            if($match=~/([0-9]+)/){
                  $pos += $1;
            }elsif($match=~/^[A-Z]$/){
                  $matchstring[$pos]="X";
                  $pos++;
            }else{
                  $pos++
            }
      }
      if ($_[2] eq "fw") {
            foreach (1..$_[1]){
                  unshift @matchstring , "n";
            }
      }
      else {
            foreach (1..$_[1]){
                  push @matchstring , "n";
            }
      }
      
      
      return(@matchstring);
}

#########################################################################################
#name:      print_offtarget_string
#function:  convert a "Mismatch string" into html output with different color-highlighting
#           for M,X,D and I
#input:     (Mismatch string) 
#output:    converted string for html output
#########################################################################################
sub print_offtarget_string {
      my $string=$_[0];
      $string=~s/n/<span style="color: black;">n<\/span>/g;
      $string=~s/M/<span style="color: lightgreen;">M<\/span>/g;
      $string=~s/X/<span style="color: red;">X<\/span>/g;
      $string=~s/D/<span style="color: pink;">D<\/span>/g;
      $string=~s/I/<span style="color: pink;">I<\/span>/g;
      return $string;
}
#########################################################################################
#name:      score_micro_homolgy
#function:  calculate an microhomology score between 0 and 12 for a qiven position in a 
# indexed sequence
#input:     (indices hash of hashes, threshold int,length int, sequence ref to string ) 
#output:    micro homolgy score (int)
#########################################################################################
sub score_micro_homolgy {
	my $score=0;
	my $count=0;
	my $lengthhom=1;
	my $seq;
	#my $position=$_[2];
	#my $threshold=$_[1];
	#my $length=$_[3];
	#my $postitions=%{$_[0]};
	my $stuff=0;
	my $outframe=1;
	my $inframe=1;
	my $limit_right=$_[2]+$_[1];
	if(($_[2]+$_[1]+$_[3])>length(${$_[4]})){
	    $limit_right=length(${$_[4]})-$_[3];
	}
	RIGTHSTART:foreach my $rightarmstart ($_[2]..($limit_right)){
		LENGTH:foreach my $length (2..$_[3]){
			if($lengthhom>0){
				$lengthhom=0;
				my @right_seq=split("",substr(${$_[4]},$rightarmstart,$length));
				$stuff=$length+1;
				LEFTARM: while($stuff<$_[1]){
					$stuff++;
					$count=0;
					$seq="";
					foreach my $letter (@right_seq){
						if(exists(${${$_[0]}{$letter}}{($_[2]-$stuff+$count)})){
							$seq.=$letter;
							$count++;
						}else{
							next LEFTARM;
						}
					}
					if($count==scalar(@right_seq)){
						$lengthhom=1;
						my $gaplength=($stuff+($rightarmstart-$_[2]));
						if($gaplength%3 == 0){
							$inframe=$inframe+($count*exp(0.1*(-$gaplength)));
						}else{
							$outframe=$outframe+($count*exp(0.1*(-$gaplength)));
						}
						#print "(@right_seq): $seq ".scalar(@right_seq)." ".$count." $stuff $position $rightarmstart hit"."\n";
						#print substr($string,($position-20),40)."\n";
						#print substr($string,($position-20),(20-$stuff+$count));
						#print '-' x ($stuff+($rightarmstart-$position-$count)); 
						#print substr($string,($rightarmstart),20-($rightarmstart-$position))."\n";
					}else{
						next LEFTARM;
					}
				}
			}else{
				$lengthhom=1;
				next RIGTHSTART;
			}
		}
	}
	return (10*log($outframe/$inframe));
}
sub hor_bar_chart {
	my $outstring='
	 <table>
      <tbody>
        <tr>
          <td style="font-size: 0.6em;">S</td><td class="bar"><div style="width: '.$_[0].'px;" class="item1"></div></td>
           </tr><tr>
          <td style="font-size: 0.6em;">A</td><td class="bar"><div style="width: '.$_[1].'px;" class="item2"></div></td>
           </tr><tr>
          <td style="font-size: 0.6em;">E</td><td class="bar"><div style="width: '.$_[2].'px;" class="item3"></div></td>
        </tr>
      </tbody>
    </table> ';
    return($outstring);
}
#########################################################################################
#name:      create_popup
#function:  creates popup with. the target-, match- and querry-string adjusted to each
#input:     (line of the outfile.tab as string, databasepath as string,
#           $something{"non-seed-length"}, int overflow before and after Sequence)
#output:    HTML formated string
#########################################################################################
sub create_popup {
      my @line          = @{$_[0]};
      my $report        = "";
      my $target        = "";
      my $match         = $line[8];
      my $querry        = "";
      my $adjust        = 0;
      my $adjustleft    = 0;
      my $start         = 0;
      my $end           = 0;
      my $insert        = 0;
      
      if ($line[10] eq "rc") { #turn and translate the match and querry string if direction is reverse complementary
            $querry = reverse_comp($line[9]);
            $querry =~s/\s+//ig;
            $querry =~s/^(\w{2})\w(\w+)/$1N$2/ig;
      }
      else {
            $querry =$line[9];
            $querry =~s/\s+//ig;
             $querry =~s/(\w+)\w(\w{2})$/$1N$2/ig;
            $adjust = $_[2];
      }
      
      #search the target-string in database - substr workaround used, because the Bioperl function does not work proper with large numbers
      $db         = Bio::DB::Fasta->new( $_[1] . '.dna.toplevel.fa', -makeid => \&make_my_id );
       $obj        = $db->get_Seq_by_id($line[1]);
      
      $start   = $line[3]-$adjust-$_[3]-1;
      if ($start < 0 ) {
            $adjustleft = abs($start);
            $start = 0;
      }
      
      $end     = ($line[3]-$adjust+$_[3]);
      $target  = substr $obj->seq, $start, $end - $start - 1;
      #count insertions and adjust the end property to display
      #$insert  = $line[2] =~ tr/I/x/;
      
      $report .= '<table>';
      $report .= '<tr><td>Target:</td><td>|'.$start.'*|</td>'.create_popup_string($target, $_[3], 0, 0, $match, 0, 0, $adjustleft).'<td>|'.($end-$insert).'*|</td></tr>';
      $report .= '<tr><td>Matchstring:</td><td></td>'.create_popup_string($match, $_[3], 1, 1, $match, 0, 0, $adjustleft).'</tr>';
      $report .= '<tr><td>Query:</td><td></td>'.create_popup_string($querry, $_[3], 1, 0, $match, 0, 0, $adjustleft).'</tr>';
      $report .= '</table>';
      $report .= '<hr /><p style="font-size:50%">*Start and End-point in the target</p>';
      
      return $report;
}

#########################################################################################
#name:      create_popupD
#function:  for double sequence - does the same as create_popup
#input:     (line of the outfile.tab as string, databasepath as string,
#           $something{"non-seed-length"}, int overflow before and after Sequence)
#output:    HTML formated string
#########################################################################################
sub create_popupD {
      my @line          = @{$_[0]};
      my $report        = "";
      my $target        = "";
      my @match         = split("-", $line[19]);
      my $tmp           = $line[5];
      $tmp              =~s/\s+//ig;
      my @querry        = split("_", $tmp );
      my $leadingbases  = $_[2];
      my $adjustleft    = 0;
      my $start         = 0;
      my $end           = 0;
      my $insert0       = 0;
      my $insert1       = 0;
      my $deletion0     = 0;
      my $deletion1     = 0;
      my $spacer        = "";
      my $match0        = "";
      my $match1        = "";
      
      #adjust the first/second querry-string 
      if ($line[22] eq "rc") {
            $querry[0]   = reverse_comp($querry[0]);
            $match0 = $match[0];
            $match1 = $match[1];
            #count insertions and deletions to adjust the end/start property
            $deletion0   = $match0 =~ tr/D/o/;
            $deletion1   = $match1 =~ tr/D/o/;
            #calc start and end of the target sequence
            $start       = $line[17] - $_[3] - 1 - $deletion1; # start of the match - the wanted overflow on the left side
            $end         = $line[18] + $line[23] + length($match[0]) + $_[3] - ($leadingbases*2) - ($deletion0*2); # end of the match + spacer size + length of the matchstring + the wanted overflow on the right side - the count of unspec leadingbases*2 (1st and 2nd)
      }
      else {
            $querry[1]   = reverse_comp($querry[1]);
            #swap both query and matchstring and change the start and end points ($line[15] is no longer the start of the target cause the first query swaped)
            @querry[0,1] = @querry[1,0];
            @match[0,1]  = @match[1,0];
            $match0 = $match[0];
            $match1 = $match[1];
            #count deletions to adjust the end property
            $deletion0   = $match0 =~ tr/D/o/;
            $deletion1   = $match1 =~ tr/D/o/;
            #calc start and end of the target sequence
            $start       = $line[17] - $line[23] - length($match[0]) - $_[3] + $leadingbases - 1 - $deletion1; # swaped of the if-clause and reverted (-/+), but only the leadingbases of the 2nd string count here
            $end         = $line[18] + $_[3] - $leadingbases - ($deletion0*2); # swaped of the if-clause and reverted (-/+), but only the leadingbases of the 1st string count here
      }
      
      if ($start < 0 ) {
            $adjustleft = abs($start);
            $start = 0;
      }
      
      #count insertions and deletions to adjust the end/start property
      $insert0   = $match0 =~ tr/I/o/;
      $insert1   = $match1 =~ tr/I/o/;
      for (my $i = 0; $i < ($line[23] - $leadingbases*2 - $deletion0 + $deletion1); $i++){
            $spacer .= " ";
      }
      
      #search the target-string in database - substr workaround used, because the Bioperl function does not work proper with large numbers
      my $db             = Bio::DB::Fasta->new( $_[1] . '.all.dna.fa', -makeid => \&make_my_id );
      my @target_name    = split("::", $line[16]);
      my $obj            = $db->get_Seq_by_id($target_name[0]);
      if (!defined $obj) { #have to search in whole chromosom
            $db          = Bio::DB::Fasta->new( $_[1] . '.dna.toplevel.fa', -makeid => \&make_my_id );
            $obj         = $db->get_Seq_by_id($line[16]);
      }
      
      $target  = substr $obj->seq, $start, $end - $start - 1;
      
      #build the table for the popup
      $report .= '<table style="font-size:75%">';
      $report .= '<tr><td>Query:</td><td></td>'.create_popup_string($querry[0], $_[3], 1, 0, $match[0], 0, 0, $adjustleft).'</tr>';
      $report .= '<tr><td>Matchstring:</td><td></td>'.create_popup_string($match[0], $_[3], 1, 1, $match[0], 0, 0, $adjustleft).'</tr>';
      $report .= '<tr><td>Target:</td><td>|'.$start.'*|</td>'.create_popup_string($target, $_[3], 0, 0, $match[0].$spacer.$match[1], 0, 1, $adjustleft).'<td>|'.($end - 1 - $insert1).'*|</td></tr>';
      $report .= '<tr><td>Matchstring:</td><td></td>'.create_popup_string($match[1], $_[3], 1, 1, $match[1], (length($match[0]) + int($line[23]) + $insert0 - ($leadingbases*2) - $deletion0 + $deletion1), 0, $adjustleft).'</tr>';
      $report .= '<tr><td>Query:</td><td></td>'.create_popup_string($querry[1], $_[3], 1, 0, $match[1], (length($match[0]) + int($line[23]) + $insert0 - ($leadingbases*2) - $deletion0 + $deletion1), 0, $adjustleft).'</tr>';
      $report .= '</table>';
      $report .= '<hr /><p style="font-size:50%">*start- and endpoint in the target sequence</p>';
      
      return $report;
}

#########################################################################################
#name:      create_popup_string
#function:  adjusts a sequence for the popup-output
#input:     (string sequence, int overflow from create_popup, boolean if it should be used,
#           boolean if colored or not, string matchstring, 2nd overflow for double seq,
#           boolean if the last letter should  only be popped for 2nd half - 1 = yes,
#           int additional left re-adjustment)
#output:    HTML formated string
#########################################################################################
sub create_popup_string {
      my $string  = "";
      my @letters = split("",$_[0]);
      my @match   = split("",$_[4]);
      my $indel   = 0;
      my $letter  = "";
      my $length  = length($_[4]);
      
      if ($_[2] == 1) {
            for (my $i = 0; $i < $_[1] + $_[5] - $_[7]; $i++){
                  $string .= '<td></td>';
            }
      }
       
      
      for(my $i = 0; $i < scalar(@letters); $i++){
            if ($_[3] == 1) { # matchstring needs color
                  $string .= '<td align="center" valign="middle">'.print_offtarget_string($letters[$i]).'</td>';
            }
            else{
                  #adjust the letters for indel
                  $letter = $letters[$i];
                  if ($_[2] == 1 && $indel < $length) { # for querystring (same size as matchstring)
                        if ($match[$indel] eq "D") {
                              $letter = "_";
                              $i--;
                        }
                  }
                  elsif ($indel - $_[1] >= 0 && ($indel - $_[1]) < $length) { # for targetstring (needs adjustment)
                        if ($match[$indel - $_[1]] eq "I") {
                              $letter = "_";
                              $i--;
                              if ($_[6] == 0) {
                                    pop (@letters); #delete last Element
                              }
                              
                              elsif ($indel - $_[1] > $length/2) {
                                    pop (@letters); #delete last Element only for I's in the 2nd matchstring
                              }
                        }
                  }
                  
                  $string .= '<td align="center" valign="middle">'.$letter.'</td>';
                  $indel++;
            }
      }
      
      return $string;
}
#########################################################################################
#name:      calc_doench_score
#function:  Calculate CRISPR Score after Doench et al. 2014 Rational design of highly active sgRNAs for CRISPR-Cas9–mediated gene inactivation
#input:     < string > #lengt 30 mandatory
#output:    <numeric double>
#########################################################################################
sub calc_doench_score{
    my $score;
    if (length($_[0])==30) {
    my %sing_nuc_hash = ('G2'=>-0.275377128,'A3'=>-0.323887456,'C3'=>0.172128871,'C4'=>-0.100666209,'C5'=>-0.20180294, 
                    'G5'=>0.245956633,'A6'=>0.036440041,'C6'=>0.098376835,'C7'=>-0.741181291,
                    'G7'=>-0.393264397,'A12'=>-0.466099015,'A15'=>0.085376945,'C15'=>-0.013813972,
                    'A16'=>0.272620512,'C16'=>-0.119022648,'T16'=>-0.285944222,'A17'=>0.097454592,
                    'G17'=>-0.17554617,'C18'=>-0.345795451,'G18'=>-0.678096426,'A19'=>0.22508903,
                    'C19'=>-0.507794051,'G20'=>-0.417373597,'T20'=>-0.054306959,'G21'=>0.379899366,
                    'T21'=>-0.090712644,'C22'=>0.057823319,'T22'=>-0.530567296,'T23'=>-0.877007428,
                    'C24'=>-0.876235846,'G24'=>0.278916259,'T24'=>-0.403102218,'A25'=>-0.077300704,
                    'C25'=>0.287935617,'T25'=>-0.221637217,'G28'=>-0.689016682,'T28'=>0.117877577,
                    'C29'=>-0.160445304,'G30'=>0.386342585);
    my %dinuc_hash = ('GT2'=>-0.625778696,'GC5'=>0.300043317,'AA6'=>-0.834836245,'TA6'=>0.760627772,'GG7'=>-0.490816749,
                      'GG12'=>-1.516907439,'TA12'=>0.7092612,'TC12'=>0.496298609,'TT12'=>-0.586873894,'GG13'=>-0.334563735,
                      'GA14'=>0.76384993,'GC14'=>-0.53702517,'TG17'=>-0.798146133,'GG19'=>-0.66680873,'TC19'=>0.353183252,
                      'CC20'=>0.748072092,'TG20'=>-0.367266772,'AC21'=>0.568209132,'CG21'=>0.329072074,'GA21'=>-0.836456755,
                      'GG21'=>-0.782207584,'TC22'=>-1.029692957,'CG23'=>0.856197823,'CT23'=>-0.463207679,'AA24'=>-0.579492389,
                      'AG24'=>0.649075537,'AG25'=>-0.077300704,'CG25'=>0.287935617,'TG25'=>-0.221637217,'GT27'=>0.117877577,
                      'GG29'=>-0.697740024);
    my $gc = ( substr($_[0],4,20) =~ tr/GC/GC/);
    if ($gc < 10){
        $score=0.597636154+(abs($gc-10)*-0.202625894)
    }else{
        $score=0.597636154+(($gc-10)*-0.166587752)
    }        
    foreach my $i (0..29){        
       my $key = substr($_[0],$i,1).($i+1);
       if ($sing_nuc_hash{$key}) {
        $score+=$sing_nuc_hash{$key};
       }
       if($i<29){
        $key =substr($_[0],$i,2).($i+1);
        if ($dinuc_hash{$key}){
                $score+=$dinuc_hash{$key};
        }
       }
    }
    return(1/(1+exp(-$score)))
      #code
    }else{
        return(0);
    }
}
#########################################################################################
#name:      calc_XU_score
#function:  Calculate CRISPR Score after XU et al. 2015 Sequence determinants of improved CRISPR sgRNA design
#input:     < string > #lengt 30 mandatory 20 Protspacer followed by 10 including NGG PAM
#output:    <numeric double>
#########################################################################################
sub calc_XU_score{
    my $score=0;  
    if (length($_[0])==30) {
        my %scoring_matrix;
        @{$scoring_matrix{'A'}}=(0,0,0,0,0.025840846,0,0,0,0.02156311,0.129118902,0.030483786,0.093646913,0,0,0.202820147,0.129158071,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
        @{$scoring_matrix{'C'}}=(0,0,-0.113781378,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.23502822,0,-0.125927965,0,0,0,0.179639101,0,0,0,0,0,0);
        @{$scoring_matrix{'G'}}=(0,0,0,0.080289971,0.072680697,0.100642827,0.082839514,0,0,0,0,0,0,-0.214271553,0,0,0.107523301,0,0.238517854,0.353047311,0,0,0,0,0,0,0,0,0,0);
        @{$scoring_matrix{'T'}}=(0,0,0,0,0,0,-0.070933894,0,0,0,-0.169986128,0,0,0.073750154,0,0,-0.349240474,-0.145493093,-0.300975354,-0.221752041,-0.155910373,0,0,0,0,0,-0.116646129,0,0,0);
        my $pos=0;
        while ( $_[0]=~m/(\w)/g) {
            $score+=@{$scoring_matrix{$1}}[$pos];
            $pos++;
        }
        return(($score-(-0.5))/(2.5));
    }else{
        return(0);
    }
}
#########################################################################################
#name:      from_pam_to_fasta_combis
#function:  find every possble ACGT sequence out of any given IUPAC sequence of any given length
#input:     string
#output:    string
#########################################################################################
sub from_pam_to_fasta_combis{
        my %translator;
        @{$translator{"U"}}="T";
        @{$translator{"A"}}="A";
        @{$translator{"G"}}="G";
        @{$translator{"T"}}="T";
        @{$translator{"C"}}="C";
        @{$translator{"N"}}=("A","C","G","T");
        @{$translator{"K"}}=("G","T");
        @{$translator{"M"}}=("A","C");
        @{$translator{"S"}}=("C","G");
        @{$translator{"W"}}=("A","T");
        @{$translator{"R"}}=("A","G");
        @{$translator{"Y"}}=("C","T");
        @{$translator{"B"}}=("G","C","T");
        @{$translator{"D"}}=("G","A","T");
        @{$translator{"H"}}=("C","A","T");
        @{$translator{"V"}}=("A","C","G");        
        my @old_words=($_[0]);
        my @words=();
        my @word_split=();
        my @tmp=();
        my $pos=0;
        while ($pos<length($_[0])) {
            @words=();
            foreach my $word (@old_words){
                @word_split=split("",$word);
                @tmp=();
                foreach my $translate (@{$translator{$word_split[$pos]}}){
                    @tmp=@word_split;
                    $tmp[$pos]=$translate;
                    push @words , join("",@tmp);
                }
            }
            @old_words=@words;
            $pos++;
        }
        return(@old_words);        
}
#########################################################################################
#name:      calculate_CRISPR_score
#function:  helper-function for find_and_print_CRISPRS
#           creates the score for the given CRISPRS
#input:     (builded tree as referemce, something-Hashreference, start, end, chrom
#           1/0 for $score{"CDS"}++)
#output:    the calculated score as hash
#########################################################################################
sub calculate_CRISPR_score {
      my %score = ();
      my @new_score=@{$_[6]};  
      my $expression="[";
      if (($_[1]->{"number_of_CDS"}>0)) {
         foreach my $number(1..$_[1]->{"number_of_CDS"}){
            $expression.=$number;
            }
         $expression.="]";
      }else{
             $expression="[.]";
      }
      $score{"CRISPRi"}=0;
      $score{"CRISPRa"}=0;
      my %transcripts=();
      if ( exists $_[0]->{$_[4]} ) { # check wethere the tree exists
            #search for annotations in the intervall from start (2) to end (3) and count them in score
            my $annotations = $_[0]->{$_[4]}->fetch( int($_[2]), int($_[3]) );
            
            foreach  my $anno ( @{$annotations} ) {
                  if ( $anno =~ m/gene_(\S+)_(\d+)_(\d+)/ ) {
                       my $gene_annotations = $_[0]->{$_[4]}->fetch( int($2), int($3) );                        
                        foreach  my $gene_anno ( @{$gene_annotations} ) {
                              if ($gene_anno=~m/exon::(\S+)::(\d+)::(\S+)\_(\d+)_(\d+)/) {    
                                    ${$transcripts{$1}}{"exon".$2}=$4."_".$5;
                              }
                        }
                        last;
                  }
            }
            foreach  my $anno ( @{$annotations} ) {
                  if ( $anno =~ m/gene_(\S+)_(\d+)_(\d+)/ ) {
                        $new_score[1]++;
                        ${ $score{"gene"} }{$1}++;                      
                  } elsif ( $anno =~ m/exon::(\S+)::(\d+)::(\S+)\_(\d+)_(\d+)/) {
                        ${ $score{"exon"} }{$2}++;
                        $new_score[1]=$new_score[1]+5/$2;
						 $new_score[4]=$new_score[4]+5/$2;
                        if (exists $score{"transcripts"}) {
                              $score{"transcripts"}=$score{"transcripts"}."_".$1;
                        }else{
                              $score{"transcripts"}="_".$1;
                        }
                        ${$score{"transexons"}}{$1."::".$2}++;
                        
                  } elsif ( $anno =~ m/CpG/ ) {
                        $score{"CpG"}++;
						if($new_score[1]-1>=0){
							$new_score[1]--;
						}                        
                  } elsif ( $anno =~ m/CDS::(\S+)::(\d+)::(\S+)\_(\d+)_(\d+)/ ) {
                        $new_score[1]=$new_score[1]+5/$2;
						$new_score[3]=$new_score[3]+5/$2;
                  } elsif ( $anno =~ m/CDS/ ) {
                        $score{"CDS"}++;
                        $new_score[1]++;
                  }
            }
            my $strand=1;
            my $first_start=0;
            my $first_end=0;
            my $second_start=0;
            foreach my $trans (keys %transcripts){
                  my %temp=%{$transcripts{$trans}};
                  my @first_temp=split("_",$temp{"exon1"});
                  $first_start=$first_temp[0];
                  $first_end=$first_temp[1];
                  my @second_temp=split("_",$temp{"exon2"});
                  $second_start=$second_temp[0];
                  if ($second_start<$first_start) {
                       $strand=0;
                  }                  
                  if (
                        $strand==1
                        && ($first_start+300)>=int($_[2])
                        && ($first_start-50)<=int($_[2])
                        ) {
                          $score{"CRISPRi"}=1;
                     }elsif(
                           $strand==0
                         && ($first_end-300)<=int($_[2])
                         && ($first_end+50)>=int($_[2])
                     ){
                           $score{"CRISPRi"}=1;
                     }elsif(
                           $strand==1
                         && ($first_start-400)<=int($_[2])
                         && ($first_start-50)>=int($_[2])
                     ){
                           $score{"CRISPRa"}=1;
                     }elsif(
                           $strand==0
                         && ($first_end+400)>=int($_[2])
                         && ($first_end+50)<=int($_[2])
                     ){
                           $score{"CRISPRa"}=1;
                     }
            }
            
            #search for the start and stop coddon in the intervall from start (2) to end (3) with an up/downstream window
            $annotations = $_[0]->{$_[4]}->fetch( int( $_[2] - $_[1]->{"downstream_window"}), int( $_[3] + $_[1]->{"upstream_window"}) );
            foreach  my $anno ( @{$annotations} ) {
                  if ( $anno =~ m/start_codon/ ) {
                        $score{"start_codon"}++;
                        $new_score[1]++;
                  }elsif ( $anno =~ m/stop_codon/ ) {
                        $score{"stop_codon"}++;
                        $new_score[1]++;
                  }
            }
      }
      $score{"new_score"}=\@new_score;
      return %score;
}
