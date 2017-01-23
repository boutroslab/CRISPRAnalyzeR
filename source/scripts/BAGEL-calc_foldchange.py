#!/usr/bin/python 

VERSION = 0.1

#---------------------------------
# BAGEL-calc_foldchange:  given a matrix of read counts, normalize, filter for low reads, and calculate fold change
# (c) Traver Hart, 10/2015.
# modified 9/2015
# Free to modify and redistribute with attribtution
#---------------------------------

helptext = ('\n'
		   'BAGEL-calc_foldchange.py -i [read count file] -o [output label] -c [control column]\n' 
		   '\n'
           '  from the Bayesian Analysis of Gene EssentiaLity (BAGEL) suite\n'
           '  Version ' + str(VERSION) + '\n' 
           '\n'
           '  required options:\n' 
           '     -i  [read count file]          Tab-delmited file of reagents and fold changes.  See documentation for format.\n' 
           '     -o  [output label]             Label for all output files\n' 
           '     -c  [control column]           Control (T0 or plasmid) column\n'
           '\n' 
           '  other options:\n'
           '     --minreads=N                   Discard gRNA with T0 counts < N (default 30)\n'
           '     -h, --help                     Show this help text\n'
           '\n'
           '  Example:\n' 
           '  BAGEL-calc_foldchange.py -i readcount_file -o experiment_name -c 1\n' 
           '\n'
           '  Filters readcount_file for reagents with at least 30 reads in the control sample,\n'
           '  calculates fold change, and writes [output label].foldchange and [output label].normalized_reads\n'
           '\n')

from numpy import *					# into current namespace
import sys, getopt
import pandas as pd


#-------------------------------------------#
#   SET CONTSTANTS; INITIALIZE VARIABLES    #
#-------------------------------------------#

MIN_READS = 30

#----------------------------------#
#   READ COMMAND LINE ARGUMENTS    #
#----------------------------------#

try:
	opts, args = getopt.getopt(sys.argv[1:], "hi:o:c:", ["minreads=","help"])
except getopt.GetoptError:
	print helptext
	sys.exit(2)
for opt, arg in opts:
	if opt in ( '-h', '--help'):
		print helptext
		sys.exit()
	elif opt == '-i':
		readcountfile = arg
	elif opt == '-o':
		label = arg
	elif opt == '-c':
		ctrl_column = int(arg)
	elif opt == '--minreads':
		MIN_READS = int(arg)

#----------------------------------------------------------------#
# Import raw read data, normalize, filter for T0 min readcounts  #
# Output:   [output label].foldchange                            #
#----------------------------------------------------------------#

reads = pd.read_table(readcountfile, sep='\t', index_col=0)

control_label = reads.columns.values[ctrl_column]
numClones, numColumns = reads.shape

#
# missing gene name = replace
# missing read count = zero count
#
reads[ reads.columns.values[1] ].fillna('NO_GENE_NAME', inplace=True)
reads.fillna(0, inplace=True)

#
# normalize each sample to a fixed total readcount
#
sumReads = reads.ix[:,range(1,numColumns)].sum(0)
normed   = pd.DataFrame( index=reads.index.values )
normed['GENE'] = reads.ix[:,0]				# first column is gene name
normed = reads.ix[:,range(1,numColumns)] / tile( sumReads, [numClones,1]) * 10000000	# normalize to 10M reads

#
# filter for minimum readcount
#
f = where( reads.ix[:,ctrl_column ] >= MIN_READS )[0]
normed = normed.ix[f,:]

#
# calculate fold change
#
foldchange = pd.DataFrame( index=normed.index.values )
foldchange.index.name = 'REAGENT_ID'
foldchange['GENE'] = reads.ix[f,0]				# dataframe 'normed' has no GENE column
for i in range( numColumns -1 ):			
	foldchange[ normed.columns.values[i] ] = log2( (normed.ix[:,normed.columns.values[i] ]  + 0.5) \
											    / ( normed.ix[:,control_label] + 0.5 ) )
#
# we have calculated a foldchange for the control column.  Drop it.
#
foldchange.drop( control_label, axis=1, inplace=True)

#
# write normed readcount file
# write foldchange file
#
foldchange_filename = label + '.foldchange'
foldchange.to_csv( foldchange_filename, sep='\t', float_format='%4.3f')

normedreads_filename = label + '.normed_readcount'
normed.to_csv( normedreads_filename, sep='\t', float_format='%3.2f')

