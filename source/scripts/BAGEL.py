#!/usr/bin/python 

VERSION = 0.91

#---------------------------------
# BAGEL:  Bayesian Analysis of Gene EssentaLity
# (c) Traver Hart, 02/2015.
# modified 9/2015
# Free to modify and redistribute with attribtuion
#---------------------------------

from numpy import *
import scipy.stats as stats
import sys, getopt

helptext = ('\n'
		   'BAGEL.py -i [fold change file] -o [output file] -e [reference essentials] -n [reference nonessentials] -c [columns to test]\n' 
		   '\n'
		   '  from the Bayesian Analysis of Gene EssentiaLity (BAGEL) suite\n'
		   '  Version ' + str(VERSION) + '\n' 
		   '\n'
		   '  required options:\n' 
		   '     -i  [fold change file]         Tab-delmited file of reagents and fold changes.  See documentation for format.\n' 
		   '     -o  [output file]              Output filename\n' 
		   '     -e  [reference essentials]     File with list of training set of essential genes\n' 
		   '     -n  [reference nonessentials]  File with list of training set of nonessential genes\n' 
		   '     -c  [columns to test]          comma-delimited list of columns in input file to include in analyisis\n' 
		   '\n' 
		   '  other options:\n'
		   '     --numiter=N                    Number of bootstrap iterations (default 1000)\n'
		   '     -h, --help                     Show this help text\n'
		   '\n'
		   '  Example:\n' 
		   '  BAGEL.py -i foldchange_file -o experiment.bf -e essentials_training_set -n nonessentials_training_set -c 1,2,3\n'
		   '\n'
		   '  Calculates a log2 Bayes Factor for each gene; positive BFs indicate confidence that the gene is essential.\n'
		   '  writes to [output file]: gene name, mean Bayes Factor across all iterations, std deviation of BFs, and number of iterations\n'
		   '  in which the gene was part of the test set (and a BF was calculated[output file]\n' 
		   '\n')

NUM_BOOTSTRAPS = 1000

try:
	opts, args = getopt.getopt(sys.argv[1:], "hi:o:c:e:n:", ["numiter=","help"])
except getopt.GetoptError:
	print(helptext)
	sys.exit(2)
for opt, arg in opts:
	if opt in ( '-h', '--help'):
		print(helptext)
		sys.exit()
	elif opt == '-i':
		foldchangefile = arg
	elif opt == '-o':
		outfilename = arg
	elif opt == '-e':
		ess_ref = arg
	elif opt == '-n':
		non_ref = arg
	elif opt == '-c':
		columns = arg.split(',')
	elif opt == '--numiter':
		NUM_BOOTSTRAPS = int(arg)


column_list = [int(c) for c in columns]

FC_THRESH = 2**-7
genes={}
fc = {}

def round_to_hundredth(x):
	return round( x*100) / 100.0

def bootstrap_resample(X, n=None):
	""" Bootstrap resample an array_like
	Parameters
	----------
	X : array_like
	  data to resample
	n : int, optional
	  length of resampled array, equal to len(X) if n==None
	Results
	-------
	returns X_resamples

	adapted from
	Dated 7 Oct 2013
	http://nbviewer.ipython.org/gist/aflaxman/6871948
	"""
	if n == None:
			n = len(X)

	resample_i = floor(random.rand(n)*len(X)).astype(int)
	X_resample = X[resample_i]
	return X_resample


#
# LOAD FOLDCHANGES
#


fin  = open(foldchangefile)
skipfields = fin.readline().rstrip().split('\t')
for i in column_list:
	print("Using column:  " + skipfields[i+1])
for line in fin:
	fields = line.rstrip().split('\t')
	gsym = fields[1]
	genes[ gsym ]=1
	if ( not gsym in fc ):
		fc[gsym]=[]    # initialize dict entry as a list
	for i in column_list:
		fc[gsym].append( float(fields[i + 1]))		# per user docs, GENE is column 0, first data column is col 1.
genes_array = array( genes.keys() )
gene_idx = arange( len( genes ) )
#print("Number of gRNA loaded:  " + str( len(genes_array) ))
#print("genes_array:  " + str(genes_array))
#print("Genes:  " + str(genes))
#print("Bayes:	" + str(fc))
print("Number of unique genes:  " + str( len(genes) ))

#
# DEFINE REFERENCE SETS
#
coreEss = []
fin = open(ess_ref)
for line in fin:
	coreEss.append( line.rstrip().split('\t')[0] )
fin.close()
coreEss=array(coreEss)
print("Number of reference essentials: " + str(len(coreEss)))

nonEss = []
fin = open(non_ref)
for line in fin:
	nonEss.append( line.rstrip().split('\t')[0] )
fin.close()
nonEss = array(nonEss)
print("Number of reference nonessentials: " + str(len(nonEss)))

#
# INITIALIZE BFS
#
bf = {}
for g in genes_array:
	bf[g]=[]

#
# BOOTSTRAP ITERATIONS
#
print("Iter"),
print("TrainEss"),
print("TrainNon"),
print("TestSet")
sys.stdout.flush()
for loop in range(NUM_BOOTSTRAPS):
	print(str(loop)),
	#
	# bootstrap resample from gene list to get the training set
	#
	gene_train_idx = bootstrap_resample(gene_idx)
	#
	# test set for this iteration is everything not selected in bootstrap resampled training set
	#
	gene_test_idx = setxor1d(gene_idx, gene_train_idx)
	#
	# define essential and nonessential training sets:  arrays of indexes
	#
	train_ess = where( in1d( genes_array[gene_train_idx], coreEss))[0]
	train_non = where( in1d( genes_array[gene_train_idx], nonEss))[0]
	print(len(train_ess)),
	print(len(train_non)),
	print(len(gene_test_idx))
	sys.stdout.flush()
	#
	# define ess_train: vector of observed fold changes of essential genes in training set
	#
	ess_train_fc_list_of_lists = [ fc[x] for x in genes_array[gene_train_idx[train_ess]] ]
	ess_train_fc_flat_list = [obs for sublist in ess_train_fc_list_of_lists for obs in sublist]
	#
	# define non_train vector of observed fold changes of nonessential genes in training set
	#
	non_train_fc_list_of_lists = [ fc[x] for x in genes_array[gene_train_idx[train_non]] ]
	non_train_fc_flat_list = [obs for sublist in non_train_fc_list_of_lists for obs in sublist]
	#
	# calculate empirical fold change distributions for both
	#
	kess = stats.gaussian_kde( ess_train_fc_flat_list )
	knon = stats.gaussian_kde( non_train_fc_flat_list )
	#
	# define empirical upper and lower bounds within which to calculate BF = f(fold change)
	#
	x = arange(-10,2,0.01)
	nonfitx = knon.evaluate(x)
	# define lower bound empirical fold change threshold:  minimum FC where knon is above threshold
	f = where( nonfitx > FC_THRESH)
	xmin = round_to_hundredth( min(x[f]) )
	# define upper bound empirical fold change threshold:  minimum value of log2(ess/non)
	subx = arange( xmin, max(x[f]), 0.01)
	logratio_sample = log2( kess.evaluate(subx) / knon.evaluate(subx) )
	f = where( logratio_sample == logratio_sample.min() )
	xmax = round_to_hundredth( subx[f] )
	#
	# round foldchanges to nearest 0.01
	# precalculate logratios and build lookup table (for speed)
	#
	logratio_lookup = {}
	for i in arange(xmin, xmax+0.01, 0.01):
		logratio_lookup[round(i*100)] = log2( kess.evaluate(i) / knon.evaluate(i) )
	#
	# calculate BFs from lookup table for withheld test set
	#
	for g in genes_array[gene_test_idx]:
		foldchanges = array( fc[g] )
		foldchanges[foldchanges<xmin]=xmin
		foldchanges[foldchanges>xmax]=xmax
		bayes_factor = sum( [ logratio_lookup[ round( x * 100 ) ] for x in foldchanges ] )
		bf[g].append(bayes_factor)


fout = open(outfilename, 'w')
fout.write('GENE\tBF\tSTD\tNumObs\n')
for g in sorted( bf.keys() ):
	num_obs = len( bf[g] )
	bf_mean = mean( bf[g] )
	bf_std  = std( bf[g] )
	bf_norm = ( bf[g] - bf_mean ) / bf_std
	#dstat, pval = stats.kstest( bf_norm, 'norm')
	fout.write('{0:s}\t{1:4.3f}\t{2:4.3f}\t{3:d}\n'.format( g, bf_mean, bf_std, num_obs ) )
fout.close()