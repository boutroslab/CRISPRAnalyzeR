# save as 'analysis.r'
# batch script run by CRISPRAnalyzeR shiny app with 1 argument
# Rscript path/analysis.r tmp/path/analysis.info
# runs most of car' functions






##############
#### init ####
##############
# path of info file is in 1st argument
# load info file
infoFile <- commandArgs(TRUE)[1] #"./newDir/analysis.info"  
x <- scan(infoFile, what="", sep="\n")
info <- list()
xlist <- strsplit(x, split = ";", fixed = TRUE) 

for( i in 1:length(xlist) ){
  info[[xlist[[i]][1]]] <- xlist[[i]][-1]
}

logDir <- info$logDir
userID <- info$userID
userDir <- info$userDir

# get WD of app and change it to user Directory only for these calculations
wd <- getwd()
setwd(userDir)

# create log for troubleshooting
logFile <- file.path(logDir, "analysis.log")
log <- c(paste(userID, ": analysis.r starting at", Sys.time()),
         paste(userID, ": signature is", info$signature))
if( !file.exists(logFile) ){
  write(log, logFile)  
} else {
  write(log, logFile, append = TRUE)  
}

# load all external functions
write(paste(userID, ": source files"), logFile, append = TRUE)
local({
  files <- as.list(unlist(strsplit(info$extfiles, ",")))
  
  for(i in files) {
    #write(paste(userID, ": source file ", i), logFile, append = TRUE)
    source(file.path(info$funDir, i))
  }
})





##################
#### Progress ####
##################
# don't want to many progress updates because it makes dropdown menu of app reload everytime
# which means it closes if it was opened by user before

# progress updates are communicated via infoFile
# via 1st line named 'progress' (columns are ';'-sep in file)
# progress = 1 is termination signal telling app fastq-extraction.r stopped
# if 2nd line (named 'info') is empty := 0 exit status
# if 2nd line is not empty := non-0 exit status (some kind of error)
progress <- 0.01
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))




 



##################
#### Packages ####
##################
write(paste(userID, ": loading packages"), logFile, append = TRUE) 
write(paste(userID, ": loading BiocGenerics"), logFile, append = TRUE) 
library(BiocGenerics)
write(paste(userID, ": loading biomaRt"), logFile, append = TRUE) 
library(biomaRt)
write(paste(userID, ": loading seqinr"), logFile, append = TRUE) 
library(seqinr)
write(paste(userID, ": loading MESS"), logFile, append = TRUE) 
library(MESS)
write(paste(userID, ": loading DESeq2"), logFile, append = TRUE) 
library(DESeq2)
write(paste(userID, ": loading sm"), logFile, append = TRUE) 
library(sm)
write(paste(userID, ": loading reshape2"), logFile, append = TRUE) 
library(reshape2)
write(paste(userID, ": loading DEoptim"), logFile, append = TRUE) 
library(DEoptim)
write(paste(userID, ": loading ScreenBEAM"), logFile, append = TRUE) 
library(ScreenBEAM)
write(paste(userID, ": loading tidyverse"), logFile, append = TRUE) 
library(tidyverse)
write(paste(userID, ": loading caTools"), logFile, append = TRUE) 
library(caTools)
write(paste(userID, ": loading BiocParallel"), logFile, append = TRUE) 
library(BiocParallel)

write(paste(userID, ": finished loading packages"), logFile, append = TRUE) 

write(paste(userID, ": databasepath ", info$databasepath), logFile, append = TRUE) 




######################
#### Try Function ####
######################
# wrapping expressionin try, and taking care of log and info file writing if there was an error
# arguments:  expr    the expression to be evaluated
#             place   chr str specifying the info message that will be shown to user
#             logFile chr str path of logging file
#             userID  chr str of user ID
#             userDir chr str of tmp user directory
# value:  if no error occured, result of expression
#         if error occured stop with a error message
# side effects: if error occured, writes on logging and info file
tryFunction <- function( expr, place, log = logFile, ID = userID, dir = userDir ){
  res <- try(expr)
  if( class(res) == "try-error" ){
    
    write(paste(ID, ": try-error occured"), log, append = TRUE)
    write(paste(ID, ":", res[1]), log, append = TRUE)
    
    info <- switch(place,
      readcount = "We have problems reading the read count files. Please make sure they meet the formatting criteria.<br/>Please see the help for more information.<br/>",
      biomart = "Gene Identifier handling via the biomaRt service failed.<br/><br/>There might be several reasons for this, e.g. the biomaRt service might be down (in this case, please try again later).<br/>Additional, please check whether you have selected the correct <b>gene identifier ans organism in the 'Set Groups and Identifier' tab</b>.</br>
      Please make sure you select the correct identifiers.<br/><br/>If you do not want gene identifiers to be converted, please select your gene identifier also as the identifier to convert it to.<br/>",
      checkbiomart = "<strong>Unfortunately, the ENSEMBL biomaRt web-service seems to be offline. Plase make sure you have set the proxy settings correctly. </strong></br> Otherwise, please try again later.</br> <strong>You can also get the latest ENSEMBL status updates on the <a href='http://http://www.ensembl.info/' target='_blank'>Ensembl Blog</a>.</strong></br></br> You can check the proxy status on About CRISPRAnalyzeR.",
      cpnorm = "Data normalization failed.<br/>This is an indication that the majority of read counts is 0.",
      pre = "Something went wrong when pre-processing the data.<br/>",
      qc = "Quality Calculations could not be performed on your data.<br/>A reason might be that the read count is 0 for the majority of sgRNAs.<br/>",
      unmapped = "An error occured during the calculations of missing sgRNAs/Genes. Most probably this indicates sgRNA identifier or sample name are not unique.<br/>",
      edger = "EdgeR Hit calling could not be performed. Right now, a minimum of 2 replicates is necessary.<br/>",
      ha = "Something went wrong during Hit Calling calculations.<br/>",
      deseq = "DESeq2 Hit calculation could not be performed.<br/>",
      bagel = "BAGEL essential score calculation could not be performed.<br/>",
      screenbeam = "ScreenBEAM essential gene calculation could not be performed.<br/>",
      mageck = "MAGeCK Hit calculation could not be performed.<br/>",
      rsea = "sgRSEA Hit calculation could not be performed.<br/>",
      wilcox = "Wilcoxon Hit calculation could not be performed.<br/>",
      aggregate = "sgRNA readcount data could not be aggregated for gene.<br/>Please make sure you selected the correct Regular Expression (on the Setup Page) that describes your sgRNA identifier.<br/>You can also send us a ticket/post to the forum and we will help you.<br/>",
      thresholdcheck = "<strong>The median or mean of your read count data is 0 or no sgRNAs are left.</strong></br>In case you selected to remove sgRNAs with low or high read counts from your data,</br>you can try to unset this option in the 'Set Analysis Parameters' section and start the analysis again.<br/>",
      PCA = "Principal Component Analysis failed.</br></br>In case you selected to remove low or high read counts from your data,</br>you can try to unset this option in the 'Set Analysis Parameters' section and start the analysis again.<br/>"
    )
    
    outInfo <- c(paste("progress", 1, sep = ";"), paste("info", info, sep = ";"))
    write(outInfo, file.path(dir, "analysis.info"))
    
    write(paste(ID, ": analysis.r quit at", Sys.time()), log, append = TRUE)
    quit(save = "no", status = 1)
  } else return(res)
}

# check for missing data after applying the thresholds
# if now median/mean is 0, we have some issues!
check_thresholds <- function(threshold_to_check, nontargeting = NULL, positivcontrol = NULL){
  # check number of rows
  if(nrow(cp$readcount) <= 10){
    stop("Less than 10 sgRNAs left after applying the cutoff.")
  }
  
  # check read count
  check_median <- dplyr::summarise_each(cp$readcount[,cp$miaccs$file.names], dplyr::funs(median))
  check_mean <- dplyr::summarise_each(cp$readcount[,cp$miaccs$file.names], dplyr::funs(mean))
  if(check_median[1,] <= threshold_to_check || check_mean[1,] <= threshold_to_check)
  {
    stop("Mean or Median read count of your data is 0.")
  } else { return(NULL) }
  
  # check controls if required
  
  if(!is.null(nontargeting) )
  {
    # check read count
    check_median <- dplyr::filter(cp$readcount[,cp$miaccs$file.names], gene %in%  nontargeting) %>% dplyr::summarise_each( dplyr::funs(median))
    check_mean <- dplyr::filter(cp$readcount[,cp$miaccs$file.names], gene %in%  nontargeting) %>% dplyr::summarise_each( dplyr::funs(mean))
    if(check_median[1,] <= threshold_to_check || check_mean[1,] <= threshold_to_check)
    {
      stop("Mean or Median read count of your non-targeting control data is 0.")
    } else { return(NULL) }
  }
  if(!is.null(positivcontrol) )
  {
    # check read count
    check_median <- dplyr::filter(cp$readcount[,cp$miaccs$file.names], gene %in%  positivcontrol) %>% dplyr::summarise_each( dplyr::funs(median))
    check_mean <- dplyr::filter(cp$readcount[,cp$miaccs$file.names], gene %in%  positivcontrol) %>% dplyr::summarise_each( dplyr::funs(mean))
    if(check_median[1,] <= threshold_to_check || check_mean[1,] <= threshold_to_check)
    {
      stop("Mean or Median read count of your positive control data is 0.")
    } else { return(NULL) }
  }
  
}

progress <- 0.02
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))


###################
#### create cp ####
###################
# create cp environment
# create objects in cp env in a way similar to original car
# so that original car functions will work
write(paste(userID, ": creating cp"), logFile, append = TRUE) 
cp <- new.env()


groups <- list()
m <- ">>Groups start<<"
i <- which(names(info) == m) + 1
while( m != ">>Groups end<<" ){
  groups[[names(info[i])]] <- info[[i]]
  i <- i + 1
  m <- names(info[i])
}

cp$miaccs <- list(
    
    #### Proxy Server
    "car.proxy" = as.character(info$proxy),
    
    ####  Paths
    "scriptpath" = as.character(info$scriptDir),
    "datapath" = list( "seqFiles" = as.character(info$seqPaths), "libFile" = as.character(info$libPath)),

    ####  Sequencing File Names
    "files" = as.character(info$seqNames),
    "file.names" = as.character(info$seqGen_names),
    "treatmentgroup" = groups,                                      
    
    # Groups to compare
    "groups.compare" = c(info$compareGroup1, info$compareGroup2),                               
    
    # sgRNA library name
    "referencefile" = as.character(info$libName),

    ####  Regular Expressions
    "g.extractpattern" = as.character(info$g.extractpattern),
    
    ####  Annotations
    "g.convert" = TRUE,
    "a.database" = as.character(info$bmDatabase),
    "a.dataset" = as.character(info$annoDataset),
    "g.identifier" = as.character(info$annoID),
    "g.identifier.new" = as.character(info$annoIDnew), 
    "a.annotate.hits" = TRUE,
    "a.annotate" = c("ensembl_gene_id", "name_1006", "mim_gene_description", "family_description", 
                     "ensembl_peptide_id", "description"),
    
    ####  Normalization
    "normalize" = TRUE,
    "norm.function" = "deseq",
    
    #### Analysis Settings
    "controls.target" = info$comparePos,
    "controls.nontarget" = info$compareNeg,
    "control.picks" = as.numeric(info$analysisWilcoxRand),
    "sig.pval.deseq" = as.numeric(info$analysisDeseq2Pval),
    "sig.pval.mageck" = as.numeric(info$analysisMageckPval),
    "sig.pval.wilcox" = as.numeric(info$analysisWilcoxPval),
    "sig.pval.rsea" = as.numeric(info$analysisSgrseaPval),
    "sig.pval.edger" = as.numeric(info$analysisEdgerPval)
    
) # end of MIACCS list

# check whether there are Ctrls
if( is.null(cp$miaccs$controls.nontarget) || is.na(cp$miaccs$controls.nontarget) || 
    length(cp$miaccs$controls.nontarget) == 0 || cp$miaccs$controls.nontarget == "" ){
  ctrlNon <- FALSE
  cp$miaccs$controls.nontarget <- NULL
} else {
  ctrlNon <- TRUE
}
if( is.null(cp$miaccs$controls.target) || is.na(cp$miaccs$controls.target) || 
    length(cp$miaccs$controls.target) == 0 || cp$miaccs$controls.target == "" ){
  ctrlPos <- FALSE
  cp$miaccs$controls.target <- NULL
} else {
  ctrlPos <- TRUE
}

# cleanup
rm("i", "x", "xlist", "m")



progress <- 0.03
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))



###################
### Optimize FASTA
###################
if(info$optimizeFASTA == TRUE)
{
  write(paste(userID, ": Optimize sgRNA library"), logFile, append = TRUE)
  extract_geneID = info$libregex # extract gene ID in first capture group and sgRNA identifier in second capture group
  
  # we need to get the _ from the second capture
  sub(pattern = ".*\\(.+\\)\\((.{1}).+\\).*",x = extract_geneID, replacement = "\\1")
  
  
  library(ShortRead)
  library(seqinr)
  
  # Load lib
  lib <- seqinr::read.fasta(file=libpath, seqtype = "DNA", as.string = TRUE, forceDNAtolower = FALSE,set.attributes = TRUE, legacy.mode = TRUE, seqonly = FALSE, strip.desc = FALSE, bfa = FALSE, apply.mask = TRUE)
  
  # Make df
  libdf <- data.frame(
    design = seqinr::getName(object = lib),
    sgrna = toupper(unlist(seqinr::getSequence(object = lib, as.string = TRUE))),
    stringsAsFactors = FALSE
  )
  
  
  
  # extract identifiers and remove unwanted characters
  libdf$Genes <- sub(pattern = "[[:space:][:blank:]]", replacement = "", x = sub(pattern = extract_geneID, replacement = "\\1", x=libdf$design))
  libdf$sgRNAidentifier <- sub(pattern = "[[:punct:][:space:][:blank:]]", replacement = "", x = sub(pattern = pattern1, replacement = "\\2", x=libdf$design))
  
  
  
  # check for uniqueness
  
  if(any(duplicated(libdf$sgrna)))
  {
    duplicated <- libdf[duplicated(libdf$sgrna),]
    # Remove duplicated
    libdf <- libdf[!duplicated(libdf$sgrna),]
  }
  
  # rewrite sgRNA identifier to use sgrna sequence
  libdf$design <- apply(libdf,1, function(x){
    
    return(paste(as.character(x["Genes"]), "_", as.character(x["sgrna"]), sep=""))
  })
  
  libdf$sgrna <- tolower(libdf$sgrna)
  
  
  # Write back to FASTA
  oligos <- as.list(libdf$sgrna)
  names(oligos) <- libdf$design
  seqinr::write.fasta(sequences = oligos,names = names(oligos) ,file.out =  libpath)
  
}


####################
#### load Files ####
####################
# load files
# create objects in cp
# arrange groups

local({
  files <- cp$miaccs$files
  files.names <- cp$miaccs$file.names
  names(files) <- files.names
  libpath <- cp$miaccs$datapath$libFile
  cp$dataset.names <- files
  seqpaths <- as.character(cp$miaccs$datapath$seqFiles)

   
  ## sgRNA library
  write(paste(userID, ": reading sgRNA library"), logFile, append = TRUE)
  file.raw <- seqinr::read.fasta(file = libpath, seqtype = "DNA", as.string = TRUE, forceDNAtolower = FALSE,set.attributes = TRUE, legacy.mode = TRUE, seqonly = FALSE, strip.desc = FALSE, bfa = FALSE, apply.mask = TRUE)
  seqinr::getName(object = file.raw)
  #file.raw = file.raw[unique(names(file.raw))]
  #names(file.raw) <- lapply( file.raw, function(x) attr(x,"name") )
  cp$libFILE <- data.frame(
    "design" = seqinr::getName(object = file.raw),#attr(file.raw,"name"),
    "sequence" = unlist(seqinr::getSequence(object = file.raw, as.string = TRUE)),#unlist(file.raw),
    stringsAsFactors = FALSE
  )
  
  cp$libFILE$design <- unique(as.character(cp$libFILE$design))
  
  cp$readcount <- data.frame(
    "design" = as.character(unique(seqinr::getName(object = file.raw))),#as.character(attr(file.raw,"name")),
    stringsAsFactors=FALSE
  )
  
  cp$readcount$design <- as.character(cp$readcount$design)

  ## add data from sequencing Files
 write(paste(userID, ": reading sequencing files"), logFile, append = TRUE)
  files.loaded <- for( i in 1:length(seqpaths) ){
    x <- as.character(seqpaths[i])
    write(paste(userID, ": reading sequencing files - ", x), logFile, append = TRUE)
    # Use TRY to check if readcount file is correct
    fileread <- tryFunction(read.table( x, header = TRUE, sep = "\t", comment.char = "", stringsAsFactors = FALSE ), place="readcount" )
    
    # Debug
    write(paste(userID, ": reading sequencing files - Set colnames"), logFile, append = TRUE)
    colnames(fileread) = c("design", "reads")
    
    # keep only unique entries
    fileread <- fileread[!duplicated(fileread$design),]
    
    # Debug
    write(paste(userID, ": reading sequencing files - Set rownames"), logFile, append = TRUE)
    cp$readcount <- dplyr::left_join(x = cp$readcount, y = fileread, by="design")
    #cp$readcount <- merge(cp$readcount, fileread, by="design", suffixes = sample(1:2000,2), all.x = TRUE) # keep all sgRNAs from library file
    
    # if NA, set to 0
    cp$readcount[is.na(cp$readcount)] <- 0
  }
  
  # make sure its a char
  cp$readcount$design <- as.character(cp$readcount$design)
  write(paste(userID, ": set rownames"), logFile, append = TRUE)
  rownames(cp$readcount) <- as.character(cp$readcount$design)
  write(paste(userID, ": set colnames"), logFile, append = TRUE)
  colnames(cp$readcount) <- c("design", files.names)
  
  #DEBUG
  #readr::write_tsv(x=cp$readcount, path=file.path(userDir, "test.txt") )
  
  ## arrange groups
  write(paste(userID, ": arranging groups"), logFile, append = TRUE) 
  cp$groups.compare <- cp$miaccs$groups.compare 
  cp$treatmentgroup <- list()
  for( i in 1:length(cp$miaccs$treatmentgroup) ) {
    ind <- which(files.names %in% cp$miaccs$treatmentgroup[[i]])
    name <- names(cp$miaccs$treatmentgroup)[i]
    cp$treatmentgroup[[name]] <- ind
  }
  write(paste(userID, ": Groups - ", cp$groups.compare, collapse = " "), logFile, append = TRUE) 
  write(paste(userID, ": Treatmentgroups - ", cp$treatmentgroup, collapse = " "), logFile, append = TRUE) 
}) 

progress <- 0.04
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
######################
### Remove sgRNAs with low / highreadcounts (if set) from the screen
######################


if(info$removeLow)
{
 # check if removeThreshold is set
  if(as.numeric(info$removeThresholdLow) >= 0)
  {
   
      write(paste(userID, ": Removing low readcounts ", info$removeThresholdLow), logFile, append = TRUE)
      # is numeric
      # go through read count and remove every line in which a zero occurs
      # write(paste(userID, ": Filenames ", file), logFile, append = TRUE)
    
    # check where to remove
    if(info$removeGroups == "all")
    {
      cp$readcount <- cp$readcount[apply(cp$readcount[cp$miaccs$file.names],1,function(z) !any( z <= as.numeric(info$removeThresholdLow) )),] 
      
      # check
      tryFunction(check_thresholds(threshold_to_check = 0), "thresholdcheck")
      
      cp$readcount$design <- as.character(cp$readcount$design)
      
      # set back row names
      rownames(cp$readcount) <- cp$readcount$design
    } else {
      # Only remove based on files belonging to a specific group
      # get dataset names
      groupstouse <- cp$treatmentgroup[[info$removeGroups]] + 1 # add additional row for design, so we start at 2
      
      
      cp$readcount <- cp$readcount[apply(cp$readcount[groupstouse],1,function(z) !any( z <= as.numeric(info$removeThresholdLow) )),] 
      
      # check
      tryFunction(check_thresholds(threshold_to_check = 0), "thresholdcheck")
      
      cp$readcount$design <- as.character(cp$readcount$design)
      
      # set back row names
      rownames(cp$readcount) <- cp$readcount$design
    }
      
      
      # remove also from libFile for consistency
      cp$libFILE <- cp$libFILE[cp$libFILE$design %in% cp$readcount$design,]
  }
}
if(info$removeHigh)
{
  # check if removeThreshold is set
  if(as.numeric(info$removeThresholdHigh) >= 0)
  {
      write(paste(userID, ": Removing high readcounts ", info$removeThresholdHigh), logFile, append = TRUE)
      # is numeric
      # go through read count and remove every line in which a zero occurs
      # write(paste(userID, ": Filenames ", file), logFile, append = TRUE)
      
    # check where to remove
    if(info$removeGroups == "all")
    {
      cp$readcount <- cp$readcount[apply(cp$readcount[cp$miaccs$file.names],1,function(z) !any( z >= as.numeric(info$removeThresholdHigh) )),] 
      # check
      tryFunction(check_thresholds(threshold_to_check = 0), "thresholdcheck")
      
      cp$readcount$design <- as.character(cp$readcount$design)
      
      # set back row names
      rownames(cp$readcount) <- cp$readcount$design
    } else {
      # Only remove based on files belonging to a specific group
      # get dataset names
      groupstouse <- cp$treatmentgroup[[info$removeGroups]] + 1 # add additional row for design, so we start at 2
      
      cp$readcount <- cp$readcount[apply(cp$readcount[groupstouse],1,function(z) !any( z >= as.numeric(info$removeThresholdHigh) )),] 
      # check
      tryFunction(check_thresholds(threshold_to_check = 0), "thresholdcheck")
      
      cp$readcount$design <- as.character(cp$readcount$design)
      
      # set back row names
      rownames(cp$readcount) <- cp$readcount$design
    }
    

      
      # remove also from libFile for consistency
      cp$libFILE <- cp$libFILE[cp$libFILE$design %in% cp$readcount$design,]
  }
}


progress <- 0.05
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
########################
#### Pre Processing ####
########################
# some functions which write on cp and other objects

## get gene symbols
write(paste(userID, ": Convert Gene Identifier using biomaRt"), logFile, append = TRUE)
write(paste(userID, ": Gene Extraction Pattern:",  cp$miaccs$g.extractpattern), logFile, append = TRUE)
write(paste(userID, ": Old Identifier:",  cp$miaccs$g.identifier), logFile, append = TRUE)
write(paste(userID, ": New Identifier:",  cp$miaccs$g.identifier.new), logFile, append = TRUE)
write(paste(userID, ": Database:",  cp$miaccs$a.database), logFile, append = TRUE)
write(paste(userID, ": Dataset:",  info$annoDataset), logFile, append = TRUE)

if( is.null(info$proxy) || is.na(info$proxy) || length(info$proxy) == 0 ) info$proxy <- ""
options(RCurlOptions = list(proxy = info$proxy, http.version = 1))



# check for availability of biomaRt
checkbiomaRt <- tryFunction(biomaRt::listEnsembl(host="www.ensembl.org"), "checkbiomart")

tryFunction(
 get.gene.info(extractpattern = cp$miaccs$g.extractpattern,
             database = cp$miaccs$a.database,
             dataset = info$annoDataset,
             annotate = FALSE,
             convert.identifier = TRUE,
             new.identifier = cp$miaccs$g.identifier.new,
             filter = cp$miaccs$g.identifier,
             attribute = c(info$annoID)),
             "biomart")
if( ctrlNon ){
 ctrl <- cp$miaccs$controls.nontarget
 for( i in 1:length(ctrl) ){
   ctrl[i] <- cp$readcount$gene[which(gsub(cp$miaccs$g.extractpattern, "\\1", rownames(cp$readcount)) == ctrl[i])][1]
 }
 cp$miaccs$controls.nontarget <- ctrl
}
if( ctrlPos ){
 ctrl <- cp$miaccs$controls.target
 for( i in 1:length(ctrl) ){
   ctrl[i] <- cp$readcount$gene[which(gsub(cp$miaccs$g.extractpattern, "\\1", rownames(cp$readcount)) == ctrl[i])][1]
 }
 cp$miaccs$controls.target <- ctrl
}

progress <- 0.08
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))


## aggregate to genes
write(paste(userID, ": creating object aggregatetogenes"), logFile, append = TRUE)
write(paste(userID, ": ",paste(cp$readcount[1:4,"gene"], collapse = ";") ), logFile, append = TRUE)
aggregateok <- tryFunction(aggregatetogenes(agg.function = sum, 
                                            extractpattern = cp$miaccs$g.extractpattern), "aggregate")

write(paste(userID, ": Check for unique genes"), logFile, append = TRUE)
## Save unique genes
uniquegenes <- unique(cp$readcount$gene)

write(paste(userID,": ", length(uniquegenes)), logFile, append = TRUE)

# get randoms samples
#sampledraw <- sample(10:nrow(cp$readcount), 5)
#sampleaggregated <- cp$readcount[sampledraw,"gene"]
#sampledraw <- NULL

write(paste(userID, ": Check for Gene level read counts"), logFile, append = TRUE)

#write(paste(userID, ": 5 random lines of cp$aggregated.readcount:",  cp$aggregated.readcount[cp$aggregated.readcount$gene %in% sampleaggregated,]), logFile, append = TRUE)

### CHECK if readcount is not 0! In this case -> throw error message
# get colnames apart from design
sampledraw <- sample(x = c(2:length(cp$miaccs$file.names)), size = 1)

if(mean(cp$aggregated.readcount[,sampledraw]) == 0)
{
  write(paste(userID, ": Gene-leve mean read Count is 0"), log, append = TRUE)

  outInfo <- c(paste("progress", 1, sep = ";"), paste("info", "Mean gene-level read count is 0. Please check the Regular Expressions used, as an alternative you can remove low read counts from the analsys. Please try it again.", sep = ";"))
  write(outInfo, file.path(dir, "analysis.info"))
  
  write(paste(userID, ": analysis.r quit at", Sys.time()), log, append = TRUE)
  quit(save = "no", status = 1)
}

## readcount normalisieren
# schreibt cp$readcount und cp$aggregated.readcount als normalisiert
# -> cp$normalized.readcount und cp$normalized.aggregated.readcount
write(paste(userID, ": creating object cpnorm"), logFile, append = TRUE)
cpnorm <- tryFunction(car.normalize(norm.function = "deseq", extractpattern = cp$miaccs$g.extractpattern), "cpnorm")

progress <- 0.1
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))







########################
#### Screen Quality ####
########################
# functions whose output will be used for plots and tables in screen quality tab
# basic statistics, coverage, unmapped, read distributions of datasets

# Create PCA Plot for ALL samples
write(paste(userID, ": creating object PCA"), logFile, append = TRUE)

pca <- list()

# get dataset names we want to use
datasetcols <- as.numeric(unlist(cp$treatmentgroup[cp$groups.compare]))
datasetspca <- names(cp$dataset.names)[datasetcols]

# for Genes
data <- cp$normalized.aggregated.readcount[,datasetspca]
rownames(data) <- cp$normalized.aggregated.readcount$gene
pca$genes <- tryFunction(princomp(data, scores=TRUE),"PCA")


# make CDF plots
### progress
progress <- 0.12
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))

# gene reads
cdf_list_gene <- list(NA)
for(i in 1:length(cp$miaccs$file.names))
{
    #get number of gene
    n = length(cp$normalized.aggregated.readcount[,cp$miaccs$file.names[i]])
    
    # get a summary
    sample_summary <- summary(cp$normalized.aggregated.readcount[,cp$miaccs$file.names[i]])
    
    # Order data
    sample_sorted <- log2(sort(cp$normalized.aggregated.readcount[,cp$miaccs$file.names[i]]))
    # remove -inf by setting them to 0
    sample_sorted[!is.finite(sample_sorted)] <- 0
    
    # data is plotted like this:
    ##  x   sample_sorted
    ##  y   (1:n)/n
    
    # make tibble for highcharts
    cdf_list_gene[[i]] <- list(tibble::tibble(
      "x" = sample_sorted,
      "y" = (1:n)/n
    )
    )
    sample_sorted <- NULL
}

# sgrna 
cdf_list_sgrna <- list(NA)
for(i in 1:length(cp$miaccs$file.names))
{
    #get number of gene
    n = length(cp$normalized.readcount[,cp$miaccs$file.names[i]])
    
    # get a summary
    sample_summary <- summary(cp$normalized.readcount[,cp$miaccs$file.names[i]]+1)
    
    # Order data
    sample_sorted <- log2(sort(cp$normalized.readcount[,cp$miaccs$file.names[i]]+1))
    # remove -inf by setting them to 0
    sample_sorted[!is.finite(sample_sorted)] <- 0
    
    # data is plotted like this:
    ##  x   sample_sorted
    ##  y   (1:n)/n
    
    # make tibble for highcharts
    cdf_list_sgrna[[i]] <- list(tibble::tibble(
      "x" = sample_sorted,
      "y" = (1:n)/n
    )
    )

    sample_sorted <- NULL
}

CDF_list <- list(
  "gene" = cdf_list_gene,
  "sgRNA" = cdf_list_sgrna
)

#highcharter::hchart(pca$genes)

# for sgRNAs
#data <- cp$normalized.readcount[,datasetspca]
#rownames(data) <- cp$normalized.readcount$design
#pca$sgrna <- princomp(data)



# Statistics
write(paste(userID, ": creating object statsGeneral"), logFile, append = TRUE)
statsGeneral <- list()
statsGeneral[["basic"]] <- stats.data(extractpattern = cp$miaccs$g.extractpattern, type="stats", output = FALSE, write = FALSE)

### progress
progress <- 0.17
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

statsGeneral[["dataset"]] <- stats.data(extractpattern = cp$miaccs$g.extractpattern, type="dataset", 
                                  write = FALSE, output=FALSE)

### progress
progress <- 0.18
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

if( ctrlNon == TRUE ){
  statsGeneral[["ctrl.n"]] <- stats.data(controls.nontarget = cp$miaccs$controls.nontarget, 
      extractpattern = cp$miaccs$g.extractpattern, type = "controls", write = FALSE, output = FALSE)
}
if( ctrlPos == TRUE ){
  statsGeneral[["ctrl.p"]] <- stats.data(controls.nontarget = cp$miaccs$controls.target, 
      extractpattern = cp$miaccs$g.extractpattern, type = "controls", write = FALSE, output = FALSE)
}


# unmapped Genes and sgRNAs
write(paste(userID, ": creating object unmappedGenes"), logFile, append = TRUE)
unmappedGenes <- list()
unmappedGenes[["basic"]] <- tryFunction(unmapped.genes(dataframe = TRUE, extractpattern = cp$miaccs$g.extractpattern), "unmapped")

### progress
progress <- 0.19
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

if( ctrlNon == TRUE ){
  unmappedGenes[["nontarget"]] <- tryFunction(
    unmapped.genes(plotgenes = cp$miaccs$controls.nontarget, type="single", dataframe = TRUE, cp$miaccs$g.extractpattern),
  "qc")
}
if( ctrlPos == TRUE ){  
  unmappedGenes[["pos"]] <- tryFunction(
    unmapped.genes(plotgenes = cp$miaccs$controls.target, type="single", dataframe = TRUE, cp$miaccs$g.extractpattern),
  "qc")
}

### progress 
progress <- 0.2
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))

## read distros
write(paste(userID, ": creating object readDistribution"), logFile, append = TRUE)
readDistribution <- tryFunction(car.read.distribution(statistics = TRUE, dataframe = TRUE), "qc") 

write(paste(userID, ": creating object essentialDistribution"), logFile, append = TRUE)

DAISY_essentials <- readRDS(file = file.path(cp$miaccs$scriptpath, "DAISY_Essentials.rds"))
write(paste(userID, ": DAISY read"), logFile, append = TRUE)
essentialDistribution <- as.list(names(cp$miaccs$treatmentgroup))
attr(essentialDistribution, which="name") <- names(cp$miaccs$treatmentgroup)
write(paste(userID, ": go in FOR"), logFile, append = TRUE)

for(i in 1:length(essentialDistribution))
{
  files <- cp$miaccs$treatmentgroup[[attr(essentialDistribution, which="name")[i]]]
  write(paste(userID, ": files ", files), logFile, append = TRUE)
  write(paste(userID, ": Go through files ", i), logFile, append = TRUE)
  for(x in 1:length(files))
  {
    # get all info from daisy essentials
    essentials <- try(dplyr::filter(cp$normalized.readcount, gene %in% DAISY_essentials[,cp$miaccs$g.identifier.new][[1]]))
    nonessentials <- try(dplyr::filter(cp$normalized.readcount, !gene %in% DAISY_essentials[,cp$miaccs$g.identifier.new][[1]]))
    
    
    write(paste(userID, ": checked ",x), logFile, append = TRUE)
    if(x==1)
    {
      write(paste(userID, ": 1"), logFile, append = TRUE)
      ess <- try(density(log2(essentials[, files[x]])))
      if(class(ess) == "try-error")
      {
        ess <- NA
      }
      noness <- try(density(log2(nonessentials[, files[x]])))
      if(class(noness) == "try-error")
      {
        noness <- NA
      }
      
      
      essentialDistribution2 = list(list("essentials" = ess, "nonessentials" = noness))
      attr(essentialDistribution2, which="name") <- files[x]
    } else
    {
      write(paste(userID, ": 2"), logFile, append = TRUE)
      attr.old <- attr(essentialDistribution2, which="name")
      
      ess <- try(density(log2(essentials[, files[x]])))
      if(class(ess) == "try-error")
      {
        ess <- NA
      }
      noness <- try(density(log2(nonessentials[, files[x]])))
      if(class(noness) == "try-error")
      {
        noness <- NA
      }
      
      essentialDistribution2 <- c(essentialDistribution2, list(list("essentials" = ess, "nonessentials" = noness)))
      attr(essentialDistribution2, which="name") <- c(attr.old, files[x])
    }
  }
  # add to essentialDistribution
  essentialDistribution[[i]] <- essentialDistribution2
  
}




### progress
progress <- 0.21
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

# write(paste(userID, ": creating object readDistributionBox"), logFile, append = TRUE)
# # make log2
# readcountbox1 <- log2((dplyr::select(cp$readcount, -design,-gene)+1) )
# readcountbox2 <- data.frame(
#   "design" = cp$readcount$design,
#                       stringsAsFactors=FALSE)
# readcountbox2 <- dplyr::bind_cols(readcountbox2, readcountbox1)
# readcountbox2$gene <- cp$readcount$gene
# readDistributionBox <- tryFunction(reshape2::melt(readcountbox2), "qc")

### progress
progress <- 0.22
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

write(paste(userID, ": creating object readDistributionBoxNorm"), logFile, append = TRUE)

readcountbox1 <- log2((dplyr::select(cp$normalized.readcount, -design,-gene, -design.old)+1) )
readcountbox2 <- data.frame(
  "design" = cp$normalized.readcount$design,
  stringsAsFactors=FALSE)
readcountbox2 <- dplyr::bind_cols(readcountbox2, readcountbox1)
readcountbox2$gene <- cp$normalized.readcount$gene
readDistributionBoxNorm <- tryFunction(reshape2::melt(readcountbox2), "qc")

### progress
progress <- 0.23
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

## read depth
write(paste(userID, ": creating object readDepth"), logFile, append = TRUE)
readDepth <- tryFunction(
  car.read.depth(statistics = TRUE, labelgenes = NULL, controls.target = cp$miaccs$controls.target, 
                                 controls.nontarget = cp$miaccs$controls.nontarget, dataframe = TRUE), "qc")

### progress
progress <- 0.24
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

## designs
write(paste(userID, ": creating object geneDesigns"), logFile, append = TRUE)
geneDesigns <- tryFunction(
  car.reads.genedesigns(extractpattern = cp$miaccs$g.extractpattern, dataframe = TRUE), "qc")


## controls
write(paste(userID, ": creating object readCountVS"), logFile, append = TRUE)

readCountVS <- tryFunction(car.read.count.vs(nontargeting.controls = cp$miaccs$controls.nontarget, pos.controls=cp$miaccs$controls.target), "qc")


### progress 30%
progress <- 0.3
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))

write(paste(userID, ": creating object sample.list"), logFile, append = TRUE)

#### CALCULATE SAMPLE COMPARISONS

# get all samples that a user uploaded
#write(paste(userID, "load caTools"), logFile, append = TRUE)


samples <- cp$miaccs$file.names
write(paste(userID, "Make Combinations"), logFile, append = TRUE)
combinationslist <- as.data.frame(caTools::combs(datasetspca, 2), stringsAsFactors=FALSE)
# make list for bplapply
combinations <- as.list(1:nrow(combinationslist))
names(combinations) <- paste(combinationslist$V1, "vs." ,combinationslist$V2)
for(i in 1:length(combinations))
{
  combinations[[i]] = list("V1" = combinationslist[i,"V1"], "V2" = combinationslist[i,"V2"])
}
rm(sample.list)
# calculate all combinations (excluding self-self)
#write(paste(userID, "start combinations"), logFile, append = TRUE)
options(bphost="localhost")
sample.list <- BiocParallel::bplapply(combinations, function(u){
  
  # sample 1:   combinations$V1
  # sample 2:   combinations$V2
  
  ###### sgrna information
  #write(paste(userID, u, "1"), logFile, append = TRUE)
  log2foldchange <- log2(as.numeric(cp$normalized.readcount[,u[["V1"]]] / cp$normalized.readcount[,u[["V2"]]]))
  untreated.log10 <- log10(cp$normalized.readcount[,u[["V1"]]])
  #write(paste(userID, u, "1.1"), logFile, append = TRUE)
  zscore.untreated <- log10(cp$normalized.readcount[,u[["V1"]]])
  
  zscore.untreated[is.infinite(zscore.untreated)] <- NA
  
  #write(paste(userID, u, "1.2"), logFile, append = TRUE)
  untreated.log10[is.infinite(untreated.log10)] <- NA
  
  #write(paste(userID, u, "2"), logFile, append = TRUE)
  zscore.untreated <- zscore.untreated - mean(untreated.log10, na.rm = TRUE)
  zscore.untreated <- zscore.untreated / sd(zscore.untreated, na.rm = TRUE)
  #write(paste(userID, u, "2.1"), logFile, append = TRUE)
  treated.log10 <- log10(cp$normalized.readcount[,u[["V2"]]])
  zscore.treated <- log10(cp$normalized.readcount[,u[["V2"]]])
  
  zscore.treated[is.infinite(zscore.treated)] <- NA
  
  #write(paste(userID, u, "2.2"), logFile, append = TRUE)
  
  treated.log10[is.infinite(treated.log10)] <- NA
  
  #write(paste(userID, u, "2.3"), logFile, append = TRUE)
  zscore.treated <- zscore.treated - mean(treated.log10, na.rm = TRUE)
  zscore.treated <- zscore.treated / sd(zscore.treated, na.rm = TRUE)
  #write(paste(userID, u, "3"), logFile, append = TRUE)
  #zscore.treated <- log10(cp$normalized.readcount[,u[["V2"]]]) / sd(log10(cp$normalized.readcount[,u[["V2"]]]), na.rm = TRUE)
  # remove infinites
  zscore.untreated[is.infinite(zscore.untreated)] <- NA
  
  #write(paste(userID, u, "3.1"), logFile, append = TRUE)
  zscore.treated[is.infinite(zscore.treated)] <- NA
  
  #write(paste(userID, u, "3.2"), logFile, append = TRUE)
  
  # make INF to NA
  log2foldchange[is.infinite(log2foldchange)] <- NA
  
  
  #write(paste(userID, u, "3.3"), logFile, append = TRUE)
  df.sgrna <- data.frame(
    design = cp$normalized.readcount[,"design"],
    gene = cp$normalized.readcount[,"gene"],
    log2fc = round(log2foldchange, digits=2 ),
    stringsAsFactors = FALSE
  )
  colnames(df.sgrna) <- c("design", "gene" , "log2fc")
  
  ###### gene information
  log2foldchange <- log2(as.numeric(cp$normalized.aggregated.readcount[,u[["V1"]]] / cp$normalized.aggregated.readcount[,u[["V2"]]]))
  #write(paste(userID, u, "4"), logFile, append = TRUE)
  # make INF/NaN to NA
  log2foldchange[is.infinite(log2foldchange)] <- NA
  
  #write(paste(userID, u, "4.1"), logFile, append = TRUE)
  # get all untreated read count
  df.genes <- data.frame(
    gene = cp$normalized.aggregated.readcount[,"gene"],
    log2fc = round(log2foldchange, digits=2 ),
    stringsAsFactors = FALSE
  )
  
  
  #write(paste(userID, u, "5"), logFile, append = TRUE)
  untreated.readcount.all <- log10(unlist(as.list(cp$normalized.aggregated.readcount[,u[["V2"]]])))
  #write(paste(userID, u, "5.1"), logFile, append = TRUE)
  untreated.readcount.all[is.infinite(untreated.readcount.all)] <- NA
  
  #write(paste(userID, u, "6"), logFile, append = TRUE)
  # calculate z-score for untreated
  df.genes$zscore.untreated <- sapply(df.genes$gene, function(x){
    untreated.readcount <- log10(mean(unlist(as.list(cp$normalized.aggregated.readcount[cp$normalized.aggregated.readcount$gene == as.character(x),u[["V2"]]], na.rm = TRUE))))
    if(!is.finite(untreated.readcount)) { untreated.readcount <- NA}
    #write(paste(userID, u, "6", untreated.readcount), logFile, append = TRUE)
    zscore <- (untreated.readcount - mean(untreated.readcount.all, na.rm=TRUE)) / sd(untreated.readcount.all, na.rm=TRUE)
    
    return(zscore)
  })
  #write(paste(userID, u, "6.1"), logFile, append = TRUE)
  # calc treated all
  treated.readcount.all <- log10(unlist(as.list(cp$normalized.aggregated.readcount[,u[["V1"]]])))
  treated.readcount.all[is.infinite(treated.readcount.all)] <- NA
  
  #write(paste(userID, u, "7"), logFile, append = TRUE)
  
  df.genes$zscore.treated <- sapply(df.genes$gene, function(x){
    treated.readcount <- log10(mean(unlist(as.list(cp$normalized.aggregated.readcount[cp$normalized.aggregated.readcount$gene == as.character(x),u[["V1"]]])), na.rm = TRUE))
    if(is.infinite(treated.readcount)) { treated.readcount <- NA}
    
    zscore <- (treated.readcount - mean(treated.readcount.all, na.rm=TRUE)) / sd(treated.readcount.all, na.rm=TRUE)
    return(zscore)
  })
  #write(paste(userID, u, "7.1"), logFile, append = TRUE)
  # Remove NAs etc
  df.genes$zscore.untreated[is.infinite(df.genes$zscore.untreated)] <- NA
  
  #write(paste(userID, u, "7.2"), logFile, append = TRUE)
  df.genes$zscore.treated[is.infinite(df.genes$zscore.treated)] <- NA
  # Calculate Z-Ratio
  df.genes$zratio <- apply(df.genes, 1, function(x){
    ratio <- ((as.numeric(x["zscore.treated"]) - as.numeric(x["zscore.untreated"])) / sd(df.genes$zscore.treated - df.genes$zscore.untreated, na.rm = TRUE))
    #write(paste(userID, u, "8"), logFile, append = TRUE)
    return(ratio)
  })
  # add colors
  df.genes$log2fc.color <-  "F44336"
  df.genes$zratio.color <-  "F44336"
  
  
  colnames(df.genes) <- c("gene", "log2fc", "zscore.untreated", "zscore.untreated", "zratio", "log2fc.color", "zratio.color")
  
  return(list("genes" = df.genes, "sgrna" = df.sgrna))
  
})


# output: list sample.list with each list element being a comparison. Within this element another list with genes and sgrna dataframes



### progress 40%
progress <- 0.4
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))








#####################
#### Hit Calling ####
#####################
# calculate pvalues for genes with different methods
# return dataframes which can be viewed as tables
# functions write on cp environment
# atferwards overview dataframes are calculated and 
# venn diagram objects are created summarizing cross method hits

#DEBUG
#readr::write_tsv(x=cp$readcount, path=file.path(userDir, "test.txt") )

GSE_methodlist <- list()

#### Wilcox
write(paste(userID, ": creating object wilcox"), logFile, append = TRUE)
wilcox <- list()
wilcox[["info"]] <- list("pval" = cp$miaccs$sig.pval.wilcox, "norm" = cp$miaccs$normalize)

progress <- 0.42
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))

wilcox[["data"]] <- tryFunction(stat.wilcox(normalize = FALSE, controls = cp$miaccs$controls.nontarget, 
  control.picks = cp$miaccs$control.picks, sorting = FALSE, groups = cp$groups.compare, logfile = NULL), "wilcox") #logfile=logFile if logging is used

GSE_methodlist <- list("Wilcox" = "wilcox")

### progress 45%
progress <- 0.45
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

#### deseq
write(paste(userID, ": creating object deseq"), logFile, append = TRUE)
deseq <- list()
deseq[["info"]] <- list("pval" = cp$miaccs$sig.pval.deseq)

progress <- 0.47
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))

deseq[["data"]] <- tryFunction(stat.DESeq(extractpattern = cp$miaccs$g.extractpattern, 
  sorting = FALSE, groups = cp$groups.compare, sgRNA.pval = cp$miaccs$sig.pval.deseq, 
  fitType = "mean", filename.deseq = "deseq2_data"), "deseq")

GSE_methodlist <- c(GSE_methodlist, "DESeq2" = "deseq")

### progress 55%
progress <- 0.55
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###


#### mageck
write(paste(userID, ": creating object mageck"), logFile, append = TRUE)
write(paste(userID, " Userdir", userDir), logFile, append = TRUE)
mageck <- list()
mageck[["info"]] <- list("pval" = cp$miaccs$sig.pval.mageck)
mageck[["data"]] <- tryFunction(stat.mageck(norm.fun = 'none', extractpattern = cp$miaccs$g.extractpattern, 
                                    groups = cp$groups.compare, sort.criteria = "neg", 
                                    adjust.method = "fdr", fdr.pval = cp$miaccs$sig.pval.mageck,
                                    filename = "mageckdata", mageckfolder = userDir, logfile = logFile), "mageck")

GSE_methodlist <- c(GSE_methodlist,"MAGeCK" = "mageck")

### progress 60%
progress <- 0.60
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

#### RSEA
write(paste(userID, ": creating object rsea"), logFile, append = TRUE)
rsea <- list()
rsea[["info"]] <- list("pval" = cp$miaccs$sig.pval.rsea)
rsea[["data"]] <- tryFunction(stat.RSEA(extractpattern = cp$miaccs$g.extractpattern, sorting = FALSE, 
                            groups = cp$groups.compare, normalize = TRUE, rsea.multiplier = 50, 
                            rsea.seed = NULL, type = "standard"), "rsea")

GSE_methodlist <- c(GSE_methodlist,"sgRSEA" = "rsea")

### progress 65%
progress <- 0.65
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

write(paste(userID, ": creating zratio"), logFile, append = TRUE)
#### Z-Ratio according to paper: Analysis of Microarray Data Using Z Score Transformation
zratio <- data.frame(
  gene = cp$normalized.aggregated.readcount$gene,
  stringsAsFactors = FALSE)

# Calculate z-score for each gene
# Z score = (intensityG - mean intensityG1. . .Gn)/ SDG1. . .Gn

write(paste(userID, ": zratio 1"), logFile, append = TRUE)
# get all untreated read count
untreated.readcount.all <- log10(unlist(as.list(cp$normalized.aggregated.readcount[,(cp$treatmentgroup[[1]])+1])))

untreated.readcount.all[!is.finite(untreated.readcount.all)] <- NA

# untreated.readcount.all <- sapply(untreated.readcount.all, function(x){
#   if(is.infinite(x) || is.na(x))
#   {return(NA)} else {
#     return(x)
#   }
# })
write(paste(userID, ": zratio 2"), logFile, append = TRUE)
  # calculate z-score for untreated
zratio$zscore.untreated <- sapply(zratio$gene, function(x){
  untreated.readcount <- log10(mean(unlist(as.list(cp$normalized.aggregated.readcount[cp$normalized.aggregated.readcount$gene == as.character(x),(cp$treatmentgroup[[1]])+1])), na.rm = TRUE))
  #if(!is.finite(untreated.readcount)) { untreated.readcount <- NA}
  zscore <- (untreated.readcount - mean(untreated.readcount.all, na.rm=TRUE)) / sd(untreated.readcount.all, na.rm=TRUE)
  return(zscore)
})

write(paste(userID, ": zratio 3"), logFile, append = TRUE)
# calc treated all
treated.readcount.all <- log10(unlist(as.list(cp$normalized.aggregated.readcount[,(cp$treatmentgroup[[2]])+1])))
treated.readcount.all[!is.finite(treated.readcount.all)] <- NA

# treated.readcount.all <- sapply(treated.readcount.all, function(x){
#   if(is.infinite(x) || is.na(x))
#   {return(NA)} else {
#     return(x)
#   }
# })

### progress 67%
progress <- 0.67
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

zratio$zscore.treated <- sapply(zratio$gene, function(x){
  treated.readcount <- log10(mean(unlist(as.list(cp$normalized.aggregated.readcount[cp$normalized.aggregated.readcount$gene == as.character(x),(cp$treatmentgroup[[2]])+1])), na.rm = TRUE))
  if(is.infinite(treated.readcount)) { treated.readcount <- NA}
  
  zscore <- (treated.readcount - mean(treated.readcount.all, na.rm=TRUE)) / sd(treated.readcount.all, na.rm=TRUE)
  return(zscore)
})

# Remove NAs etc

zratio$zscore.untreated[!is.finite(zratio$zscore.untreated)] <- NA
zratio$zscore.treated[!is.finite(zratio$zscore.treated)] <- NA

# Calculate Z-Ratio
zratio$zratio <- apply(zratio, 1, function(x){
  ratio <-  ((as.numeric(x["zscore.treated"]) - as.numeric(x["zscore.untreated"])) / sd(zratio$zscore.treated - zratio$zscore.untreated, na.rm = TRUE))
  return(ratio)
})
  
 
GSE_methodlist <- c(GSE_methodlist,"Z-Ratio" = "zratio")


#######################


## progress 70%
progress <- 0.7
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))


#### edger
write(paste(userID, ": creating object edger"), logFile, append = TRUE)
edger <- list()
# get length of replicates, so we can "modify edgeR"

if((length(cp$treatmentgroup[[1]]) < 2) || (length(cp$treatmentgroup[[2]]) < 2) )
{
  edger.type = "noreplicate"
} else {
  edger.type = "standard"
}
write(paste(userID, ": EdgeR Type", edger.type), logFile, append = TRUE)
#print(cp$treatmentgroup[cp$groups.compare[[1]]])
edger[["info"]] <- list("pval" = cp$miaccs$sig.pval.edger)
edger[["data"]] <- tryFunction(stat.edgeR(sorting = FALSE, 
                          groups = cp$groups.compare, type = edger.type, fdr = edger[["info"]],
                          filename.edger = "edger_data"), "edger")

GSE_methodlist <- c(GSE_methodlist,"edgeR" = "edger")

### progress 73%
progress <- 0.73
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

#### BAGEL
write(paste(userID, ": creating object bagel"), logFile, append = TRUE)
write(paste(userID, ": bagel cutoffs -", info$analysisBagelLower, "-" , info$analysisBagelHigher), logFile, append = TRUE)
bagel <- list()

bageldf <- try(stat.bagel(scriptpath = cp$miaccs$scriptpath, outputfolder=userDir, groups = cp$groups.compare,  lowercutoff = info$analysisBagelLower, highercutoff = info$analysisBagelHigher, logfile = logFile))

if(class(bageldf) == "try-error")
{
  # BAGEL did not work, which CAN happen, so we need to deal with it
  bagel[["info"]] <- NULL
  bagel[["data"]] <- NULL
  
  write(paste(userID, ": bagel analysis not possible ", bageldf), logFile, append = TRUE)
  
} else
{
  bagel[["info"]] <- bageldf$info
  bagel[["data"]] <- bageldf$data
  GSE_methodlist <- c(GSE_methodlist,"BAGEL" = "bagel")
}



###
# 
#### ScreenBEAM
write(paste(userID, ": creating object screenbeam"), logFile, append = TRUE)

write(paste(userID, info$analysisScreenBEAMRun), logFile, append = TRUE)
write(paste(userID, info$analysisScreenBEAMIterations), logFile, append = TRUE)
write(paste(userID, info$analysisScreenBEAMBurnin), logFile, append = TRUE)

screenbeam <- list()
if(info$analysisScreenBEAMRun)
{
  ### progress 75%
  progress <- 0.75
  outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
  write(outInfo, file.path(userDir, "analysis.info"))
  
  screenbeamdf <- list()
  
  screenbeamdf <- tryFunction(stat.screenbeam(groups=cp$groups.compare, folder = userDir, iterations = info$analysisScreenBEAMIterations, burnin=info$analysisScreenBEAMBurnin), "screenbeam")
  
  if(class(screenbeamdf) == "try-error")
  {
    # ScreenBEAM did not work, which CAN happen, so we need to deal with it
    screenbeam[["info"]] <- NULL
    screenbeam[["data"]] <- NULL
    
    write(paste(userID, ": screenBEAM error ", screenbeamdf), logFile, append = TRUE)
    
  } else
  {
    screenbeam[["info"]] <- list("iterations" = info$analysisScreenBEAMIterations, "burnin" = info$analysisScreenBEAMBurnin, "cutoff" = as.numeric(info$analysisScreenBEAMPval))
    screenbeam[["data"]] <- screenbeamdf
    GSE_methodlist <- c(GSE_methodlist,"ScreenBEAM" = "screenbeam")
  }
  # screenbeam[["info"]] <- screenbeamdf$info
  # screenbeam[["data"]] <- screenbeamdf$data
  
} else {
  write(paste(userID, ": No screenBEAM analysis performed"), logFile, append = TRUE)
  screenbeam[["info"]] <- NULL
  screenbeam[["data"]] <- NULL
}


### progress 78%
progress <- 0.78
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))

#### Hit Candidates Overview 
write(paste(userID, ": creating object hitOverview"), logFile, append = TRUE)

saveRDS(wilcox, file = file.path(userDir, "wilcox.rds"))
saveRDS(deseq, file = file.path(userDir, "deseq.rds"))
saveRDS(mageck, file = file.path(userDir, "mageck.rds"))
saveRDS(rsea, file = file.path(userDir, "rsea.rds"))
saveRDS(edger, file = file.path(userDir, "edger.rds"))
saveRDS(zratio, file = file.path(userDir, "zratio.rds"))
saveRDS(bagel, file = file.path(userDir, "bagel.rds"))
saveRDS(cp$readcount, file = file.path(userDir, "readcount.rds"))
saveRDS(cp$normalized.readcount, file = file.path(userDir, "normalizedReadcount.rds"))
saveRDS(cp$aggregated.readcount, file = file.path(userDir, "aggregatedReadcount.rds"))



write(paste(userID, ": ", cp$miaccs$sig.pval.wilcox), logFile, append = TRUE)
hitOverview <- tryFunction(hit.overview( cutoff.deseq = cp$miaccs$sig.pval.deseq, cutoff.wilcox = cp$miaccs$sig.pval.wilcox, cutoff.mageck = cp$miaccs$sig.pval.mageck, cutoff.edger = cp$miaccs$sig.pval.edger, cutoff.rsea = cp$miaccs$sig.pval.rsea, cutoff.override=cp$miaccs$cutoff.override, cutoff.hits=cp$miaccs$compare.cutoff,methods=NULL, dataframe=TRUE), "ha")
hitOverview$color[which(hitOverview$color == "#D3D3D3FF")] <- "Not Enriched/ Depleted"
hitOverview$color[which(hitOverview$color == "#FFA500FF")] <- "Non-Overlapping Hit"
hitOverview$color[which(hitOverview$color == "#D92323FF")] <- "Overlapping Hit Enriched"
hitOverview$color[which(hitOverview$color == "#2E62A6FF")] <- "Overlapping Hit Depleted"
hitOverview$color <- factor(hitOverview$color, levels = c("Overlapping Hit Depleted", 
  "Overlapping Hit Enriched", "Non-Overlapping Hit", "Not Enriched/ Depleted"))


#### compute overlaps
overlap.enriched <- tryFunction(generate.hits(type="enriched", plot.genes="overlapping"), "ha")
overlap.depleted <- tryFunction(generate.hits(type="depleted", plot.genes="overlapping"), "ha")

top5 <- ceiling(nrow(cp$aggregated.readcount) / 100 * 5)
overlap.enriched.dataset = data.frame(
      gene = overlap.enriched,
      log2FC = cp$deseq[[1]][rownames(cp$deseq[[1]]) %in% overlap.enriched, "log2FoldChange"],
      wilcox.pval = cp$wilcox[rownames(cp$wilcox) %in% overlap.enriched,"p.value"],
      deseq2.pval = cp$deseq[[1]][rownames(cp$deseq[[1]]) %in% overlap.enriched,"padj"],
      mageck.pval = cp$mageck[[1]][rownames(cp$mageck[[1]]) %in% overlap.enriched,"pos"],
      stringsAsFactors=FALSE)

overlap.depleted.dataset = data.frame(
      gene = overlap.depleted,
      log2FC = cp$deseq[[1]][rownames(cp$deseq[[1]]) %in% overlap.depleted, "log2FoldChange"],
      wilcox.pval = cp$wilcox[rownames(cp$wilcox) %in% overlap.depleted,"p.value"],
      deseq2.pval = cp$deseq[[1]][rownames(cp$deseq[[1]]) %in% overlap.depleted,"padj"],
      mageck.pval = cp$mageck[[1]][rownames(cp$mageck[[1]]) %in% overlap.depleted,"neg"],
      stringsAsFactors=FALSE)
df <- hitOverview
hitOverview_info <- data.frame("genes" = df$genes, 
                 "wilcoxSign" = (df$wilcox.pval < wilcox$info$pval),
                 "deseqSign" = (df$deseq.pval < deseq$info$pval), 
                 "mageckSign" = (df$mageck.pval.enriched < mageck$info$pval | 
                                   df$mageck.pval.depleted < mageck$info$pval),
                 "rseaSign" = (df$rsea.enriched.pval < rsea$info$pval | 
                                   df$rsea.depleted.pval < rsea$info$pval),
                 "edgerSign" = (df$edger.pval < edger$info$pval))

### progress 80%
progress <- 0.8
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

#### return overlaps
write(paste(userID, ": creating object venn"), logFile, append = TRUE)
vennEnriched <- tryFunction(compare.analysis(type = "enriched", cutoff.override = FALSE, 
                                 cutoff.hits = top5, output = "venn"), "ha")
vennDepleted <- tryFunction(compare.analysis(type = "depleted", cutoff.override = FALSE, 
                                  cutoff.hits = top5, output = "venn"), "ha")


#### general information
# to give back to the app
# collect info of groups and dataset which were compared against each other in the analysis
compare <- list()
compare[[info$compareGroup1]] <- groups[[info$compareGroup1]]
compare[[info$compareGroup2]] <- groups[[info$compareGroup2]]
# whether controls were used
ctrls <- list("non" = ctrlNon, "pos" = ctrlPos)








##############
#### save ####
##############
# save rds files for screen quality and hit calling
# save cp environment for further use by other processes
# set progress to 1
# save info file ^= signal to app
write(paste(userID, ": write cp environment and objects as .rds files"), logFile, append = TRUE)

### progress 85%
progress <- 0.85
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

saveRDS(pca, file = file.path(userDir, "PCA.rds"))
saveRDS(statsGeneral, file = file.path(userDir, "statsGeneral.rds"))
saveRDS(unmappedGenes, file = file.path(userDir, "unmappedGenes.rds"))
saveRDS(readDistribution, file = file.path(userDir, "readDistribution.rds")) 
saveRDS(essentialDistribution, file = file.path(userDir, "essentialDistribution.rds"))
#saveRDS(readDistributionBox, file = file.path(userDir, "readDistributionBox.rds"))
saveRDS(readDistributionBoxNorm, file = file.path(userDir, "readDistributionBoxNorm.rds"))

saveRDS(CDF_list, file = file.path(userDir, "CDF_list.rds"))

### progress 90%
progress <- 0.90
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

saveRDS(readDepth, file = file.path(userDir, "readDepth.rds"))
saveRDS(geneDesigns, file = file.path(userDir, "geneDesigns.rds"))
saveRDS(readCountVS, file = file.path(userDir, "readCountVS.rds"))

### progress 91%
progress <- 0.91
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

saveRDS(wilcox, file = file.path(userDir, "wilcox.rds"))
saveRDS(deseq, file = file.path(userDir, "deseq.rds"))
saveRDS(mageck, file = file.path(userDir, "mageck.rds"))
saveRDS(rsea, file = file.path(userDir, "rsea.rds"))
saveRDS(edger, file = file.path(userDir, "edger.rds"))
saveRDS(zratio, file = file.path(userDir, "zratio.rds"))
saveRDS(bagel, file = file.path(userDir, "bagel.rds"))
saveRDS(screenbeam, file = file.path(userDir, "screenbeam.rds"))
saveRDS(cp$libFILE, file = file.path(userDir, "libFILE.rds"))

### progress 92%
progress <- 0.92
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

saveRDS(hitOverview, file = file.path(userDir, "hitOverview.rds"))
saveRDS(hitOverview_info, file = file.path(userDir, "hitOverview_info.rds"))
saveRDS(vennEnriched, file = file.path(userDir, "vennEnriched.rds"))
saveRDS(vennDepleted, file = file.path(userDir, "vennDepleted.rds"))

### progress 93%
progress <- 0.93
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

saveRDS(cp$readcount, file = file.path(userDir, "readcount.rds"))
saveRDS(cp$normalized.readcount, file = file.path(userDir, "normalizedReadcount.rds"))
saveRDS(cp$aggregated.readcount, file = file.path(userDir, "aggregatedReadcount.rds"))

saveRDS(compare, file = file.path(userDir, "compare.rds"))
saveRDS(ctrls, file = file.path(userDir, "ctrls.rds"))

saveRDS(uniquegenes, file = file.path(userDir, "uniqueGenes.rds"))
saveRDS(sample.list, file = file.path(userDir, "sampleList.rds"))

saveRDS(GSE_methodlist, file = file.path(userDir, "GSE_methodlist.rds"))

### progress 94%
progress <- 0.94
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))
###

# Write CP environment

save(cp, file = file.path(userDir, "cp.rds"))

info$info <- ""
progress <- 1
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "analysis.info"))







##########################
#### start get_info.r ####
##########################
# communicate necessary variables to get_info.r via .info file
# start get_info.r and exit
scriptpath <- file.path(info$scriptDir, "get_info.r")
filepath <- file.path(userDir, "get_info.info")


write(paste(userID, ": databasepath ", info$databasepath), logFile, append = TRUE) 

if(info$proxyurl == "" || is.null(info$proxyurl))
{
  info$proxyurl = "NULL"
}
if(info$proxyport == ""|| is.null(info$proxyport))
{
  info$proxyport = "NULL"
}

info <- c(paste("progress", 0, sep = ";"),
          paste("info", "", sep = ";"),
          paste("logDir", logDir, sep = ";"),
          paste("userID", userID, sep = ";"),
          paste("signature", info$signature, sep = ";"),
          paste("userDir", userDir, sep = ";"),
          paste("scriptDir", info$scriptDir, sep = ";"),
          paste("funDir", info$funDir, sep = ";"),
          paste("databasepath", info$databasepath, sep = ";"),
          paste("annoDataset", info$annoDataset, sep = ";"),
          paste("organism", info$annoDataset, sep = ";"),
          paste("bt2Threads", info$bt2Threads, sep = ";"),
          paste("proxyurl", info$proxyurl, sep = ";"),
          paste("proxyport", info$proxyport, sep = ";"),
          paste("ecrisp", info$ecrisp, sep = ";")
)
write(info, filepath)

write(paste(userID, ": execute: Rscript", scriptpath, filepath), logFile, append = TRUE)
system2("Rscript", args = c(scriptpath, filepath), wait = FALSE, stdout = NULL, stderr = NULL)

write(paste(userID, ": analysis.r quit at", Sys.time()), logFile, append = TRUE)
quit(save = "no", status = 0)

















 
