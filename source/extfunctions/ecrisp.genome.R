ecrisp.genome <- function(organism = "homo_sapiens",
                            ensembl.genome.release = "GRCh38",#"hg19",
                            database = cp$miaccs$a.database,
                            dataset = cp$miaccs$a.dataset,
                            host = "www.ensembl.org",
                            biomart = TRUE,
                            local.genome.release = NULL,
                            timeout = 6000,
                            sleep = 10,
                            unspecific_bases = 1,
                            edit_distance = 2,
                            cas = "SpCas9", # other Cas Proteins will be possible in the future
                            reannotate = FALSE,
                            dataframe = FALSE,
                            extractpattern = expression("^(.+?)(_.+)"), # should be cp$miaccs$g.extractpattern in final version
                            debug=FALSE,
                            #ecrisp = "http://b110-ws01/E-CRISP/reannotate_crispr_carpools.pl"
                            ecrisp = "http://www.e-crisp.org/E-CRISP/reannotate_crispr_carpools.pl",
                            write=TRUE,
                          userid = NULL
                           ) # single, all
{
  plot.offtarget = FALSE
  
  # Get assembly
  if(ensembl.genome.release == "GRCh37")
  {
    host = "grch37.ensembl.org"
    ucsc.genome <- "hg19"
  } else if(ensembl.genome.release == "GRCh38" || ensembl.genome.release == "hg19")
  {
    ucsc.genome <- "hg38"
    host <- host
  } 
  
#   organism = "homo_sapiens"
#   ensembl.genome.release = "hg19"
#   database = cp$miaccs$a.database
#   dataset = cp$miaccs$a.dataset
#   host = "www.ensembl.org"
#   biomart = TRUE
#   local.genome.release = NULL
#   timeout = 600
#   sleep = 10
#   unspecific_bases = 1
#   edit_distance = 0
#   type = "single"
#   labelgene = "BAX"
#   mapping.sgRNA = "all" # missing enriched depleted
#   annotate = c("regulation","foldchange","fdr", "doench","CDS","seed_GC") #unmappedXU
#   from.zoom=NULL
#   to.zoom=NULL
#   get.model = "single"
#   overwrite=FALSE
#   span = 0.4

# ####### Idea is to
# ## Load fasta library file sgRNAs and re-evaluate them using E-CRISP evaluation
# 
  
  ## Was E-CRISP already asked for the library annotation?
if(!exists("ecrisp",envir=cp) || identical(reannotate,TRUE))
{ 
    
  # # Load sgRNAs from cp$libFile and make fasta sequences from it
  # 
  # Preapre to just call for single genes, in this case labelgene!
  #sgrnas.to.pick <- cp$readcount[cp$readcount$gene == labelgene,"design"]
  sgrna.fasta  <- as.character(paste('>',cp$libFILE[,"design"],'\n',cp$libFILE[,"sequence"],'\n',collapse="", sep=""))
  

# 
# ## get all scores for this sgRNA and genomic location
# # call E-CRISP evaluation
# library(RCurl)

  ua  <- ""#"Mozilla/5.0 (Windows NT 6.1; WOW64; rv:33.0) Gecko/20100101 Firefox/33.0"
  url <- ecrisp
  ecrisp.annotate <- httr::POST(url, 
              body = list(ref_organism = dataset,
                           unspecific_leading_bases = unspecific_bases,
                           edit_distance_allowed = edit_distance,
                           data_type = "fasta",
                           pasted_seq = sgrna.fasta,
                           PAM = cas,
                           userid = userid,
                           send = "Submit"),
              encode = "form",
              httr::add_headers("Expect"=""),
              httr::user_agent(ua))
# print(httr::status_code(ecrisp.annotate))
# print(httr::content(ecrisp.annotate))

if(httr::status_code(ecrisp.annotate) == 200)
{
  # E-CRISP sends OK status code, so we go on
  folder <- httr::content(ecrisp.annotate)
  if(debug==TRUE)
  {print(folder)}
}
 else
 {
    stop(paste("The server returned the following http status code:",httr::status_code(ecrisp.annotate),"\n",httr::content(ecrisp.annotate), sep=" ") )
  }

#View(ECRISP)

# We get the working directory backand look for the presence of a results.tab file to read using read.table
# check for presence until a certain timeout

time <- R.utils::currentTimeMillis.System() # in milliseconds
time.timeout <- time+(timeout*1000)



while(time < time.timeout )
{
  if(debug==TRUE)
  {
    print(RCurl::url.exists(paste("http://www.e-crisp.org/E-CRISP/workdir/",folder,"/results.tab",sep="",collapse="")))
  }
  if(RCurl::url.exists(paste("http://www.e-crisp.org/E-CRISP/workdir/",folder,"/results.tab",sep="",collapse="")) )
  {
   # downloaded.file <- download.file(paste("http://b110-ws01/E-CRISP/workdir/",folder[1],"/results.tab",sep="",collapse=""), destfile = "results.tab", method = "internal", quiet = FALSE, mode = "w",
    #              cacheOK = TRUE
     #             )
    
    Sys.sleep(sleep)
    # Download file
    loaded.file <- download.file(paste("http://www.e-crisp.org/E-CRISP/workdir/",folder,"/results.tab",sep="",collapse=""), paste(folder,"results.tab", sep=""), method="auto", quiet = FALSE, mode = "w",
                  cacheOK = FALSE,
                  extra = getOption("download.file.extra"))

    #ecrispresult <- read.table("results.tab", header=TRUE, sep="\t", comment.char = "", stringsAsFactors = TRUE, colClasses = c("factor","factor","numeric","numeric","character","numeric","numeric","numeric","character","character","character","numeric","numeric","numeric","numeric","numeric","character"))
    results.old <- file.size(paste(folder,"results.tab", sep=""))
    results.new <- file.size(paste(folder,"results.tab", sep=""))
    
    while(results.new >= results.old)
    {
      # Download and test
      loaded.file <- download.file(paste("http://www.e-crisp.org/E-CRISP/workdir/",folder,"/results.tab",sep="",collapse=""), paste(folder,"results.tab", sep=""), method="auto", quiet = FALSE, mode = "w",
                                   cacheOK = FALSE,
                                   extra = getOption("download.file.extra"))
      results.old <- file.size(paste(folder,"results.tab", sep=""))
      
      # Since results.tab needs to be generated, which takes a while, we send this R script to a sleep
      if(nrow(cp$libFILE) < 12000)
      {
        Sys.sleep((sleep)*2)
      }else if (nrow(cp$libFILE) > 12000 && nrow(cp$libFILE) < 40000)
      {
        Sys.sleep((sleep)*6)
      }else
      {
        Sys.sleep((sleep*12))

      }
      
      loaded.file <- download.file(paste("http://www.e-crisp.org/E-CRISP/workdir/",folder,"/results.tab",sep="",collapse=""), paste(folder,"results.tab", sep=""), method="auto", quiet = FALSE, mode = "w",
                                   cacheOK = FALSE,
                                   extra = getOption("download.file.extra"))
      results.new <- file.size(paste(folder,"results.tab", sep=""))
      
      if(results.old == results.new)
      {
        #print("READ ECRISP FILE as READ TABLE")
        ecrispresult <- read.table(paste(folder,"results.tab", sep=""), header=TRUE, sep="\t", comment.char = "", stringsAsFactors = TRUE, colClasses = c("factor","factor","numeric","numeric","character","numeric","numeric","numeric","character","character","character","numeric","numeric","numeric","numeric","numeric","character"))
        break
      }
      
    }
   
    
    break # abort loop since file is there
  }
  Sys.sleep(sleep) # make system sleep
  time <- R.utils::currentTimeMillis.System()
}
#print("Read ECRISP")

# Check for Timeout
if(!exists("ecrispresult"))
{stop("Timeout while contacting www.e-crisp.org. Depending on the library size, please increase the timeout limit.")}

# now we go into loop that runs a certain time and check for the presence of the table


# # load test file output
# eval.file <- read.table(file = "/Users/janwinter/Documents/PhD/GitHub/scripts/CRISPR/CRISPRAnalyzeRpools-development/test-eval.tab",header = TRUE, sep="\t",as.is = TRUE)
 colnames(ecrispresult) <- c("design", "chr","Start", "End", "Gene.targets", "Spec.Score",
                           "Anno.Score","Eff.Score","Matchstring","Sequence","Direction","CDS_score",
                           "exon_score","seed_GC","doench_score","xu_score","doench_30_mer")
 
 ecrispresult <- merge.data.frame(ecrispresult, cp$readcount[,c("gene","design")], by.x = "design", by.y="design",all.x = TRUE, all.y=FALSE)
#print("merged ecrisp")

 #remove missing strand informaiton with *
 ecrispresult[ecrispresult$Direction == "","Direction"] <- "*"
 # make chromosomes work with gviz
 
 if(!grepl(pattern = "^chr.*" , x=ecrispresult$chr, perl=TRUE))
 {
   ecrispresult$chr <- sapply(ecrispresult$chr, function(x){
     if(length(grep(expression("^([\\d\\w]+)"),as.character(x),perl=TRUE)) > 0)
     {
       return(paste("chr",as.character(x),sep="",collapse=""))
     }
     else
     {
       return(as.character(x))
     }
   })
 }

 
 
 
 
 # Store ecrispt table
 cp$ecrisp <- ecrispresult
 
} # End of ECRISP check
# 

 
 }