# save as 'gene_annotation.r'
# batch script run by shiny app to retrieve information for gene annotation
# Rscript path/gene_annotation.r tmp/path/annotation.info






##############
#### init ####
##############
# path of info file is in 1st argument
# load info file
infoFile <- commandArgs(TRUE)[1] #"./newDir/annotation.info"  
x <- scan(infoFile, what="", sep="\n")
info <- list()
xlist <- strsplit(x, split = ";", fixed = TRUE) 

for( i in 1:length(xlist) ){
  info[[xlist[[i]][1]]] <- xlist[[i]][-1]
}

logDir <- info$logDir
userID <- info$userID
userDir <- info$userDir
eCRISP <- info$ecrisp

gene <- info$geneName
annotation <- info$annotation

# get all genes and all annotations
#gene <- unlist(strsplit(gene.all, split=","))
#annotation <- unlist(strsplit(annotation.all, split=","))

wd <- getwd()
setwd(userDir)

# create log for troubleshooting
logFile <- file.path(logDir, "gene_annotation.log")
log <- c(paste(userID, ": hgene_annotation.r starting at", Sys.time()),
         paste(userID, ": gene symbol entered is"),
         paste(userID, ":", gene),
         paste(userID, ": annotation options are:"),
         paste(userID, ":", annotation))
if( !file.exists(logFile) ){
  write(log, logFile)  
} else {
  write(log, logFile, append = TRUE)  
}








##################
#### Progress ####
##################
# the progress bar of this process is a bit meaningless
# but it is needed, so that the user knows, there actually is a process
# 0.1 showing the app that this process was started
# then 0.8 right after eCRISP thing
# 1 showing the app that this process is finished
# take 1 s to give the app time to realize it was started (0.5s poll handle)
progress <- 0.1
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "annotation.info"))
Sys.sleep(1)




 




#################
#### load cp ####
#################
# load cp.rds created by analysis.r before
write(paste(userID, ": loading cp.rds"), logFile, append = TRUE) 

load(file.path(userDir, "cp.rds"))
source(file.path(info$funDir, "get.gene.info.R"))



################
#### biomaRt ####
################
# contact biomaRt and get gene annotation
# writes on cp and creates annotation file
write(paste(userID, ": contacting biomaRt"), logFile, append = TRUE) 

if( is.null(info$proxyurl) || is.na(info$proxyurl) || length(info$proxyurl) == 0 )
  {
  info$proxy <- ""
  httr::set_config(httr::use_proxy(url = NULL, port = NULL))
} else {
  info$proxy <- paste(info$proxyurl, info$proxyport, sep=":")
  httr::set_config(httr::use_proxy(url = info$proxyurl, port = as.numeric(info$proxyport)))
}

options(RCurlOptions = list(proxy = info$proxy, http.version = 1))



# check if annotation is more than 3 elements, if this is the case split it and put it in loop
if(length(annotation) > 3)
{
  # make for loop with max 3 attributes
  annotation.list <- split(annotation, ceiling(seq_along(annotation)/3))
  reslist <- c(1:length(annotation.list))
  reslist <- as.list(reslist)
  
  write(paste(userID, ":", length(annotation.list)), logFile, append = TRUE)
  
  # call biomart in loop
  for(i in 1:length(annotation.list)){
    err <- try(get.gene.info(
      gene = gene, extractpattern=expression(cp$miaccs$g.extractpattern), database="ENSEMBL_MART_ENSEMBL", dataset=info$annoDataset, filter=info$annoIDnew, new.identifier = info$annoIDnew, attribute = annotation.list[[i]], controls=FALSE, annotate = TRUE,
      convert.identifier = FALSE, host="www.ensembl.org"))
    
    write(paste(userID, ":", i), logFile, append = TRUE)
    
    if( class(err) == "try-error" ){
      write(paste(userID, ": try-error occured"), logFile, append = TRUE)
      write(paste(userID, ":", res[1]), logFile, append = TRUE)
      
      info <- "My apologies. Gene Annotation was not successful.<br/>Please contact us via the help page and create a ticket, so we can get back to you as fast as possible."
      outInfo <- c(paste("progress", 1, sep = ";"), paste("info", info, sep = ";"))
      write(outInfo, file.path(userDir, "annotation.info"))
      
      write(paste(userID, ": gene_annotation.r quit at", Sys.time()), logFile, append = TRUE)
    } else {
      write(paste(userID, ":", "write to reslist"), logFile, append = TRUE)
      # put result in list
      reslist[i] <- list(err)
    }
    
    # merge lists into one single data frame using dplyr
    if(i==1){
      
      res <- reslist[[i]]
    } else {
      
      res <- dplyr::full_join(res, reslist[[i]], by = info$annoIDnew)
    }
    
    
  }
   
} else {
  res <- try(get.gene.info(
    gene = gene, extractpattern=expression(cp$miaccs$g.extractpattern), database="ENSEMBL_MART_ENSEMBL", dataset=info$annoDataset, filter=info$annoIDnew, new.identifier = info$annoIDnew, attribute = annotation, controls=FALSE, annotate = TRUE,
    convert.identifier = FALSE, host="www.ensembl.org"
  ))
  
  
  if( class(res) == "try-error" ){
    write(paste(userID, ": try-error occured"), logFile, append = TRUE)
    write(paste(userID, ":", res[1]), logFile, append = TRUE)
    
    info <- "My apologies. Gene Annotation was not successful.<br/>Please contact us via the help page and create a ticket, so we can get back to you as fast as possible."
    outInfo <- c(paste("progress", 1, sep = ";"), paste("info", info, sep = ";"))
    write(outInfo, file.path(userDir, "annotation.info"))
    
    write(paste(userID, ": gene_annotation.r quit at", Sys.time()), logFile, append = TRUE)
  }
}


write(paste(userID, ": creating object geneAnnotation"), logFile, append = TRUE) 

write(paste(userID, ":", "Do the rest"), logFile, append = TRUE)

geneAnnotation <- list()
geneAnnotation[["table"]] = res
geneAnnotation[["filter"]] = cp$miaccs$g.identifier.new
geneAnnotation[["annotation"]] = annotation

progress <- 0.8
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "annotation.info"))







##############
#### save ####
##############
# save cp environment for further use by other processes
# set progress to 1
# save info file ^= signal to app
write(paste(userID, ": writing cp environment and geneAnnotation .rds files"), logFile, append = TRUE)

save(cp, file = file.path(userDir, "cp.rds"))
saveRDS(geneAnnotation, file = file.path(userDir, "geneAnnotation.rds"))

write(paste(userID, ": gene_annotation.r quit at", Sys.time()), logFile, append = TRUE)

info$info <- ""
progress <- 1
outInfo <- c(paste("progress", progress, sep = ";"), paste("info", info$info, sep = ";"))
write(outInfo, file.path(userDir, "annotation.info"))

quit(save = "no", status = 0)











 
