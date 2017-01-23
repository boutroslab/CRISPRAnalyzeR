# save as 'fastq_extraction.r'
# batch script run by CRISPRAnalyzeR shiny app with 1 argument
# Rscript path/fastq_extraction.r tmp/path/fastq_extraction.info
# converts fastq to readcount file if necessary


##############
#### init ####
##############

#### Tweak
# this is to ensure the poll function (which triggers every 0.5 s)
# will read the changed progress status even though this r script would
# run faster than 1s
# relevant when uploading files a second time
Sys.sleep(2)


#### load info file
# path of info file is in 1st argument
infoFile <- commandArgs(TRUE)[1] #"./newDir/fastq_extraction.info"  
x <- scan(infoFile, what="", sep="\n")
info <- list()
xlist <- strsplit(x, split = ";", fixed = TRUE) 

for( i in 1:length(xlist) ){
  info[[xlist[[i]][1]]] <- xlist[[i]][-1]
}

logDir <- info$logDir
userID <- info$userID
userDir <- info$userDir
nfiles <- length(info$paths)



#### scan sgRNA library
libScan <- scan(info$libPath, what="", sep="\n")

#### create log
logFile <- file.path(logDir, "fastq_extraction.log")
log <- c(paste(userID, ": fastq_extraction.r starting at", Sys.time()),
         paste(userID, ": sgRNA library is", info$libName),
         paste(userID, ": lines 1:5 of sgRNA library are:"),
         paste(userID, ":", libScan[1:5]),
         paste(userID, ": sgRNA library regex is", info$libRegex),
         paste(userID, ": read", nfiles, "sequencing files:"),
         paste(userID, ":", info$names),
         paste(userID, ": user named them:"),
         paste(userID, ":", info$gen_names),
         paste(userID, ": machine target regex is", info$targetRegex))
if( !file.exists(logFile) ){
  write(log, logFile)  
} else {
  write(log, logFile, append = TRUE)  
}

# Load packages
write(paste(userID, ": Loading libraries Rqc and ShortRead"), logFile, append = TRUE)

#library(Rqc)
#write(paste(userID, ": Rqc done"), logFile, append = TRUE)
library(ShortRead)
write(paste(userID, ": ShortRead done"), logFile, append = TRUE)

write(paste(userID, ": Copy files"), logFile, append = TRUE)
#### copy lib file to userDir
# add newline to sgRNA lib if there is no at end of file
file.copy(info$libPath, file.path(userDir, "libFile"), overwrite = TRUE)
info$libPath <- file.path(userDir, "libFile")
fileName <- info$libPath
a <- readChar(fileName, file.info(fileName)$size)
if( !grepl( ".*\n$", a, perl = TRUE) ){
  write(paste(userID, ": appended newline to end of sgRNA library"), logFile, append = TRUE)
  write( "\n", fileName, append = TRUE )
}





####################
#### Check File ####
####################
# function for checking file content
# chance to see problems in files before they are given to app
# this function appends log file if error occured
# arguments   path    chr string of file to be checked
#             name    chr string of original file name for user error message
#             userID  chr string for log file messages
# values      error   bolean whether there was something wrong in file
#             log     chr string for log message to be written
#             info    chr string for info message that user will see
Check_File <- function(path, name, userID, rc = FALSE){
  error <- FALSE
  log <- ""
  info <- ""
  
  # load file content
  con <- file(path)
  top <- readLines(con)
  close(con)

  # check whether readcount file has useful content
  if( is.null(top) || is.na(top) || length(top) == 0 || any(top == "") || any(top == " ") ){
    error <- TRUE
    log <- c(log, paste(userID, ": content of file is NULL, NA, or empty"))
    if(rc) info <- paste0(info,paste("Content of ", name, "does not look like a readcount file.<br/>"))
    else info <- paste0(info,paste("Content of extracted and mapped", name, "does not look like a readcount file.<br/>
                                   Please check whether you have duplicate or wrong entries in your sgRNA library file.<br/>"))
  
  } else if( any(!grepl("^.*\t[0-9]*$", top[-1], perl = TRUE)) ){
    error <- TRUE
    # get first line with this grep
    wrongline <- top[!grepl("^.*\t[0-9]*$", top[-1], perl = TRUE)]
    log <- c(log, paste(userID, ": file has at least 1 line with exception to regex: ^.*\t[0-9]*$"))
    log <- c(log,paste(userID, ": ",wrongline[1]))
    if(rc) info <- paste0(info,paste("Content of ", name, "does not look like a readcount file.<br/>"))
    else info <- paste0(info,paste("Content of extracted and mapped", name, "does not look like a readcount file.<br/>
                                   Please check whether you have duplicate or wrong entries in your sgRNA library file or whether the regular expression for extracting FASTQ files is correct.<br/>"))
  }
  
  # append log
  if( error ){
    log <- c(log, paste(userID, ": quit with error at", Sys.time()))
  }
  
  return(list("error" = error, "log" = log, "info" = info))
}

### Return n lines back to log

Check_File_log <- function(path, name, userID, numberlines, rc = FALSE){
  
  # load file content
  con <- file(path)
  top <- readLines(con,n = numberlines)
  close(con)
  
  log <- paste(userID, ": File ", name)
  log <- c(log, paste(top, collapse = "\n"))
  
  return(log)
}






######################
#### Try Function ####
######################
# wrapping expressionin try, and taking care of log and info file writing if there was an error
# arguments:  expr    the expression to be evaluated
#             place   chr str specifying the info message that will be shown to user
#             name    chr str file name of file being processed
#             logFile chr str path of logging file
#             userID  chr str of user ID
#             userDir chr str of tmp user directory
# value:  if no error occured, result of expression
#         if error occured stop with a error message
# side effects: if error occured, writes on logging and info file
tryFun <- function( expr, place, name, log = logFile, ID = userID, dir = userDir ){
  res <- try(expr)
  if( res != 0 ){
    
    write(paste(ID, ": non-0 exit status"), log, append = TRUE)
    write(paste(ID, ":", res[1]), log, append = TRUE)
    
    info <- switch(place,
      index = "Sorry, the bowtie2 index could not be made.<br/>Please make sure your sgRNA library is a real fasta file.<br/>",
      unzip = paste0("Unfortunately your fastq.gz file ", name, " could not be unzipped.<br/>Are you sure it is a valid .fastq.gz file?<br/>"),
      extract = paste0("Sorry, but sgRNA target sequences could not be extracted from your file ", name, ".<br/>Please make sure your FASTQ.gz file content match the RegEx you selected.<br/> See the help for more information.<br/>"),
      map = paste0("Sorry but I was not able to map your file ", name, " to the provided sgRNA library.<br/>Please make sure you uploaded corresponding files.<br/>")
    )
  
    outInfo <- c(paste("progress", 1, sep = ";"), paste("info", info, sep = ";"))
    write(outInfo, file.path(dir, "fastq_extraction.info"))
    
 
    write(paste(ID, ": fastq_extraction.r quit at", Sys.time()), log, append = TRUE)
    quit(save = "no", status = 1)
  
  } else return(res)
}




write(paste(userID, ": Check for Extraction Requirements"), logFile, append = TRUE)



#######################
#### bowtie2 index ####
#######################
# create bowtie2 index if necessary
if( info$extract ){
  bt2index <- file.path(userDir, "bt2Index")

  write(paste(userID, ": run:", "bowtie2-build", info$libPath, bt2index), logFile, append = TRUE)
  
  tryFun(system2("bowtie2-build", args = c(info$libPath, bt2index)), "index", "")
  
  
}








##################
#### Progress ####
##################
# not needed for readcount files (too fast anyway)
# reasonable for fastQ files
# don't want to many progress updates because it makes dropdown menu of app reload everytime
# which means it closes if it was opened by user before
# settle for 2 progress updates per fastQ file extraction

# progress updates are communicated via infoFile
# via 1st line named 'progress' (columns are ';'-sep in file)
# progress = 1 is termination signal telling app fastq-extraction.r stopped
# if 2nd line (named 'info') is empty := 0 exit status
# if 2nd line is not empty := non-0 exit status (some kind of error)
total <- sum(grepl(".*\\.fastq\\.gz$", tolower(info$names), perl = TRUE), na.rm = TRUE) * 2
if( total > 0 ) step <- 0.9 / total
count <- 0.1
outInfo <- c(paste("progress", count, sep = ";"), paste("info", "", sep = ";"))
write(outInfo, file.path(userDir, "fastq_extraction.info"))








#######################
#### Process Files ####
#######################
# for several files, process files 1 by 1
# this is very fast if readcount file, so no progress update
# for fastq files give progress updates
# Always: copy files into userDir (clearer than working around in /tmp)
# FastQ: unzip -> extract -> map
# Always: check file content

# make filename rdy for RQC later
rqcqa <- list()
rqcqa$QCperCycle <- "empty"

write(paste(userID, ": Analysing files"), logFile, append = TRUE)

info$oldpaths <- info$paths
info$oldextractedpaths <- info$paths

if( nfiles > 1 ){
  for( i in 1:(nfiles - 1) ){
  
    # copy file
    file.copy(info$paths[i], file.path(userDir, paste0(i, ".seqFile")), overwrite = TRUE)
    info$paths[i] <- file.path(userDir, paste0(i, ".seqFile"))
    info$oldpaths[i] <- info$paths[i]
    
    #### fastQ file 
    if( grepl(".*\\.fastq\\.gz$", tolower(info$names[i]), perl = TRUE) ){

      write(paste(userID, ": fastq.gz file:", info$names[i]), logFile, append = TRUE)
      #Copy for FASTQ QC
      file.copy(info$paths[i], file.path(userDir, paste0(info$gen_names[i], ".fastq.gz")), overwrite = TRUE)
      
      file.rename(info$paths[i], paste0(info$paths[i], ".gz"))
      
      # FASTQ QC file to add to report
      if(!exists("file.rqc"))
      {
        file.rqc <-  list(file.path(userDir, paste0(info$gen_names[i], ".fastq.gz")) ) 
      }
      else
      {
        file.rqc <-  c(file.rqc, list(file.path(userDir, paste0(info$gen_names[i], ".fastq.gz")) ) )
      }
      
  
      ## unzip
      arguments <- c("-d", paste0(info$paths[i], ".gz"))
      write(paste(userID, ": run: gzip", paste(arguments, collapse = " ")), logFile, append = TRUE)
      tryFun(system2("gzip", args = arguments), "unzip", info$names[i])

      ### add couple of lines to log
      testlines <- Check_File_log(info$paths[i], info$names[i], userID, 27)
      write(testlines, logFile, append = TRUE)
      
      ## extract
      extractstring <- file.path(info$scriptDir, "CRISPR-extract.pl")
      arguments <- c(extractstring, shQuote(info$targetRegex), shQuote(info$paths[i]), info$reverse)
      write(paste(userID, ": run:", "perl", paste(arguments, collapse = " ")), logFile, append = TRUE)
      tryFun(system2("perl", args = arguments), "extract", info$names[i])
      info$paths[i] <- c(paste0(info$paths[i], "_extracted.fastq"))
      
      info$oldextractedpaths[i] <- info$paths[i]
  
      ## progress update
      count <- count + step
      info$progress <- count
      outInfo <- c(paste("progress", info$progress, sep = ";"), paste("info", info$info, sep = ";"))
      write(outInfo, file.path(userDir, "fastq_extraction.info"))
    
      ## map
      mappinglog <- paste("1>", " ", file.path(userDir, paste(info$names[i],"_bt2_error.log", sep="")), " ", "2>", " ", file.path(userDir, paste(info$names[i],"_bt2.log", sep="")), sep= "") # will catch the stderr to gather mapping information
      
      params <- c("-p", info$bt2Threads, paste0("--", info$bt2Sensitivity))
      arguments <- c("-x", bt2index, "-U", info$paths[i], "-S", paste0(info$paths[i], ".sam"),  params, mappinglog)
      write(paste(userID, ": run:", "bowtie2", paste(arguments, collapse = " ")), logFile, append = TRUE)
      tryFun(system2("bowtie2", args = arguments), "map", info$names[i])
    
      extractstring <- shQuote(file.path(info$scriptDir, "CRISPR-mapping.pl"))
      arguments <- c(extractstring, info$libPath, paste0(info$paths[i], ".sam"), info$bt2Quality)
      write(paste(userID, ": run: perl", paste(arguments, collapse = " ")), logFile, append = TRUE)
      tryFun(system2("perl", args = arguments), "map", info$names[i])
      info$paths[i] <- paste0(info$paths[i], "-designs.txt")


      # Write couple of lines to log
      testlines <- Check_File_log(info$paths[i], info$names[i], userID, 10)
      write(testlines, logFile, append = TRUE)
      
      ## check
      test <- Check_File(info$paths[i], info$names[i], userID)
      
      if( test$error ){
        outInfo <- c(paste("progress", 1, sep = ";"), paste("info", test$info, sep = ";"))
        write(outInfo, infoFile)
        write(test$log, logFile, append = TRUE)
        quit(save = "no", status = 1)
      }
      
      ## progress update
      count <- count + step
      info$progress <- count
      outInfo <- c(paste("progress", info$progress, sep = ";"), paste("info", info$info, sep = ";"))
      write(outInfo, file.path(userDir, "fastq_extraction.info"))
    

    #### readcount file 
    } else {
      
      ## check
      test <- Check_File(info$paths[i], info$names[i], userID, rc = TRUE)
      if( test$error == TRUE ){
        outInfo <- c(paste("progress", 1, sep = ";"), paste("info", test$info, sep = ";"))
        write(outInfo, infoFile)
        write(test$log, logFile, append = TRUE)
        quit(save = "no", status = 1)
      }
    
    }

    write(paste(userID, ": processed file", i, "of", nfiles), logFile, append = TRUE)
  
  }
}








###########################
#### Process Last File ####
###########################
# final file operation
# decoupled from loop to make sure all results are written before
# finishing signal (process = 1) is set
i <- length(info$names)


#### copy file in userDir
file.copy(info$paths[i], file.path(userDir, paste0(i, ".seqFile")), overwrite = TRUE)
info$paths[i] <- file.path(userDir, paste0(i, ".seqFile"))
# old file
info$oldpaths[i] <- info$paths[i]

#### Fastq file 
if( grepl(".*\\.fastq\\.gz$", tolower(info$names[i]), perl = TRUE) ){

  write(paste(userID, ": FASTQ.gz file:", info$names[i]), logFile, append = TRUE)
  #Copy for FASTQ QC
  file.copy(info$paths[i], file.path(userDir, paste0(info$gen_names[i], ".fastq.gz")), overwrite = TRUE)
  
  file.rename(info$paths[i], paste0(info$paths[i], ".gz"))
  
  # FASTQ QC file to add to report
  file.rqc <-  c(file.rqc, list(file.path(userDir, paste0(info$gen_names[i], ".fastq.gz")) ) )
  
  ## unzip
  arguments <- c("-d", paste0(info$paths[i], ".gz"))
  write(paste(userID, ": run: gzip", paste(arguments, collapse = " ")), logFile, append = TRUE)
  tryFun(system2("gzip", args = arguments), "unzip", info$names[i])
  
  
  ### add couple of lines to log
  testlines <- Check_File_log(info$paths[i], info$names[i], userID, 27)
  write(testlines, logFile, append = TRUE)
  
  ## extract
  extractstring <- file.path(info$scriptDir, "CRISPR-extract.pl")
  arguments <- c(extractstring, shQuote(info$targetRegex), info$paths[i], info$reverse)
  write(paste(userID, ": run: perl", paste(arguments, collapse = " ")), logFile, append = TRUE)
  tryFun(system2("perl", args = arguments), "extract", info$names[i])
  info$paths[i] <- c(paste0(info$paths[i], "_extracted.fastq"))
  info$oldextractedpaths[i] <- info$paths[i]
  
  ## progress update
  count <- count + step
  info$progress <- count
  outInfo <- c(paste("progress", info$progress, sep = ";"), paste("info", info$info, sep = ";"))
  write(outInfo, paste(userDir, "fastq_extraction.info", sep = "/"))
    
  ## map
  mappinglog <- paste("2>",file.path(userDir, paste(info$names[i],"_bt2.log", sep="")), sep= " ") # will catch the stderr to gather mapping information
  params <- c("-p", info$bt2Threads, paste0("--", info$bt2Sensitivity))
  arguments <- c("-x", bt2index, "-U", info$paths[i], "-S", paste0(info$paths[i], ".sam"),  params, mappinglog)
  write(paste(userID, ": run: bowtie2", paste(arguments, collapse = " ")), logFile, append = TRUE)
  tryFun(system2("bowtie2", args = arguments), "map", info$names[i])

  extractstring <- file.path(info$scriptDir, "CRISPR-mapping.pl")
  arguments <- c(extractstring, info$libPath, paste0(info$paths[i], ".sam"), info$bt2Quality)
  write(paste(userID, ": run: perl", paste(arguments, collapse = " ")), logFile, append = TRUE)
  tryFun(system2("perl", args = arguments), "map", info$names[i])
  info$paths[i] <- paste0(info$paths[i], "-designs.txt")



  ## check
  test <- Check_File(info$paths[i], info$names[i], userID)
  if( test$error ){
    outInfo <- c(paste("progress", 1, sep = ";"), paste("info", test$info, sep = ";"))
    write(outInfo, infoFile)
    write(test$log, logFile, append = TRUE)
    quit(save = "no", status = 1)
  }
      
  
#### readcount file 
} else {
      
  ## check
  test <- Check_File(info$paths[i], info$names[i], userID, rc = TRUE)
  if( test$error ){
    outInfo <- c(paste("progress", 1, sep = ";"), paste("info", test$info, sep = ";"))
    write(outInfo, infoFile)
    write(test$log, logFile, append = TRUE)
    quit(save = "no", status = 1)
  }
  
}

write(paste(userID, ": processed file", i, "of", nfiles), logFile, append = TRUE)

# DEBUG
#write(paste(userID, ": Length file.rqc", length(file.rqc)), logFile, append = TRUE)
#write(paste(userID, ": rqc", rqcqa), logFile, append = TRUE)

outInfo <- c(paste("progress", 0.9, sep = ";"), paste("info", "", sep = ";"))
write(outInfo, file.path(userDir, "fastq_extraction.info"))

write(paste(userID, ": ", exists("file.rqc"), collapse = " - "), logFile, append = TRUE)

###### FASTQ DATA QUALITY  
### check for FASTQ quality plot if fastq files were loaded
if(exists("file.rqc"))#length(file.rqc) >=1 ) #&& file.rqc[[1]] != ""
{
  write(paste(userID, ": Starting FASTQ QC Analysis"), logFile, append = TRUE)
  write(paste(userID, ": FASTQ QC Analysis on ", file.rqc, collapse = ""), logFile, append = TRUE)
  
  #write(paste(userID, ": ", unlist(file.rqc), collapse = " - "), logFile, append = TRUE)
  options(bphost="localhost")
  
  outInfo <- c(paste("progress", 0.91, sep = ";"), paste("info", "", sep = ";"))
  write(outInfo, file.path(userDir, "fastq_extraction.info"))
  
  fastq.qa <- Rqc::rqcQA(unlist(file.rqc), workers=as.numeric(info$bt2Threads))
  
  write(paste(userID, ": Creating FASTQ QC Analysis Report"), logFile, append = TRUE)
  
  
  outInfo <- c(paste("progress", 0.92, sep = ";"), paste("info", "", sep = ";"))
  write(outInfo, file.path(userDir, "fastq_extraction.info"))
  # make report file for download
  reportFile <- Rqc::rqcReport(fastq.qa, outdir = info$userDir, file = "FASTQ_QA_Report")
  
  write(paste(userID, ": Creating FASTQ QC Analysis Plots"), logFile, append = TRUE)
  # Create plots
  
  outInfo <- c(paste("progress", 0.93, sep = ";"), paste("info", "", sep = ";"))
  write(outInfo, file.path(userDir, "fastq_extraction.info"))
  
  
  # Plots
  
  #### Per cycle plots
  
  # quality per cycle
  #rqcqa$QCperCycle <- Rqc::rqcCycleAverageQualityCalc(fastq.qa)
  rqcqa$QCperCycle <- Rqc::rqcCycleAverageQualityPlot(fastq.qa)
  
  # rqcCycleGCPlot(qa) -> included
  #GC Content per cycle
  
  rqcqa$GCcontent <- Rqc::rqcCycleGCPlot(fastq.qa)
  
  # rqcCycleQualityPlot(qa) -> 
  #quality per cycle map
  
  rqcqa$cycleqcmap <- Rqc::rqcCycleQualityPlot(fastq.qa)
  
  # rqcCycleQualityBoxPlot(qa) -> included
  # Boxplot showing quality per cycle
  
  rqcqa$QCcycle <- Rqc::rqcCycleQualityBoxPlot(fastq.qa)
  
  # rqcCycleBaseCallsPlot(qa) -> included
  # Bases called in stacked area plot
  
  rqcqa$CycleBasecall <- Rqc::rqcCycleBaseCallsPlot(fastq.qa)
  
  # rqcCycleBaseCallsLinePlot(qa)
  # Same as line
  
  rqcqa$CycleBasecallLine <- Rqc::rqcCycleBaseCallsLinePlot(fastq.qa)

  
  ### Other plots
  
  # rqcReadQualityBoxPlot(qa)
  # Boxplot of quality
  rqcqa$ReadQualityBoxPlot <- Rqc::rqcReadQualityBoxPlot(fastq.qa)
  
  
  # rqcReadQualityPlot(qa)
  rqcqa$ReadQualityPlot <- Rqc::rqcReadQualityPlot(fastq.qa)
  
  # rqcCycleAverageQualityPlot(qa)
  #AVerage quality plot
  rqcqa$AverageQualityPlot <- Rqc::rqcCycleAverageQualityPlot(fastq.qa)
  
  
  # rqcReadFrequencyPlot(qa)
  # Frequence of reads
  
  rqcqa$ReadFrequency <- Rqc::rqcReadFrequencyPlot(fastq.qa)
  
  # rqcReadWidthPlot(qa) -> already included
  #With of reads (should be same as sequencing length)
  rqcqa$Width <- Rqc::rqcReadWidthPlot(fastq.qa)
  

  
  write(paste(userID, ": rqc", length(rqcqa)), logFile, append = TRUE)
  
  write(paste(userID, ": Save FASTQ QC Analysis to rqcqa.rds"), logFile, append = TRUE)
  # save rqcqa
  
  saveRDS(rqcqa, file = file.path(userDir, "rqcqa.rds"))
  
}

outInfo2 <- c(paste("progress", 0.97, sep = ";"), paste("info", "", sep = ";"))
write(outInfo2, file.path(userDir, "fastq_extraction.info"))

################
#### Finish ####
################
# write results, then update info and progress 
# finishing signal is progress = 1

if(rqcqa$QCperCycle == "empty")
{
  outInfo <- c(paste("progress", 1, sep = ";"),
               paste("info", info$info, sep = ";"),
               paste("names", paste(info$names, collapse = ";"), sep = ";"),
               paste("paths", paste(info$paths, collapse = ";"), sep = ";"),
               paste("oldpaths", paste(info$oldpaths, collapse = ";"), sep = ";"),
               paste("oldextractedpaths", paste(info$oldextractedpaths, collapse = ";"), sep = ";"),
               paste("gen_names", paste(info$gen_names, collapse = ";"), sep = ";"),
               paste("libName", paste(info$libName, collapse = ";"), sep = ";"),
               paste("libPath", paste(info$libPath, collapse = ";"), sep = ";"),
               paste("rqc", "empty", sep = ";"))
} else
{
  outInfo <- c(paste("progress", 1, sep = ";"),
               paste("info", info$info, sep = ";"),
               paste("names", paste(info$names, collapse = ";"), sep = ";"),
               paste("paths", paste(info$paths, collapse = ";"), sep = ";"),
               paste("oldpaths", paste(info$oldpaths, collapse = ";"), sep = ";"),
               paste("oldextractedpaths", paste(info$oldextractedpaths, collapse = ";"), sep = ";"),
               paste("gen_names", paste(info$gen_names, collapse = ";"), sep = ";"),
               paste("libName", paste(info$libName, collapse = ";"), sep = ";"),
               paste("libPath", paste(info$libPath, collapse = ";"), sep = ";"),
               paste("rqc", paste(file.path(userDir, "rqcqa.rds")), sep = ";"))
}

write(outInfo, paste(info$userDir, "fastq_extraction.info", sep = "/"))
write(outInfo, paste(info$userDir, "fastq_extraction.info.bak", sep = "/"))



write(paste(userID, ": fastq_extraction.r quit at", Sys.time()), logFile, append = TRUE)

quit(save = "no", status = 0)





