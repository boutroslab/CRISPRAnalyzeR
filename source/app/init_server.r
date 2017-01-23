# sourced by 'server.r'
# save as 'init_server.r'
# define objects needed throughout this session





#################
#### Session ####
#################
# create ID, create user tmp dir
# remove dir with files when session end
# write app.log for troubleshooting regarding shiny app
startTime <- Sys.time()
userID <- paste0(format(startTime, format = "%y%m%d_%H%M%S_"), paste(sample(0:9, 4), collapse = ""))
cData <- session$clientData

if( config$userDir_loc == "TMP" ){
  userDir <- tempfile(userID) 
} else {
  userDir <- file.path(config$wd, userID)
}
dir.create(userDir)

session$onSessionEnded(function() {
  unlink(userDir, recursive = TRUE)
  log <- paste(userID, ": shiny session finished at", Sys.time())
  write(log, logFile, append = TRUE)
})
logFile <- file.path(config$logDir, "app.log")
local({
  log <- c(paste(userID, ": shiny session starting at", Sys.time()),
           paste(userID, ": with userDir", userDir))
  if( !file.exists(logFile) ){
    write(log, logFile)
  } else {
    write(log, logFile, append = TRUE)
  }
})
observe({
  log <- c(
    paste(userID, ": protocol :", isolate(cData$url_protocol)),
    paste(userID, ": hostname :", isolate(cData$url_hostname)),
    paste(userID, ": pathname :", isolate(cData$url_pathname)),
    paste(userID, ": port :", isolate(cData$url_port)),
    paste(userID, ": pixelratio :", isolate(cData$pixelratio))
  )
  write(log, logFile, append = TRUE)
})

# Add usercounter
countFile <- file.path(config$logDir, "counter.log")
local({
  counter <- paste(Sys.Date(), Sys.time(), userID, sep="_")
  if( !file.exists(countFile) ){
    write(counter, countFile)
  } else {
    write(counter, countFile, append = TRUE)
  }
})






##############
#### Time ####
##############
# store different time events
time <- reactiveValues(
  "startFastq" = Sys.time(),
  "finishFastq" = Sys.time(),
  "startAnalysis" = Sys.time(),
  "finishAnalysis" = Sys.time(),
  "finishInfo" = Sys.time(),
  "startHit" = Sys.time(),
  "finishHit" = Sys.time()
)








#######################
#### Communication ####
#######################
# with other processes
# write info files for communication between app and batch processes
# create names for info and result file names
infoFiles <- list("fastq" = file.path(userDir, "fastq_extraction.info"),
                  "analysis" = file.path(userDir, "analysis.info"),
                  "heatmap" = file.path(userDir, "heatmap.info"),
                  "info" = file.path(userDir, "get_info.info"),
                  "hit" = file.path(userDir, "hit_candidate.info"),
                  "anno" = file.path(userDir, "annotation.info"),
                  "report" = file.path(userDir, "report.info"))
local({
  info <- c(paste("progress", 0, sep = ";"),
            paste("info", "", sep = ";"))
  write(info, infoFiles$fastq)
  write(info, infoFiles$analysis)
  write(info, infoFiles$heatmap)
  write(info, infoFiles$info)
  write(info, infoFiles$hit)
  write(info, infoFiles$anno)
  write(info, infoFiles$report)
})








##########################
#### status and error ####
##########################
# booleans for general app status
# characters for error messages to render


#### status
# booleans, whether something was sucessfully uploaded, tested, executed
# checked before certain scripts are executed or condition for rendering tables and plots
# some react to action buttons (within eventReactive/ observeEvent)
# some react live (within reactive/ observe)
# if they are within a reactive function (eventReactive/ reactive)
# they have the same name
# for the others:
# final = final check before 'analysis.r' is executed
status <- reactiveValues(
  "seqFiles" = FALSE, 
  "libFile" = FALSE, 
  "groupNames" = FALSE, 
  "groups" = FALSE, 
  "extract" = FALSE, 
  "extractedFiles" = FALSE, 
  "anno" = FALSE, 
  "compare" = FALSE, 
  "analysis" = FALSE, 
  "final" = FALSE, 
  "results" = FALSE, 
  "heatmap" = FALSE,
  "info" = FALSE,
  "hit" = FALSE,
  "hitResults" = FALSE,
  "geneAnnotation" = FALSE
)


#### error
# chr with html content for error messages to be rendered
# for each, there is a little renderUI to render the actual error message
# if not rendered if chr is empty ("")
# names are according to status names
# error messages appear in the relevant place
# e.g. if status reacts to a action button and status is set FALSE,
# the according error message will be printed below the action button
error <- reactiveValues(
  "seqFiles" = "", 
  "libFile" = "", 
  "groupNames" = "", 
  "groups" = "", 
  "extract" = "", 
  "extractedFiles" = "", 
  "anno" = "", 
  "compare" = "", 
  "analysis" = "", 
  "final" = "", 
  "results" = "", 
  "heatmap" = "",
  "info" = "",
  "hit" = "",
  "hitResults" = "",
  "geneAnnotation" = ""
)








#####################
#### Poll Handle ####
#####################
# functions for poll handle
# they are necessary for getting information from batch processes running in another R session
# these functions are executed every x seconds (usually 0.5)
# can't take arguments, so 2 functions for each batch process
# names 'Info_trigger_' and 'Info_read_' batch name
# for trigger and read function resp.
# trigger is cheap funct which triggers read if its output value changes
# read actually reads .info file of respective batch process

# value     chr of status with [0;1]
Info_trigger_fastq <- function() {
  x <- scan(infoFiles$fastq, what="", sep="\n", n = 1, quiet = TRUE)
  xlist <- strsplit(x, split = ";", fixed = TRUE) 
  return(xlist[[1]][-1])
}

# value   named list of 2
#         progess   numeric between 0 and 1
#         info      chr string with optional information
Info_read_fastq <- function() {
  x <- scan(infoFiles$fastq, what="", sep="\n", n = 2, quiet = TRUE)
  xlist <- strsplit(x, split = ";", fixed = TRUE) 
  return(list("progress" = as.numeric(xlist[[1]][-1]),
          "info" = xlist[[2]][-1]))
}

# value     chr of status with [0;1]
Info_trigger_analysis <- function() {
  x <- scan(infoFiles$analysis, what="", sep="\n", n = 1, quiet = TRUE)
  xlist <- strsplit(x, split = ";", fixed = TRUE) 
  return(xlist[[1]][-1])
}

# value   named list of 2
#         progess   numeric between 0 and 1
#         info      chr string with optional information
Info_read_analysis <- function() {
  x <- scan(infoFiles$analysis, what="", sep="\n", n = 2, quiet = TRUE)
  xlist <- strsplit(x, split = ";", fixed = TRUE) 
  return(list("progress" = as.numeric(xlist[[1]][-1]),
          "info" = xlist[[2]][-1]))
}

# value     chr of status with [0;1]
Info_trigger_heatmap <- function() {
  x <- scan(infoFiles$heatmap, what="", sep="\n", n = 1, quiet = TRUE)
  xlist <- strsplit(x, split = ";", fixed = TRUE) 
  return(xlist[[1]][-1])
}

# value   named list of 2
#         progess   numeric between 0 and 1
#         info      chr string with optional information
Info_read_heatmap <- function() {
  x <- scan(infoFiles$heatmap, what="", sep="\n", n = 2, quiet = TRUE)
  xlist <- strsplit(x, split = ";", fixed = TRUE) 
  return(list("progress" = as.numeric(xlist[[1]][-1]),
          "info" = xlist[[2]][-1]))
}

# value     chr of status with [0;1]
Info_trigger_info <- function() {
  x <- scan(infoFiles$info, what="", sep="\n", n = 1, quiet = TRUE)
  xlist <- strsplit(x, split = ";", fixed = TRUE) 
  return(xlist[[1]][-1])
}

# value   named list of 2
#         progess   numeric between 0 and 1
#         info      chr string with optional information
Info_read_info <- function() {
  x <- scan(infoFiles$info, what="", sep="\n", n = 2, quiet = TRUE)
  xlist <- strsplit(x, split = ";", fixed = TRUE) 
  return(list("progress" = as.numeric(xlist[[1]][-1]),
          "info" = xlist[[2]][-1]))
}

# value     chr of status with [0;1]
Info_trigger_hit <- function() {
  x <- scan(infoFiles$hit, what="", sep="\n", n = 1, quiet = TRUE)
  xlist <- strsplit(x, split = ";", fixed = TRUE) 
  return(xlist[[1]][-1])
}

# value   named list of 2
#         progess   numeric between 0 and 1
#         info      chr string with optional information
Info_read_hit <- function() {
  x <- scan(infoFiles$hit, what="", sep="\n", n = 2, quiet = TRUE)
  xlist <- strsplit(x, split = ";", fixed = TRUE) 
  return(list("progress" = as.numeric(xlist[[1]][-1]),
          "info" = xlist[[2]][-1]))
}

# value     chr of status with [0;1]
Info_trigger_anno <- function() {
  x <- scan(infoFiles$anno, what="", sep="\n", n = 1, quiet = TRUE)
  xlist <- strsplit(x, split = ";", fixed = TRUE) 
  return(xlist[[1]][-1])
}

# value   named list of 2
#         progess   numeric between 0 and 1
#         info      chr string with optional information
Info_read_anno <- function() {
  x <- scan(infoFiles$anno, what="", sep="\n", n = 2, quiet = TRUE)
  xlist <- strsplit(x, split = ";", fixed = TRUE) 
  return(list("progress" = as.numeric(xlist[[1]][-1]),
              "info" = xlist[[2]][-1]))
}





##################
#### geneList ####
##################
# reactive value watching genes entered during session
geneList <- reactiveValues(
  SPLOM = list(),
  replicates = list(),
  heatmap = list(),
  overview = list(),
  sgRNA = list(),
  compare = list(),
  anno = list()
)


