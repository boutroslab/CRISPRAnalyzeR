
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

if( config$userDir_loc != "WD" ){
  userDir <- file.path(config$userDir_loc, userID)
} else {
  userDir <- file.path(config$wd, "userdata", userID)
}
dir.create(userDir)

# chmod to provide a workaround for issues with python
command = "chmod"
args = c("-R", "777", userDir)

system2(command = command,args = args)

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

# add other logs

logging_files <- list("analysis" = "analysis.log",
                  "fastq_extraction" = "fastq_extraction.log",
                  "gene_annotation" = "gene_annotation.log",
                  "get_info" = "get_info.log",
                  "heatmap" = "heatmap.log",
                  "hit_candidate" = "hit_candidate.log",
                  "report" = "report.log"
                  )


for(i in 1:length(logging_files))
{
  file_to_write <- file.path(config$logDir, logging_files[[i]])
  local({
    log <- c(paste(userID, ": ####################### NEW USER at", Sys.time()),
             paste(userID, ": ####################### with userDir", userDir))
    if( !file.exists(file_to_write) ){
      write(log, file_to_write)
    } else {
      write(log, file_to_write, append = TRUE)
    }
  })
  
}






## check external config settings from docker image

# make enrichr boolean if required
if(config$EnrichR == "FALSE")
{
  config$EnrichR = FALSE
} else if(config$EnrichR == "TRUE")
{
  config$EnrichR = TRUE
} else 
{
  config$EnrichR = TRUE
}

# max upload
config$max_upload <- as.numeric(config$max_upload)
# bt2 threads
config$bowtie_threads <- as.numeric(config$bowtie_threads)
# proxyport
if(!is.null(config$proxy_port) && config$proxyport != "")
{
  config$proxyport <- as.numeric(config$proxy_port)
}




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

### Set proxy settings for libCurl and RCurl
## httr is done on the fly

if(!is.null(config$car.proxy.url) && !is.null(config$car.proxy.port))
{
  options(RCurlOptions = list(
    proxy=paste(config$car.proxy.url, config$car.proxy.port, sep=":"),
    http.version = 1)
  )
   opts <- list(
     "CURLOPT_PROXY"         = config$car.proxy.url, 
     "CURLOPT_PROXYPORT"     = config$car.proxy.port
   )
  # RCurl::curlSetOpt(opts)
  #httr::set_config(httr::use_proxy(url = config$car.proxy.url, port = as.numeric(config$car.proxy.port)))
  Sys.setenv(http_proxy=paste(config$car.proxy.url, config$car.proxy.port, sep=":"))
  httr::set_config(httr::config(ssl_verifypeer = 0L))
  
} else {
  
  options(RCurlOptions = list(
          proxy="",
          http.version = 1)
          )
  
   opts <- list(
         "CURLOPT_PROXY"         = NULL, 
         "CURLOPT_PROXYPORT"     = NULL
      )
  # RCurl::curlSetOpt(opts)
  #httr::set_config(httr::use_proxy(url = NULL, port = NULL))
  Sys.unsetenv("http_proxy")
  httr::set_config(httr::config(ssl_verifypeer = 0L))
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

###############################
### Status and Status Updates #
###############################

## BiomaRt online?
#output$biomart_online <- shiny::renderUI({
#  handling <- biomaRt::useEnsembl(biomart = config$car.bm.database, host = "www.ensembl.org")
#})

## sgRNA Re-Evaluation online?
# check for ecrisp/databases

output$reevaluation_active <- shinydashboard::renderValueBox({
  if(dir.exists(config$databasepath))
  {
    out <- HTML("<span class='text'><strong>Local sgRNA re-evaluation database found</strong></span>")
    col <- "green"
  } else {
    out <- HTML("<span class='text'><strong>Local sgRNA re-evaluation database was not found</strong></span>")
    col <- "red"
  }
  
  shinydashboard::infoBox(width = 12,
           title = "Local sgRNA Re-Evaluation",
           value = out,
           icon = icon("open-file", lib = "glyphicon"),
           color = col,
           fill=TRUE
  )

})

## COSMIC Database online?
output$cosmic_active <- shinydashboard::renderValueBox({
  
  if(is.null(config$COSMIC_database))
  {
    out <- HTML("<span class='text'><strong>Inactive</strong></span>")
    col<- "red"
  } else {
    if(file.access(file.path(config$database_path,config$COSMIC_database), mode = 4) == 0)
    {
      out <- HTML("<span class='text'><strong>Active</strong></span>")
      col = "green"
    } else {
      out <- HTML("<span class='text'><strong>Inactive</strong></span>")
      col = "red"
    }
  }
  
  
  shinydashboard::infoBox(width = 12,
                          title = "COSMIC Mutation Database",
                          value = out,
                          icon = icon("hdd", lib = "glyphicon"),
                          color = col,
                          fill=TRUE
  )

})

## Enrichr activated?

output$enrichr_active <- shinydashboard::renderValueBox({
  
  
  if(config$EnrichR == TRUE)
  {
    if(!httr::with_config(httr::use_proxy(url = config$car.proxy.url, port = config$car.proxy.port),httr::http_error(config$EnrichR_URL)))
    {
      out <- HTML("<span class='text'><strong>Active</strong></span>")
      col = "green"
    } else {
      out <- HTML("<span class='text'><strong>Website could not be accessed.</strong></span>")
      col = "yellow"
    }
    
  } else {
    out <- HTML("<span class='text'><strong>Inactive</strong></span>")
    col = "red"
  }

  shinydashboard::infoBox(width = 12,
                          title = "Enrichr Web Service",
                          value = out,
                          icon = icon("education", lib = "glyphicon"),
                          color = col,
                          fill=TRUE
  )
                          
})

## Version
output$version <- shinydashboard::renderValueBox({

  out <- try(check_version(url = config$versionfile, proxyurl = config$car.proxy.url, proxyport = as.numeric(config$car.proxy.port), version = config$version))
  col = "green"
  if(class(out) == "try-error")
   {
     out <- paste("<span class='text'>CRISPRAnalyzeR Version ", config$version , "</span>", sep="")
  } 
  
  shinydashboard::infoBox(width = 12,
                          title = "Installed Version of CRISPRAnalyzeR",
                          value = HTML(out),
                          icon = icon("info-sign", lib = "glyphicon"),
                          color = col,
                          fill=TRUE
  )
})

## Internet Access
output$internet_access <- shinydashboard::renderValueBox({
  
  out <- try(check_version(url = config$versionfile, proxyurl = config$car.proxy.url, proxyport = as.numeric(config$car.proxy.port), version = config$version))
  if(class(out) == "try-error")
  {
    out <- HTML("<span class=' text'><strong>No Internet Access - Please check the proxy settings</strong></span>")
    col = "red"
  } else {
    out <- HTML("<span class=' text'><strong>OK</strong></span>")
    col = "green"
  }
  
  shinydashboard::infoBox(width = 12,
                          title = "Internet Access",
                          value = out,
                          icon = icon("sort", lib = "glyphicon"),
                          color = col,
                          fill=TRUE
  )
  
  
})

## Proxy active?
output$proxy <- shinydashboard::renderValueBox({
  
  
  if(is.null(config$car.proxy) || is.null(config$car.proxy.url))
  {
    out <- HTML("<span class=' text'><strong>No Proxy Server is used</strong></span>")
    col = "green"
  } else {
    out <- HTML("<span class=' text'><strong>Proxy Server is used</strong>
                </span>")
    col = "green"
  }
  
  shinydashboard::infoBox(width = 12,
                          title = "Proxy Server",
                          value = out,
                          icon = icon("sort", lib = "glyphicon"),
                          color = col,
                          fill=TRUE
  )
  
  
})

## Download Log files

output$downloadlogs <- renderUI({
  
  # make logs downloadable for docker
  # is located in /var/log/shiny-server
  # will have CRISPRAnalyzeR in name
  if(config$downloadlogs)
  {
    out <- downloadButton("downloadlogs_button",label = "Download all log files",icon = icon("cloud-download"), width = "250px")
  } else {
    out <- ""
  }
  
  return(out)
  
})

output$downloadlogs_button <- downloadHandler(
  filename = paste("CRISPRAnalyzeR", format(startTime, format = "%y-%m-%d"), "logs.zip", sep = "_"),
  content = function(file) {
    
    if( config$downloadlogs ){
      
      # get all downloaded stuff
      # is located in /var/log/shiny-server
      # and /srv/shiny-server/CRISPRAnalyzer/log
      # will have CRISPRAnalyzeR in name
      
      # make dir
      system2(command = "mkdir", args = c(file.path(userDir,"logs") ))
      
      # copy files to userdir
      command <- "cp"
      args1 <- c("-R","/srv/shiny-server/CRISPRAnalyzeR/log/*", file.path(userDir,"logs"))
      args2 <- c("-R","/var/log/shiny-server/CRISPRAnalyzeR*", file.path(userDir,"logs"))
      system2(command,args1)
      system2(command,args2)
      
      # make it zip
      system2("zip", args = c("-r", file.path(userDir,"logs", "CRISPRAnalyzeR_logs.zip"), file.path(userDir,"logs") )) 
      

      file.copy(file.path(userDir,"logs", "CRISPRAnalyzeR_logs.zip"), file, overwrite = TRUE)
      
    } else { NULL }
  }
)




