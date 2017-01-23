# sourced from 'server.r'
# save as 'config.r'
# configuration file
## Version 0.99
# store variables in config
config <- list()



###########################
#### working directory ####
###########################
# this is the absolute path to server.r, ui.r and config.r
# it will be the working directory for the web app
# other paths are constructed from this directory
# write path in quotes ("") or getwd() for current directory
# DEFAULT getwd()
config[["wd"]] <- getwd()


########################
#### user directory ####
########################
# this controls how the unique directory, 
# which is set for every user at the beginning of a session,
# is created
# usually this is a directory with a hashed name in your temporary directory
# but for debugging it might be handy to have it in the working directory
# write in quotes ("")
# debugging "WD"
# DEFAULT "TMP"
config[["userDir_loc"]] <- "WD"



############################
#### External Databases ####
############################
# external databases such as COSMIC can be included in CRISPRAnalyzeR
# COSMIC database files need to be downloaded first from https://cancer.sanger.ac.uk/cosmic/download AND IS NOT PROVIDED with CRISPRAnalyzeR
#  
# Config path, default ./database
config[["database_path"]] <- "./database"

# COSMIC database file, needs to be located in database_path
# DEFAULT is NULL, as CRISPRAnalyzeR is not provided with COSMIC Database
config[["COSMIC_database"]] <- NULL   # NULL if not available

# EnrichR
# Enrichr is TRuE by default, but for commercial use a license HAS to be OBTAINED!
config[["EnrichR"]] <- TRUE # False if inactive
# URL of Enrichr API
config[["EnrichR_URL"]] <- 'http://amp.pharm.mssm.edu/Enrichr/'


#############################
#### sgRNA Library and FASTQ Extraction Regex ####
#############################
# This containes a list of sgRNA regular Expression to be used in the data upload folder
# Are set as default, can be edited
config[["sgrna_regex"]] <- list(
            "^(.+?)(_.*)$",
            "^sg(.+?)(_.+)$",
            "^(.+?)(_.+)$")
names(config[["sgrna_regex"]]) <- c("Standard for TKO, Gecko, Brunello ^(.+?)(_.*)$",
                                    "Human Lentiviral sgRNA Library ^sg(.+?)(_.+)$",
                                    ">GENE1_CCDS5821.2_ex99_2:145509997-190521230:+_7-6 ^(.+?)(_.+)$"
                                    )

# This is a list for FASTQ extraction regular expressions

config[["fastq_regex"]] <- list(
            "ACC(.{20,21})G",
            "GTTT(.{20})G",
            "ACCG(.{20,21})G",
            "GTTG(.{20})GTT")
names(config[["fastq_regex"]]) <- c("Standard for LenticrispV2, LentiGuide ACC(.{20,21})G",
                                              "Human Lentivirus Library V1 GTTT(.{20})G",
                                              "pLCKO (TKO Library) ACCG(.{20,21})G",
                                              "pU6-sgRNA EF1Alpha-puro-T2A-BFP (CRISPRa/i) GTTG(.{20})GTT")



#### LOAD Biomart attributes file
## Also generate gene conversion
# Loads biomart attributes
biomart.attributes <- unique(read.table(file="biomart_hsapiens_ensembl_attributes.tab", header=TRUE, sep="\t", stringsAsFactors = FALSE, as.is = TRUE, quote=""))

# please do not change
config[["biomart.attributes"]] <- as.list(biomart.attributes$name)
names(config[["biomart.attributes"]]) <- biomart.attributes$description

config[["biomart.geneid"]] <- list(
  "ensembl_gene_id",
  "entrezgene",
  "hgnc_id",
  "hgnc_symbol",
  "unigene",
  "uniprot_genename"
)
names(config[["biomart.geneid"]]) <- biomart.attributes[biomart.attributes$name %in% config[["biomart.geneid"]],"description"]

## Organism
# select which organisms are available
# CRISPRAnalyzeR is only tested for homo_sapiens so far!
config[["organism"]] <- list("homo_sapiens","mus_musuclus","danio_rerio")
names(config[["organism"]]) <- c("Human", "Mouse", "Zebrafish")

####################
#### stylesheet ####
####################
# if you want to include a .css like stylesheet  
# place it in the 'www' directory and define its name
# you do not need to add 'www' to the path name
# if you don't want to include a stylesheet, leave this empty
# write name in quotes ("")
# DEFAULT ""
# custom stylesheet "custom.css"
config[["cssFile"]] <- "clean_green.css"




#####################
#### path to zip ####
#####################
# sometimes path to 'zip' is not set in R
# set R path to 'zip' command
# needed for 'openxlsx'
# write path in quotes ("")
# DEFAULT "/user/bin/zip"
Sys.setenv(R_ZIPCMD = "/usr/bin/zip")




###########################
#### path to app files ####
###########################
# relative path to R scripts for shiny app
# from working directory
# write path in quotes ("")
# DEFAULT "./app"
config[["appDir"]] <- "app"




###########################
#### path to log files ####
###########################
# relative path to dir with log files
# from working directory
# log files are created for troubleshooting
# mainly for monitoring processes which run in background
# write path in quotes ("")
# DEFAULT "log"
config[["logDir"]] <- "log"




#########################
#### path to tickets ####
#########################
# relative path to dir where tickets are stored
# from working directory
# tickets can be written by user as text input in help tab
# write path in quotes ("")
# DEFAULT "tickets"
config[["ticketDir"]] <- "tickets"




#########################
#### path to scripts ####
#########################
# relative path to scripts from CRISPRAnalyzeR
# from working directory
# write path in quotes ("")
# DEFAULT "scripts"
config[["scriptpath"]] <- "scripts"

#########################
#### path to E-CRISP Reevaluation Databases ####
#########################
# ABSOLUTE path to databases used for E-CRISP Re-Evaluation
# from working directory
# write path in quotes ("")
# DEFAULT ""
config[["databasepath"]] <- "/data/DATABASEFILES/"


##############################
#### car R functions ####
##############################
# relative path to directory containing all car R functions
# from working directory
# write path in quotes ("")
# DEFAULT "extfunctions"
config[["Fundir"]] <- "extfunctions"




#########################
#### bowtie2 threads ####
#########################
# threads bowtie2 should use if fastQ files were supplied
# enter plain natural number
# DEFAULT 4
config[["car.bt2.threads"]] <- 4




######################
#### Proxy Server ####
######################
# set proxy server
# enter character string
# dkfz "www-int2.inet.dkfz-heidelberg.de:80"
# DEFAULT ""
# Please uncomment if proxy is used
#config[["car.proxy"]] <- ""
config[["car.proxy.url"]] <- NULL
config[["car.proxy.port"]] <-  NULL
if(!is.null(config[["car.proxy.url"]]) && !is.null(config[["car.proxy.port"]]))
{
  config[["car.proxy"]] <- paste(config[["car.proxy.url"]], config[["car.proxy.port"]], sep=":") # NULL
} else
{
  config[["car.proxy"]] <-NULL
}




#########################################
#### E-CRISP and sgRNA Re-Annotation ####
#########################################
# how to contact E-CRISP or the sgRNA Re-annotation tool
# enter character string
# dkfz intern "http://b110-ws01/E-CRISP/reannotate_crispr_carpools.pl"
# DEFAULT "http://www.e-crisp.org/E-CRISP/reannotate_crispr_carpools.pl"
# Link to ecrisp web service
config[["ecrisp"]] <- "http://www.e-crisp.org/E-CRISP/reannotate_crispr_carpools.pl"


# Database files MUST be obtained from https://github.com/boutroslab/Supplemental-Material/tree/master/crispr-reannotation
# or http://www.dkfz.de/signaling/crispr-downloads/

##########################
#### BiomaRt Database ####
##########################

# biomaRt database
# for now there is no other database supported
# value kept for now for compatibility reasons
config[["car.bm.database"]] <- "ENSEMBL_MART_ENSEMBL"

###############
#### paths ####
###############
# constructing absolute paths
config$appDir <- file.path(config$wd, config$appDir)
config$logDir <- file.path(config$wd, config$logDir)
config$Fundir <- file.path(config$wd, config$Fundir)
config$ticketDir <- file.path(config$wd, config$ticketDir)
config$scriptpath <- file.path(config$wd, config$scriptpath)

# loading stylesheet
config$stylesheet <- ""
if( config$cssFile != "" ){
  config$cssFile <- file.path("www", config$cssFile)
  if( file.exists(config$cssFile) ){
    config$stylesheet <- paste(scan(config$cssFile, what = "", sep = "\n"), collapse = " ")
  }
}


#################
##### EMAIL #####
#################
# Email active
config[["activate.mail"]] <- FALSE

# json secret file for gmail authentification
# please see https://github.com/jimhester/gmailr for how to obtain the json file and auth key
config[["gmailjson"]] <- NULL

# Email adress used
config[["email.from"]] <- NULL

# Standard to send email to
config[["email.to"]] <- NULL

# Standard subject
config[["emailsubject"]] <- "[CRISPRAnalyzeR]"


