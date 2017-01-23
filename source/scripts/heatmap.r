# save as 'heatmap.r'
# batch script run by car shiny app with 1 argument
# Rscript path/heatmap.r tmp/path/heatmap.info
# creates plot heatmap plot object


library(highcharter)
library(data.table)








##############
#### init ####
##############
# load info file
# create log for trouble shooting
# load cp environment
# load all car functions

infoFile <- commandArgs(TRUE)[1] 
x <- scan(infoFile, what="", sep="\n")
info <- list()
xlist <- strsplit(x, split = ";", fixed = TRUE) 

for( i in 1:length(xlist) ){
  info[[xlist[[i]][1]]] <- xlist[[i]][-1]
}

logDir <- info$logDir
userID <- info$userID
userDir <- info$userDir

wd <- getwd()
setwd(userDir)

logFile <- file.path(logDir, "heatmap.log")
log <- c(paste(userID, ": heatmap.r starting at", Sys.time()),
         paste(userID, ": type:", info$show),
         paste(userID, ": direction:", info$type),
         paste(userID, ": options:", info$checks))
if( !file.exists(logFile) ){
  write(log, logFile)  
} else {
  write(log, logFile, append = TRUE)  
}

load(file.path(userDir, "cp.rds"))
source(file.path(info$funDir, "gene.heatmap.R"))









######################
#### Try Function ####
######################
# wrapping expressionin try, and taking care of log and info file writing if there was an error
# arguments:  expr    the expression to be evaluated
#             logFile chr str path of logging file
#             userID  chr str of user ID
#             userDir chr str of tmp user directory
# value:  if no error occured, result of expression
#         if error occured stop with a error message
# side effects: if error occured, writes on logging and info file
tryFunction <- function( expr, log = logFile, ID = userID, dir = userDir ){
  res <- try(expr)
  if( class(res) == "try-error" ){
    
    write(paste(ID, ": try-error occured"), log, append = TRUE)
    write(paste(ID, ":", res[1]), log, append = TRUE)
    
    info <- "Heatmap calculation failed.<br/>This can have several causes, the administrator has been informed.<br/>Please proceed to the help page and send us a ticket so we can get back to you as soon as possible."
    
    outInfo <- c(paste("progress", 1, sep = ";"), paste("info", info, sep = ";"))
    write(outInfo, file.path(dir, "heatmap.info"))
    
    write(paste(ID, ": analysis.r quit at", Sys.time()), log, append = TRUE)
    quit(save = "no", status = 1)
  
  } else return(res)
}










##################
#### Function ####
##################
# if 'show' radioButton = unmapped, run unmapped.genes function
# otherwise gne.heatmap function is run
# parameters are read from user inputs
# allplot ^= info$show
# threshold ^= thresh
# type ^= info$type
# cluster ^= cCluster
# centers ^= ncenter
# log ^? bLog
# if threshold == TRUE, 'enriched' or 'depleted' is specified by type
# for 'enriched' threshold is set to 0.95, for 'depledted' to 0.05
a <- info$checks
if( "log" %in% a ) bLog <- TRUE else bLog <- FALSE
if( info$type == "enriched" ) thresh <- 0.95 else thresh <- 0.05


# Create args for heatmap generation
# geneabundance -> allplot="fraction", base="normgenes"
if( info$show == "geneabundance" ) {
  allplot.type <- "fraction" 
  base.type <- "normgenes"
  tit <- "Gene Fraction"
  sub <- "Relative of normalized readcounts summed up for each gene compared to all reads of the dataset."
  xlab <- "Dataset"
  ylab <- "Gene"}
# genereadcount -> allplot="readcount", base="normgenes"
if( info$show == "genereadcount" ) {
  allplot.type <- "readcount" 
  base.type <- "normgenes"
  tit <- "Gene Readcount"
  sub <- "Normalized readcounts summed up for each gene."
  xlab <- "Dataset"
  ylab <- "Gene"}
# genethreshold -> allplot="threshold", base="normgenes"
if( info$show == "genethreshold" ) {
  allplot.type <- "threshold" 
  base.type <- "normgenes"
  tit <- "Gene Readcount"
  sub <- "Percentage of sgRNA for a given gene having a readcount wihtin the set threshold."
  xlab <- "Dataset"
  ylab <- "Gene"}
# sgrnaabundance -> allplot="fraction", base="normsgrna"
if( info$show == "sgrnaabundance" ) {
  allplot.type <- "fraction" 
  base.type <- "normsgrna"
  tit <- "sgRNA Abundance"
  sub <- "Relative normalized readcounts of all sgRNAs compared to the dataset"
  xlab <- "Dataset"
  ylab <- "sgRNA"}
# sgrnareadcount -> allplot="readcount", base="normsgrna"
if( info$show == "sgrnareadcount" ) {
  allplot.type <- "readcount" 
  base.type <- "normsgrna"
  tit <- "Normalized readcounts of all sgRNAs."
  sub <- "sgRNA readcounts"
  xlab <- "Dataset"
  ylab <- "sgRNA"}
if(bLog) sub <- paste(sub, "Log10 of values is shown.")


if( info$show == "unmapped" ){
  write(paste(userID, ": executing unmapped.genes"), logFile, append = TRUE)
  df <- tryFunction(unmapped.genes(type = "all", dataframe = TRUE))
  ds <- df$x
  tit <- "Missing sgRNAs"
  sub <- "Percentage of missing sgRNAs per Gene"
  xlab <- "Dataset"
  ylab <- "Gene"
} else {
  write(paste(userID, ": executing gene.heatmap"), logFile, append = TRUE)
  df <- tryFunction(gene.heatmap(cp$miaccs$g.extractpattern, allplot = allplot.type, base = base.type, threshold = thresh, 
                    type = info$type, write = FALSE, cluster = FALSE, centers = 10, 
                    dataframe = TRUE, log = bLog ))
}




##########################
#### Hier. Clustering ####
##########################
### spread data.frame
df$x <- NULL
df$y <- NULL
df <- tidyr::spread(df, Var1, value)
rownames(df) <- df$Var2
df$Var2 <- NULL
### UPGMA: euklid, average-link
m <- as.matrix(df)
m <- m[cluster::agnes(m)$order, ]















##############
#### Plot ####
##############
write(paste(userID, ": creating plot object"), logFile, append = TRUE)
heatmap <- function( object, label = FALSE, showInLegend = FALSE, tit = NULL, sub = NULL, 
                     xlab = "Dataset", ylab = "Gene", ex = TRUE, show = "1" ) 
{
  stopifnot(is.numeric(object))
  
  df <- as.data.frame(object)
  
  ismatrix <- is.null(colnames(object)) & is.null(rownames(object))
  pos <- ifelse(ismatrix, 0, 1)
  
  xnm <- if (is.null(colnames(object))) 1:ncol(object) else colnames(object)
  xnm <- as.character(xnm)
  xid <- seq(length(xnm)) - pos
  
  ynm <- if (is.null(rownames(object))) 1:nrow(object) else rownames(object)
  ynm <- as.character(ynm)
  yid <- seq(length(ynm)) - pos
  
  ds <- as.data.frame(df) %>% 
    dplyr::tbl_df() %>% 
    dplyr::bind_cols(tibble::data_frame(ynm), .)  %>% 
    tidyr::gather("key", "value", -ynm) %>% 
    dplyr::rename_("xnm" = "key") %>% 
    dplyr::mutate_("xnm" = "as.character(xnm)",
            "ynm" = "as.character(ynm)")
  
  ds$xnm <- if (is.null(colnames(object))) stringr::str_replace(ds$xnm, "V", "") else ds$xnm
  
  ds <- ds %>% 
    dplyr::left_join(tibble::data_frame(xnm, xid), by = "xnm") %>%
    dplyr::left_join(tibble::data_frame(ynm, yid), by = "ynm") %>% 
    dplyr::mutate_("name" = "paste(ynm, xnm, sep = ' in sample ')") %>% 
    dplyr::select_("x" = "xid", "y" = "yid", "value", "name")
  
  fntltp <- JS("function(){
               return this.point.name + ': ' +
               Highcharts.numberFormat(this.point.value, 2)
}")
  
  hc <- highchart() %>% 
    hc_add_series_df(data = ds, type = "heatmap") %>% 
    hc_plotOptions(
      series = list(
        showInLegend = showInLegend,
        boderWidth = 0,
        dataLabels = list(enabled = label)
      )
    ) %>% 
    hc_tooltip(formatter = fntltp) %>% 
    hc_legend(enabled = TRUE) %>% 
    hc_colorAxis(auxarg = TRUE)
  
  if (ismatrix) {
    hc <- hc %>%
      hc_xAxis(visible = FALSE) %>% 
      hc_yAxis(visible = FALSE, reversed = TRUE)
    
  } else {
    hc <- hc %>% 
      hc_xAxis(categories = xnm, title = list(text = ""), opposite = TRUE) %>% 
      hc_yAxis(categories = ynm, title = list(text = ""), reversed = TRUE)
  }
  
  hc <- hc %>%
    hc_chart(zoomType = "y") %>%
    hc_title(text = tit) %>%
    hc_subtitle(text = sub) %>%
    hc_xAxis(showempty = TRUE, title = list(text = xlab)) %>%
    hc_yAxis(showempty = FALSE, showFirstLabel = TRUE, showLastLabel = TRUE,
             title = list(text = ylab)) %>%
    hc_colorAxis(stops = color_stops(2, c("#FFFFFF", "#4285F4"))) %>%
    hc_legend(enabled = TRUE, align = "right", horizontalAlign = "right") %>%
    hc_exporting(enabled = ex, printMaxWidth = 2000, scale=8,
                 filename = paste("QualityControl_Heatmap_",show, sep=""))
  
  hc  
}
df <- heatmap(m, tit = tit, sub = sub, xlab = xlab, ylab = ylab, show = info$show)






##############
#### save ####
##############
# save rds file
# set progress to 1 and save info file as termination signal
write(paste(userID, ": writing heatmap.rds"), logFile, append = TRUE)
saveRDS(df, file = file.path(userDir, "heatmap.rds"))

outInfo <- c(paste("progress", 1, sep = ";"), paste("info", "", sep = ";"))
write(outInfo, file.path(userDir, "heatmap.info"))

write(paste(userID, ": heatmap.r quit at", Sys.time()), logFile, append = TRUE)
quit(save = "n", status = 0)





























 
