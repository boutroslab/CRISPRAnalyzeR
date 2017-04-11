# sourced by 'server.r'
# save as 'functions_server.r'
# functions


####

# Set multicore
#BiocParallel::MulticoreParam(workers = 4)



#' GetCaptures
#'
#' Split a chr string by capture groups according to a regular expression provided.
#'
#' The string is returned as a matrix of start and stop position of substrings.
#' These substrings are labelled by which capture group they belong to.
#' Substrings outside a capture are labelled as 0.
#'
#' Nested capturing groups do not work.
#'
#' @param s    chr string
#' @param pat  chr string containing a regular expression with at least 1 capture '(', ')'
#'
#' @return  Num matrix indexing substrings of s. Each columns represents a substring.
#'          Row 1 is the label, Row 2 is the start, row 3 the stop letter of each substring.
#'          Substrings which are part of a capture are labelled with increasing numbers as they appear in s beginning with 1.
#'          Substrings which are not part of a capture are labelled with 0.
#'          If nothing was captured in chr s, NULL is returned.
#'
#' @examples
#' s <- ">ENSG00000139083_8_61387.34117"
#' pat <- "^.*(SG.+?)1.*_(.+?)_.*$"
#'
#' m <- GetCaptures(s, pat)
#' apply(m, 2, function(x) substr(s, x[2], x[3]))
#'
#' @export
#'
GetCaptures <- function(s, pat){
  cs <- regexec(pat, s)[[1]]
  if( cs[1] == -1 ) return(NULL)
  m <- sapply(2:length(cs), function(x) matrix(c(x - 1, cs[x], cs[x] + attr(cs, "match.length")[x] - 1), ncol = 1))
  if( any(is.na(m[, 1])) ) stop("No captures found in regular expression.")
  bl <- 1:nchar(s) %in% if(ncol(m) > 1) unlist(apply(m, 2, function(x) x[2]:x[3])) else m[2, 1] : m[3, 1]
  
  gap <- FALSE
  o <- NULL
  for( n in 1:length(bl) ){
    if( !bl[n] ){
      if( gap ) o[2, ncol(o)] <- n else {gap <- TRUE; o <- cbind(o, c(n, n))}
    } else gap <- FALSE
  }
  if(!is.null(o)) o <- rbind(rep(0, ncol(o)), o)
  
  out <- sapply(sort(c(o[2, ], m[2, ])), function(x) if(x %in% o[2, ]) o[, o[2, ] == x] else m[, m[2, ] == x])
  rownames(out) <- c("capture", "start", "stop")
  out
}





################################
#### Check sequencing Files ####
################################
#
# look at uploaded sequencing files, check whether they are ok and return error message and status
# arguments     names       chr array of actual file names
#               paths       chr array of file datapaths
#               gen_names   chr array of file names given by user 
#               regex       chr regular expression as provided by user
# value   list of 2 elements error, message
#         error   boolean, were there issues?
#         message error message in case there were issues
#


Check_seqFiles <- function(names, paths, gen_names, regex, messages = config$messages,  threads = as.numeric(config$car.bt2.threads), userdir = userDir, ID = userID) {
  out <- list(error = FALSE, message = "")

  withProgress(value = 0.1, message = messages$checkseqfilesprogress1$String, {
  # at least 2 files must be uploaded
  if( length(names) < 2 ){
    out$error <- TRUE
    out$message <- paste(out$message, messages$checkseqfiles1$String)  
 # out$message <- paste0(out$message, "You will compare 2 conditions, so we expect at 
 #                         least 2 sequencing files.<br/>")
  }
  
  # no empty name entries
  if( any(names == "") ){
    out$error <- TRUE
    out$message <- paste0(out$message,messages$checkseqfiles2$String) 
    #out$message <- paste0(out$message, "There is an empty file name.<br/>")
  }
  
  
  # no dublicates in names
  if( anyDuplicated(names) ){
    out$error <- TRUE
    out$message <- paste0(out$message,messages$checkseqfiles3$String) 
    #out$message <- paste0(out$message, "Please use unique files.<br/>")
  }
  
  # # nothing but characters, "_", and numbers
  # if( grepl("[^a-zA-Z0-9_]", paste0(names, collapse = ""), perl = TRUE) ){
  #   out$error <- TRUE
  #   out$message <- paste0(out$message,messages$checkseqfiles4$String) 
  #   #out$message <- paste0(out$message, "Please don't use characters like *, # or % in your files as those are used for data processing.<br/>")
  # }
  
  # if fastq, it must be gzipped
  if( any(grepl(".*(\\.fastq)$", tolower(names), perl = TRUE),grepl(".*(\\.fq)$", tolower(names), perl = TRUE)) ){
    out$error <- TRUE
    out$message <- paste0(out$message,messages$checkseqfiles5$String) 
    #out$message <- paste0(out$message, "Please only upload compressed FASTQ files. Compression is usually done using the gzip command. <br/> By default, your sequencing company should provide you with already compressed files (*.fastq.gz).<br/>")
  }
  
  # no empty gen_name entries
  if( any(gen_names == "") ){
    out$error <- TRUE
    out$message <- paste0(out$message,messages$checkseqfiles6$String) 
    #out$message <- paste0(out$message, "There is an empty file name.<br/>")
  }
  
  # no dublicates in gen_names
  if( anyDuplicated(gen_names) ){
    out$error <- TRUE
    out$message <- paste0(out$message,messages$checkseqfiles7$String) 
    #out$message <- paste0(out$message, "Please use unique file names.<br/>")
  }
  
  # no nothing but numbers, characters, or "_"
  if( grepl("[^a-zA-Z0-9_-]", paste0(gen_names, collapse = ""), perl = TRUE) ){
    out$error <- TRUE
    out$message <- paste0(out$message,messages$checkseqfiles8$String) 
    #out$message <- paste0(out$message, "Please don't use characters \\, /, *, #, +, or % in your file names.<br/>")
  }
  
  # no dublicates in paths
  if( anyDuplicated(paths) ){
    out$error <- TRUE
    out$message <- paste0(out$message,messages$checkseqfiles9$String) 
    #out$message <- paste0(out$message, "Honestly, I don't even know how this happened, but 2 datapaths are the same.<br/>")
  }
  
  
  # gen_names must start with a letter
  # if( grepl("^[^a-zA-Z]", paste0(gen_names, collapse = ""), perl = TRUE) ){
  #   out$error <- TRUE
  #   out$message <- paste0(out$message, "Filenames must start with a character [a-zA-Z].<br/>")
  # }
  
  # Set progress
  setProgress(value = 0.2, detail = messages$checkseqfilesprogress2$String)
   
  out$extractRatio <- as.list(names)
  names(out$extractRatio) <- gen_names
  
  # Check for FASTQ files IF meaningfull data is retrieved
   if(any(grepl(".*(\\.fastq.gz|\\.fq.gz|\\.gz)$", tolower(names), perl = TRUE)) ){
      
     setProgress(value = 0.3, detail = messages$checkseqfilesprogress3$String)
     
     regexCheck <- paste(".*", regex, ".*", sep="") # Add captures for the rest of the sequence

     checkpath <- list(paths)
     for(i in 1:length(names))
     {
       #print(names[i])
       
       #print(grepl(".*(\\.fastq.gz|\\.fq.gz|\\.gz)$", tolower(names[i]), perl = TRUE))
       if(grepl(".*(\\.fastq.gz|\\.fq.gz|\\.gz)$", tolower(names[i]), perl = TRUE) ) {
         checkpath[i] <- list(paths[i])
       } else {
         checkpath[i] <- list(NA)
       }
     }
     
       # return value: vector with ratio of false/true working regular expression captures
       
         out$extractRatio <- list()
         setProgress(value = 0.4,detail = messages$checkseqfilesprogress4$String)
         
         
      # BiocParallel FIX
         options(bphost="localhost")
         #BiocParallel::MulticoreParam(workers = as.numeric(threads),log = TRUE, logdir = userdir, jobname = as.character(ID))
         param <- BiocParallel::SnowParam(workers = as.numeric(threads), type = "SOCK",log = FALSE)
         #param <- BiocParallel::MulticoreParam(workers = 2, type = "FORK",log = TRUE, logdir = userdir, jobname = as.character(ID))
         
         
         #BiocParallel::bptry(BiocParallel::bplapply(checkpath, function(x) {
         out$extractRatio <- BiocParallel::bplapply(checkpath, function(x) {
           
           if(!is.na(x))
           {
             con <- file(x)
             
             f <- ""
             f <- ShortRead::FastqSampler(con = con, n = 10000, ordered = TRUE)
             
             
             f <- ShortRead::yield(f) # Get the data
             
             f <- ShortRead::sread(f) # Get sequences as DNAStringSet
             
             
             #names(f) <- as.character(seq(from = 1, to = length(f), by = 1))
             f <- as.character(f)
             
             
             # do grep to check for working regular expression
             #sub(regexCheck, replacement = "\\1", x = f, perl = FALSE, fixed=FALSE)
             test <- grepl(regex, f, perl = FALSE)
             
             countTrue <- (length(test[test == TRUE]))+0.01
             countFalse <- length(test[test != TRUE])

             
             # Check ratio of working Regular Expression, should be LOWER than 30% of reads
             extractRatio <- round(as.numeric(( 1- (countFalse / 10000 ))*100 ), digits=4)
             #close(con)
             
           } else { extractRatio <- NA}
           

           
           return(extractRatio)
         }, BPPARAM = param)
         
         
         
         if(any(out$extractRatio < 30, na.rm = TRUE)) # give out an error if ratio is bad, this will be stored as additional information and displayed in FASTQ Quality and DATA Review
         {
           
           out$error <- TRUE
           out$message <- paste0(out$message,messages$checkseqfiles10$String," ", min(unlist(out$extractRatio), na.rm = TRUE),  " to " , max(unlist(out$extractRatio), na.rm = TRUE), messages$checkseqfiles11$String, ".", "</br>", "<strong>Ratios: </br>", paste(out$extractRatio, collapse = " %</br>"), "</strong>", sep=" ") 
           #out$message <- paste0(out$message, paste("The entered FASTQ regular exression did only work in approximately", max(unlist(out$extractRatio), na.rm = TRUE)*100, "% of sequences within your FASTQ files.<br/>This is an indication for a wrong FASTQ Regular Expression or a bad quality of your sequencing library."))
           
         }
       
       
     #print(checkpath)
     
  }

  setProgress(value = 0.6, detail = messages$checkseqfilesprogress5$String)
  
  
  #print("readcounts")
  # Check for readcount files if they are readcount files
  for(i in 1:length(names))
  {
    if( all(!grepl(".*(\\.fastq.gz)$", tolower(names[i]), perl = TRUE), !grepl(".*(\\.fq.gz)$", tolower(names[i]), perl = TRUE), !grepl(".*(\\.gz)$", tolower(names[i]), perl = TRUE)) )
    {
      
        con <- file(paths[i])
        top <- readLines(con)
        close(con)
        
        # Readcount file, load a couple of lines and do grep if ANY line is not like a typical readcount
        if( any(!grepl("^.*\t[0-9]*$", top[-1], perl = TRUE)) ) 
        {
          out$error <- TRUE
          out$message <- paste(out$message, paste(messages$checkseqfiles12$String, names[i] ,messages$checkseqfiles13$String, sep=" ", collapse = ""))
        }
        if( any(!grepl("^.*?\t[\\w\\d]+$", top, perl = TRUE)) ) 
        {
          out$error <- TRUE
          out$message <- paste(out$message, paste(messages$checkseqfiles12$String, names[i] ,messages$checkseqfiles14$String, sep=" ", collapse = ""))
        }
      }

    
    
  }
  setProgress(value = 1)
  }) # end of WITHPROGRESS
  
  # make extractratio matching file length for data frames
  
  for(i in 1:length(out$extractRatio))
  {
    if(is.null(out$extractRatio[[i]]))
    {
      out$extractRatio[[i]] <- NA
    } 
  }
  
  out$extractRatio <- as.vector(out$extractRatio)
  
  return(out)
}








#############################
#### Check sgRNA library ####
#############################
#
# look at uploaded sgRNA library file, check whether it's ok and return error message and status
# arguments     name       chr array of actual file name
#               path       chr array of file datapath
# value   list of 2 elements error, message
#         error   boolean, were there issues?
#         message error message in case there were issues
#
Check_libFile <- function(name, path, regex, messages = config$messages) {
  out <- list(error = FALSE, message = "")
  
  withProgress(value=0.1, message = messages$checklibfileprogress1$String, {
  # no empty name entry
  if( name == "" ){
    out$error <- TRUE
    out$message <- paste0(out$message,messages$checklibfile1$String) 
    #out$message <- paste0(out$message, "There is an empty file name.<br/>")
  }
  
  setProgress(value=0.2, detail = messages$checklibfileprogress2$String)
  
  # no ( ) * # % ~ in name
  if( grepl("[*,#%~()]", paste0(name, collapse = ""), perl = TRUE) ){
    out$error <- TRUE
    out$message <- paste0(out$message,messages$checklibfile2$String) 
    #out$message <- paste0(out$message, "Don't use special characters like *, # or % in your file name<br/>")
  }
  
  # must be fasta format
  if( !grepl(".*\\.fasta$", tolower(name), perl = TRUE) ){
    out$error <- TRUE
    out$message <- paste0(out$message,messages$checklibfile3$String) 
    #out$message <- paste0(out$message, "Please upload a fasta file (*.fasta).<br/>")
  }
  
  # no empty regex
  if( is.null(regex) || is.na(regex) || length(regex) == 0 || regex == "" ){
    out$error <- TRUE
    out$message <- paste0(out$message,messages$checklibfile4$String) 
    #out$message <- paste0(out$message, "Provide a regular expression for gene extraction.<br/>")
  }
  
  setProgress(value=0.3, detail = messages$checklibfileprogress3$String)
  # check if regex has two () and only go further with checks if this is ok
    if( sum(grepl(".*\\(.+\\).*\\(.+\\).*", as.character(regex), perl = TRUE)) == 0 ){
      out$error <- TRUE
      out$message <- paste0(out$message, paste( messages$checklibfile5$String,regex ,messages$checklibfile6$String, sep=" "))
      #out$message <- paste0(out$message, paste( "The provided regex",regex ," does not contain two capturing groups () to catch the gene as well as the sgRNA identifier part.<br/>Please see the little help symbol next to the select file button.<br/>", sep=" "))
    }
  else # regex has two capturing groups!
  {
    # check whether regex works in file
    con <- file(path)
    top <- readLines(con) # , n = 10
    close(con)
    if( sum(grepl(regex, top, perl = TRUE)) == 0 ){
      out$error <- TRUE
      out$message <- paste0(out$message, paste( messages$checklibfile7$String,regex ,messages$checklibfile8$String, sep=" "))
      #out$message <- paste0(out$message, paste( "The provided regex",regex ," doesn't work with this file.<br/>", sep=" "))
    }
    
    setProgress(value = 0.4)
    
    #### Check if we can extract the correct gene identifier and everything is unique
    # Load fasta file
    check.fasta <- try(seqinr::read.fasta(file=path, seqtype = "DNA", as.string = TRUE, forceDNAtolower = FALSE,set.attributes = TRUE, legacy.mode = TRUE, seqonly = FALSE, strip.desc = FALSE, bfa = FALSE, apply.mask = TRUE))
    # Extract gene identifier
    check.fasta.names <- try(seqinr::getName(object = check.fasta))
    
    if(class(check.fasta) == "try-error")
    {
      out$error <- TRUE
      out$message <- paste0(out$message, "Please check your sgRNA libary FASTA file. It looks like the file is not in FASTA format.") 
    }
    
    if(class(check.fasta.names) == "try-error")
    {
      out$error <- TRUE
      out$message <- paste0(out$message, "Please check your sgRNA library FASTA file. It looks like the file is not in FASTA format.") 
    }
    
    # check for empty
    if( "" %in% check.fasta.names)
    {
      out$error <- TRUE
      out$message <- paste0(out$message,messages$checklibfile9$String) 
      #out$message <- paste0(out$message, "You have empty lines in your sgRNA library FASTA file.<br/>")
    }
    
    # check for uniqueness
    if(length(check.fasta.names) != length(unique(check.fasta.names)))
    {
      out$error <- TRUE
      out$message <- paste0(out$message,messages$checklibfile10$String) 
      #out$message <- paste0(out$message, "The sgRNA identifiers in the provided sgRNA library file are not unique.<br/> Please make sure only unique identifiers are used.<br/>")
    }
    # Check for regular expression
    check.fasta.names.regex.gene <- sub(pattern = regex,x = check.fasta.names, replacement = "\\1")
    # make available for all
    libfile_gene <<- check.fasta.names.regex.gene
    
    check.fasta.names.regex.sgrna <- sub(pattern = regex,x = check.fasta.names, replacement = "\\2")
    
    setProgress(value = 0.6)
    
    # check for unique identifier
    if(length(check.fasta.names.regex.sgrna) != length(unique(check.fasta.names.regex.sgrna)))
    {
      out$error <- TRUE
      out$message <- paste0(out$message, messages$checklibfile15$String) 
      #out$message <- paste0(out$message, "The regular expression revealed a problem with your sgRNA identifier.<br/>Either the selected regular expression is not correct or you have missing/incorrect entries in your sgRNA library file.<br/>Please make sure every sgRNA identifier consists of a gene identifier and a unique sgRNA identifier part.<br/>")
      
    }

    if(length(check.fasta.names.regex.sgrna) != length(check.fasta.names.regex.gene))
    {
      out$error <- TRUE
      out$message <- paste0(out$message, messages$checklibfile11$String) 
      #out$message <- paste0(out$message, "The regular expression revealed a problem with your sgRNA identifier.<br/>Either the selected regular expression is not correct or you have missing/incorrect entries in your sgRNA library file.<br/>Please make sure every sgRNA identifier consists of a gene identifier and a unique sgRNA identifier part.<br/>")
      
    }
    sapply(check.fasta.names.regex.sgrna, function(y) {
      if(as.character(y) == "" || is.null(y) || is.na(y))
      {
        out$error <- TRUE
        out$message <- paste0(out$message,messages$checklibfile12$String) 
        #out$message <- paste0(out$message, "The regular expression revealed a problem with your sgRNA identifier.<br/>Either the selected regular expression is not correct or you have missing/incorrect entries in your sgRNA library file.<br/>")
      }
    })
    sapply(check.fasta.names.regex.gene, function(y) {
      if(as.character(y) == "" || is.null(y) || is.na(y))
      {
        out$error <- TRUE
        out$message <- paste0(out$message,messages$checklibfile13$String) 
        #out$message <- paste0(out$message, "The regular expression revealed a problem with your gene identifier as part of your sgRNA identifier.<br/>Either the selected regular expression is not correct or you have missing/incorrect entries in your sgRNA library file.<br/>")
      }
    })
    setProgress(value = 0.9)
    if(any(check.fasta.names.regex.gene %in% check.fasta.names.regex.sgrna))
    {
      files.error <- check.fasta.names.regex.sgrna[check.fasta.names.regex.gene %in% check.fasta.names.regex.sgrna]
      
      out$error <- TRUE
      out$message <- paste0(out$message, messages$checklibfile14$String , files.error[1], sep= "</br>")
      #out$message <- paste0(out$message, "The regular expression revealed a problem with your gene/sgRNA identifier as part of your sgRNA identifier.<br/>Either the selected regular expression cannot discriminate between the gene and sgRNA part of your identifier<br/> or you have missing/incorrect entries in your sgRNA library file.<br/> As an example, this was found to be the case for sgRNA ", files.error[1], ".<br/>")
      
    }
  }
  setProgress(value = 1)
  })# End of withProgress
  
  return(out)
}








###########################
#### Check Group Names ####
###########################
#
# look at group names, check whether they are ok and return error message and status
# arguments     name       chr array of group names
# value   list of 2 elements error, message
#         error   boolean, were there issues?
#         message error message in case there were issues
#
Check_groupNames <- function(names, messages = config$messages) {
  out <- list(error = FALSE, message = "")
  
  # at least 2 Groups
  if( length(names) < 2 ){
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkgroupnames1$String)    
    #out$message <- paste0(out$message, "For a comparison at least 2 groups are needed.<br/>")    
  }
  
  # no duplicated names
  if( anyDuplicated(names) ){
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkgroupnames2$String)  
    #out$message <- paste0(out$message, "Each group should have a unique name.<br/>")
  }
  
  # no empty name entry
  if( any(names == "") ){
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkgroupnames3$String)  
    #out$message <- paste0(out$message, "There is a group without a name.<br/>")
  }
  
  # no ( ) * # % ~ in name
  if( grepl("[*,#%~()]", paste0(names, collapse = ""), perl = TRUE) ){
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkgroupnames4$String)
    #out$message <- paste0(out$message, "Please don't use weird characters like *, # or % in your group name<br/>")
  }
  
  # no space in name
  if( grepl(" ", paste0(names, collapse = "")) ){
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkgroupnames5$String)
    #out$message <- paste0(out$message, "Please use no spaces in your group names. Use '_' instead<br/>")
  }
  
  return(out)
}








######################
#### Check Groups ####
######################
#
# look at groups connected with files and test
# arguments     a   list with named chr arrays. names are group names, chr arrays are allocated file names
# value   list of 2 elements error, message
#         error   boolean, were there issues?
#         message error message in case there were issues
#
Check_groups <- function(a, n, messages = config$messages) {
  out <- list(error = FALSE, message = "")
  
  # groups without any files allocated
  if( any(is.null(a)) || any(is.na(a)) || any(a == "") || any(lapply(a, length) == 0) ){
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkgroups1$String)
    #out$message <- paste0(out$message, "Some groups don't have any files associated with them.<br/>")
  }
  if( length(a) != n ){
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkgroups2$String)
    #out$message <- paste0(out$message, "Some groups don't have any files associated with them.<br/>")
  }
  # check if the same file is used for two groups
  groupelements <- as.vector(unlist(a))
  if(length(unique(groupelements)) != length(groupelements))
  {
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkgroups3$String)
  }
  return(out)
}








#######################
#### Check Extract ####
#######################
#
# if additional extraction is needed for sequencing files (fastq), check whether these infos are valid
# arguments   tar       string with regex for target sgRNA ID
#             mach      string with regex for machine ID
#             boolean   string whether extraction is needed at all ("n", "y")
# value   list of 2 elements error, message
#         error   boolean, were there issues?
#         message error message in case there were issues
#
Check_extract <- function(tar, boolean, messages = config$messages) {
  out <- list(error = FALSE, message = "")
  
  # if no don't continue
  if( boolean == FALSE ){
    return(out)
  }
  
  # target regex is empty
  if( is.null(tar) || is.na(tar) || tar == "" || length(tar) == 0 ){
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkextract$String)
    #out$message <- paste0(out$message, "Provide a regular expression for target sequence extraction.<br/>")
  }

  return(out)
}








###############################
#### Check Extracted Files ####
###############################
# if fastq file is extracted, check whether extraction went right
# arguments   names     chr arr of sequencing file names
#             paths     chr arr of sequencing file paths
#             gen_names chr arr of sequencinf file names given by user
#             info      chr of info message returned by fastq_extraction.r
#             ID        chr of user ID
# value   list of 2 elements error, message
#         error   boolean, were there issues?
#         message error message in case there were issues
Check_extractedFiles <- function( names, paths, gen_names, info, ID , messages = config$messages) {
  out <- list(error = FALSE, message = "")
  
  # add info if error
  if( length(info) ){
    out$error <- TRUE
    out$message <- paste0(out$message, info,
                          messages$checkextractedfiles$String
    )
    # out$message <- paste0(out$message, info,
    #     "For further assistence please briefly describe your problem in the input form on our help page.<br>"
    # )
  }
  
  return(out)
}








###########################
#### Check Annotations ####
###########################
#
# look at entries made for gene annotations and test
# arguments 
# value   list of 2 elements error, message
#         error   boolean, were there issues?
#         message error message in case there were issues
#
Check_annos <- function(dataset, ID, IDnew) {
  out <- list(error = FALSE, message = "")
  
  # no annotations selected even though it was checked
  #if( anno == TRUE && (is.null(attribs) || is.na(attribs) || length(attribs) == 0) ){
  #  out$error <- TRUE
  #  out$message <- paste0(out$message, "You have chosen to annotate your gene hits, 
  #    but you did not specify any annotation options.<br/>")
  #}
  
  return(out)
}








###########################
#### Check Comparisons ####
###########################
#
# look at entries for group comparison and positive and negative controls
# arguments   group1    chr string of group 1
#             group2    chr string of group 2
#             pos       chr array of gene ID(s) for positive control
#             neg       chr array of gene ID(s) for negative control
# value   list of 2 elements error, message
#         error   boolean, were there issues?
#         message error message in case there were issues
#
Check_compare <- function(group1, group2, pos, neg, top, messages = config$messages) {
  out <- list(error = FALSE, message = "")

  # check whether pos Ctrl is found in sgRNA library
  if( !any(is.null(pos)) && !any(is.na(pos)) && !any(length(pos) == 0) && !any(pos == "") && !is.null(top) ){
    for( i in 1:length(pos) ){
      pattern <- paste("^",pos[i],"$" , sep="")
      if( !any(grepl(pattern, top, fixed = FALSE, perl = TRUE)) ){
        out$error <- TRUE
        out$message <- paste0(out$message, paste(messages$checkcompare1$String, pos[i],messages$checkcompare2$String,"<br/>"))
        #out$message <- paste0(out$message, paste("Positive control", pos[i], "was not found in sgRNA library.","<br/>"))
      }
    }
  }
  
  # check whether non-targeting Ctrl is found in sgRNA library
  if( !any(is.null(neg)) && !any(is.na(neg)) && !any(length(neg) == 0) && !any(neg == "") && !is.null(top) ){
    for( i in 1:length(neg) ){
      pattern <- paste("^",neg[i],"$" , sep="")
      if( !any(grepl(pattern, top, fixed = FALSE, perl=TRUE)) ){
        out$error <- TRUE
        out$message <- paste0(out$message, paste(messages$checkcompare3$String, neg[i],messages$checkcompare2$String,"<br/>"))
        #out$message <- paste0(out$message, paste("Non-targeting control", neg[i], "was not found in sgRNA library.<br/>"))
      }
    }
  }  
  
  # there should be non-target Ctrl (comment out of not necessary!)
  #if( any(is.null(neg)) || any(is.na(neg)) || any(length(neg) == 0) || any(neg == "") ){
  #  out$error <- TRUE
  #  out$message <- paste0(out$message, paste("You didn't specify any non-targeting Ctrl.<br/>"))
  #}
  
  # there should be pos Ctrl (comment out of not necessary!)
  #if( any(is.null(pos)) || any(is.na(pos)) || any(length(pos) == 0) || any(pos == "") ){
  #  out$error <- TRUE
  #  out$message <- paste0(out$message, paste("You didn't specify any positive Ctrl.<br/>"))
  #}
  
  return(out)
}








#################################
#### Check Analysis Settings ####
#################################
#
# look at entries for group comparison and positive and negative controls
# arguments   group1    chr string of group 1
#             group2    chr string of group 2
#             pos       chr array of gene ID(s) for positive control
#             neg       chr array of gene ID(s) for negative control
# value   list of 2 elements error, message
#         error   boolean, were there issues?
#         message error message in case there were issues
#
Check_analysisSettings <- function (wilcoxPval, wilcoxRand, deseq2Pval, mageckPval, sgrseaPval, edgerPval, removeLow, removeHigh, removeThresholdLow, removeThresholdHigh, bagel_lower, bagel_higher, screenbeam_iterations, screenbeam_burnin, screenbeam_run, screenbeam_pval, messages = config$messages){
  out <- list(error = FALSE, message = "")
  
  # pvals should be between 0 and 1
  if( any(is.na(c(wilcoxPval, deseq2Pval, mageckPval, sgrseaPval, edgerPval, bagel_lower, bagel_higher, screenbeam_iterations, screenbeam_burnin, screenbeam_run,screenbeam_pval)) ) ||
      any(c(wilcoxPval, deseq2Pval, mageckPval, sgrseaPval, edgerPval, screenbeam_pval ) < 0) ||
      any(c(wilcoxPval, deseq2Pval, mageckPval, sgrseaPval, edgerPval, screenbeam_pval) > 1) ){
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkanalysissettings1$String)
    #out$message <- paste0(out$message, "There are some P values < 0 or > 1. That doesn't really make sense.<br/>")
  }
  
  # thesholds
  if((removeLow)  && is.null(removeThresholdLow) )
  {
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkanalysissettings2$String)
    #out$message <- paste0(out$message, "Please enter a sgRNA read count Threshold.<br/>")
  }
  if((removeHigh)  && is.null(removeThresholdHigh) )
  {
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkanalysissettings2$String)
    #out$message <- paste0(out$message, "Please enter a sgRNA read count Threshold.<br/>")
  }
  if(removeLow)
  {
    if(removeThresholdLow < 0) {
      out$error <- TRUE
      out$message <- paste0(out$message, messages$checkanalysissettings3$String)
      #out$message <- paste0(out$message, "Please use positive numbers only.<br/>")
    }
    if(is.na(as.numeric(removeThresholdLow)) )
    {
      out$error <- TRUE
      out$message <- paste0(out$message, messages$checkanalysissettings4$String)
      #out$message <- paste0(out$message, "Please use numbers for sgRNA read count thresholds only.<br/>")
    }
    
  }
  if(removeHigh)
  {
    if(removeThresholdHigh < 0) {
      out$error <- TRUE
      out$message <- paste0(out$message, messages$checkanalysissettings3$String)
      #out$message <- paste0(out$message, "Please use positive numbers only.<br/>")
    }
    if(is.na(as.numeric(removeThresholdHigh)) )
    {
      out$error <- TRUE
      out$message <- paste0(out$message, messages$checkanalysissettings4$String)
      #out$message <- paste0(out$message, "Please use numbers for sgRNA read count thresholds only.<br/>")
    }
    
  }

  return(out)
}








#####################
#### Check Final ####
#####################
#
# look at all status values, give appropriate error message if one is FALSE
# arguments   all entries for reactive value status. they are all booleans
# value   list of 2 elements error, message
#         error   boolean, were there issues?
#         message error message in case there were issues
#
Check_final <- function(seqFiles, libFile, extract, groups, anno, compare, analysis, extractedFiles,  messages = config$messages) {
  out <- list(error = FALSE, message = "")
  
  # something wrong in data tab
  if( seqFiles == FALSE || libFile == FALSE || extract == FALSE ){
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkfinal1$String)
    #out$message <- paste0(out$message, "Files were not sucessfully uploaded yet. Please go back to 'Data' to do this.<br/>")
  } 

  # something wrong in data tab
  if( (seqFiles == TRUE && libFile == TRUE && extract == TRUE) && extractedFiles == FALSE ){
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkfinal2$String)
    #out$message <- paste0(out$message, "The extraction of your FASTQ files hasn't finished yet. You can see the progress in the upper reight menu.<br/>")
  } 
  
  # something wrong in setup tab
  if( groups == FALSE || anno == FALSE ){
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkfinal3$String)
    #out$message <- paste0(out$message, "Something in the 'Setup' tab is not entered yet. Please correct that first.<br/>")
  }
  
  # something wrong in screening tab
  if( compare == FALSE || analysis == FALSE ){
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkfinal4$String)
    #out$message <- paste0(out$message, "Something in this tab is not entered correctly yet. Please correct it first.<br/>")
  }
  
  return(out)
}








#######################
#### Check Results ####
#######################
# results can be checked in analysis.r
# if something is wrong it is written in info file
# arguments   info  chr value with error occured in batch script
#             ID    chr of user ID
# value   list of 2: error and message
#         error   boolean, were there issues?
#         message error message in case there were issues
Check_results <- function ( info, ID , messages = config$messages){
  out <- list(error = FALSE, message = "")
  
  # add info if error
  if( length(info) ){
    out$error <- TRUE
     out$message <- paste0(out$message, info,
        messages$checkresults$String
    )
  } 
  
  return(out)
}








#######################
#### Check Heatmap ####
#######################
#
# look at info of heatmap.r and see whether errors occured
# arguments   info  chr value with error occured in batch script
# value   list of 2: error and message
#         error   boolean, were there issues?
#         message error message in case there were issues
#
Check_heatmap <- function ( info , messages = config$messages){
  out <- list(error = FALSE, message = "")
  
  # add info if error
  if( length(info) ){
    out$error <- TRUE
    out$message <- paste0(out$message, info, messages$checkheatmap$String)
    #out$message <- paste0(out$message, info, "Is there something wrong with the results?<br/>")
  }
  
  return(out)
}








####################
#### Check Info ####
####################
# test results of get_info.r by looking at get_info.info file
# arguments   info  chr value with error occured in batch script
#             ID    chr of user ID
# value   list of 2: error and message
#         error   boolean, were there issues?
#         message error message in case there were issues
#
Check_info <- function ( info, ID, messages = config$messages ){
  out <- list(error = FALSE, message = "")
  
  # add info if error
  if( length(info) ){
    out$error <- TRUE
    out$message <- paste0(out$message, info, messages$checkinfo$String)
    #out$message <- paste0(out$message, info,
    #    "For further assistence please briefly describe your problem in the input form on our help page.<br>"
    #)
  }
  
  return(out)
}








###################
#### Check hit ####
###################
# test entries made before allowing start of hit_candidate.r
# arguments name  chr of gene name entered
# value   list of 2: error and message
#         error   boolean, were there issues?
#         message error message in case there were issues
#
# Check_hit <- function( name, options.anno, options.score, messages = config$messages ){
#   out <- list(error = FALSE, message = "")
#   
#   # if empty, return error
#   if( is.null(name) || is.na(name) || length(name) == 0 || name == "" ){
#     out$error <- TRUE
#     out$message <- paste0(out$message, messages$checkhit1$String)
#     #out$message <- paste0(out$message, "You did not enter a gene name yet.<br/>")
#   }
#   
#   # not more than 2 tracks should be selected
# 
#   # if( length(options) > 2 ){
#   #   out$error <- TRUE
#   #   out$message <- paste0(out$message, "Unfortunately, not more than 2 tracks can be displayed at the same time.
#   #                         Otherwise the plot will get to crouded.<br/>")
#   # }
#   
#   # at least 1 track should be selected
#   if( length(options.anno) == 0 || length(options.score) == 0 ){
#     out$error <- TRUE
#     out$message <- paste0(out$message, messages$checkhit2$String)
#     #out$message <- paste0(out$message, "Please select at least 1 track each for the Genomic View plot.<br/>")
#   }
#   
#   return(out)
# }

## Gene annotation
Check_hit_annotation <- function( name, options.anno, messages = config$messages ){
  out <- list(error = FALSE, message = "")
  
  # if empty, return error
  if( is.null(name) || is.na(name) || length(name) == 0 || name == "" ){
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkhitannotation1$String)
    #out$message <- paste0(out$message, "You did not enter a gene name yet.<br/>")
  }
  
  # not more than 2 tracks should be selected
  
  # if( length(options) > 2 ){
  #   out$error <- TRUE
  #   out$message <- paste0(out$message, "Unfortunately, not more than 2 tracks can be displayed at the same time.
  #                         Otherwise the plot will get to crouded.<br/>")
  # }
  
  # at least 1 track should be selected
  if( length(options.anno) == 0 ){
    out$error <- TRUE
    out$message <- paste0(out$message, messages$checkhitannotation2$String)
    #out$message <- paste0(out$message, "Please select at least 1 annotation feature.<br/>")
  }
  
  return(out)
}








###########################
#### Check hit Results ####
###########################
# test results of hit_candidate.r by looking at hit_candidate.info file
# arguments   info  chr value with error occured in batch script
#             ID    chr of user ID
# value   list of 2: error and message
#         error   boolean, were there issues?
#         message error message in case there were issues
Check_hitResults <- function ( info, ID , messages = config$messages){
  out <- list(error = FALSE, message = "")
  
  # add info if error
  if( length(info) ){
    out$error <- TRUE
    out$message <- paste0(out$message, info,
                messages$checkhitresults$String# "For further assistence please briefly describe your problem in the input form on our help page.<br>"
    )
  }
  
  return(out)
}


Check_annotationResults <- function ( info, ID ){
  out <- list(error = FALSE, message = "")
  
  # add info if error
  if( length(info) ){
    out$error <- TRUE
    out$message <- paste0(out$message, info,
                          "For further assistence please briefly describe your problem in the input form on our help page.<br>"
    )
  }
  
  return(out)
}





######################
### Chooser Input ####
######################
# function for creating choser input type
# got it from stack... didnt really look at it
# it works...
chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
  size = 5, multiple = FALSE) {
  
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)
  
  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL
  
  tagList(
    singleton(tags$head(
      tags$script(src="chooser-binding.js"),
      tags$style(type="text/css",
        HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(id=inputId, class="chooser",
      div(class="chooser-container chooser-left-container",
        tags$select(class="left", size=size, multiple=multiple, leftChoices)
      ),
      div(class="chooser-container chooser-center-container",
        icon("arrow-circle-o-right", "right-arrow fa-3x"),
        tags$br(),
        icon("arrow-circle-o-left", "left-arrow fa-3x")
      ),
      div(class="chooser-container chooser-right-container",
        tags$select(class="right", size=size, multiple=multiple, rightChoices)
      )
    )
  )
}








#########################################
#### Input Handler for Chooser Input ####
#########################################
registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
  if (is.null(data))
    NULL
  else
    list(left=as.character(data$left), right=as.character(data$right))
}, force = TRUE)






####################
#### Plot_blank ####
####################
# function for plotting a placeholder for plot renders in app
# necessary arguments are device
# arguments   device  chr "hc" or "base" defining render device renderHighchart2() or renderPlot()
#             pos     num arr length 2 for x and y pos in a range of [0;1]
#             msg     chr for message to be shown
#             col     chr or num for colour
#             size    int which directs size of message
# value       highcharter or base R plot object
Plot_blank <- function( device, pos = c(1, 1), msg = config$messages$noanalysisrunyet$String, col = "#37474F", size = 3 ){
  
  if( device == "hc" ){
    size <- paste0(size, "0px")
    highchart() %>%
      hc_chart(type = "scatter") %>%
      hc_yAxis(title = list(text = NULL), gridLineWidth = 0, labels = list(enabled = FALSE)) %>%
      hc_xAxis(title = list(text = NULL), gridLineWidth = 0, labels = list(enabled = FALSE)) %>%
      hc_add_series(data = list(list("x" = pos[1], "y" = pos[2], "text" = msg)),
        type = "scatter", color = "transparent", showInLegend = FALSE, enableMouseTracking = FALSE,
        dataLabels = list(enabled = TRUE, format = "{point.text}", style = list(fontSize = size, color =  col)))
  } else if( device == "base" ){
    size <- size * 0.6
    plot.new()
    text(pos[1], pos[2], msg, col = col, cex = size, font = 2)
  }
}








#####################
#### Plot_column ####
#####################
# core function for plotting column plots with highcharter
# necessary arguments are seriesNames, catName, data
# arguments   seriesNames   chr arr of names of series to be plotted as they appear in data
#             catName       chr of name for categories arr as it appears in data
#             data          data.frame having named num arr of seriesNames and named arr of catName
#             tooltip       NULL for default tooltip or chr HTML code for tooltip in table envir
#             title         chr for title
#             subtitle      chr for subtitle
#             xLab          chr for title of x axis
#             yLab          chr for title of y axis
#             zoom          chr "x", "y", or "xy" defining zoom type
#             crosshair     bool whether to show crosshairs
#             legend        bool whether to show legend
#             export        bool whether to enable and show export
#             cols          NULL for no special colouring of columns or chr array of hex colours for individual
#                           colouring of columns. This only makes sense with one series.
#             col           chr arr of length seriesNames which specifies color for each series
#             anno          NULL or data.frame. if not NULL it must be a dataframe used for
#                           writing annotations ("x", "y", "text")
# value       highcharter plot object
#
Plot_column <- function( seriesNames, catName, data, 
                  tooltip = FALSE, title = "", subtitle = "", xLab = "", yLab = "", zoom = "x",
                  crosshair = c(FALSE, TRUE), legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = NULL , turboT = 0){
  export <- TRUE # subsequent correction
  
  #check if boost.js is useful
  # if(nrow(data) > 12000)
  # {
  #   hc <- highcharter::highchart()
  # }else {
  #   hc <- highcharter::highchart()
  # }
  # 
   hc <- highcharter::highchart() %>%
    highcharter::hc_chart( type = "column", zoomType = zoom) %>%
    highcharter::hc_plotOptions(series = list(turboThreshold = turboT)) %>% 
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = subtitle) %>%
    highcharter::hc_xAxis(title = list(text = xLab), labels  = list(rotation = "-45"), crosshair = crosshair[1]) %>%
    highcharter::hc_yAxis(title = list(text = yLab), align = "left", showFirstLabel = FALSE, 
      showLastLabel = FALSE, labels = list(useHTML = TRUE), opposite = FALSE, crosshair = crosshair[2]) %>%
    highcharter::hc_legend(enabled = legend) %>%
    highcharter::hc_exporting(enabled = export,
                 printMaxWidth = 2000,
                 scale=8,
                 filename = filename,
                 formAttributes = list(target = "_blank"))
 
   if(!is.null(catName)) {
    hc <- highcharter::hc_xAxis(hc, title = list(text = xLab), categories = data[[catName]], labels  = list(rotation = "-45"))
  }
  
  if(tooltip !=FALSE)
  {
    if( is.null(tooltip) ){
      hc <- highcharter::hc_tooltip(hc, shared = TRUE, borderWidth = 0, delayForDisplay = 1500)
    } else {
      hc <- highcharter::hc_tooltip(hc, shared = TRUE, borderWidth = 0, delayForDisplay = 1500,
                                    useHTML = TRUE, headerFormat = "<table>", pointFormat = tooltip, footerFormat = "</table>")
    }
  } else
  {
    hc <- highcharter::hc_tooltip(hc, enabled=FALSE)
  }
  
  
  if( is.null(cols) ) bCols <- FALSE else bCols <- TRUE
    
  for( i in 1:length(seriesNames) ){
    d <- data[, c(catName, seriesNames[i])]
    colnames(d) <- c("name", "y")
    hc <- highcharter::hc_add_series(hc, type = "column", name = seriesNames[i], data = highcharter::list_parse(d),
                        colorByPoint = bCols, colors = cols, color = col[i])
  }
  
  if( !is.null(anno) ){
    anno <- list_parse(anno)
    hc <- highcharter::hc_add_series(hc, data = anno, name = "annotations", type = "column",
              color = "transparent", showInLegend = FALSE, enableMouseTracking = FALSE,
              dataLabels = list(enabled = TRUE, y = 10, format = "{point.text}",
              style = list(fontSize = "10px", color =  'rgba(0,0,0,0.70)')) )
  }

  
  hc <- highcharter::hc_add_theme(hc, hc_theme_google2())
  

  return(hc)
}


#####################
#### Plot_bar ####
#####################
# core function for plotting bar plots with highcharter
# necessary arguments are seriesNames, catName, data
# arguments   seriesNames   chr arr of names of series to be plotted as they appear in data
#             catName       chr of name for categories arr as it appears in data
#             data          data.frame having named num arr of seriesNames and named arr of catName
#             tooltip       NULL for default tooltip or chr HTML code for tooltip in table envir
#             title         chr for title
#             subtitle      chr for subtitle
#             xLab          chr for title of x axis
#             yLab          chr for title of y axis
#             zoom          chr "x", "y", or "xy" defining zoom type
#             crosshair     bool whether to show crosshairs
#             legend        bool whether to show legend
#             export        bool whether to enable and show export
#             cols          NULL for no special colouring of columns or chr array of hex colours for individual
#                           colouring of columns. This only makes sense with one series.
#             col           chr arr of length seriesNames which specifies color for each series
#             anno          NULL or data.frame. if not NULL it must be a dataframe used for
#                           writing annotations ("x", "y", "text")
# value       highcharter plot object
#
Plot_bar <- function( seriesNames, catName, data, 
                         tooltip = NULL, title = "", subtitle = "", xLab = "", yLab = "", zoom = "x",
                         crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, anno = NULL, col = NULL, filename = NULL ){
  export <- TRUE # subsequent correction
  
  hc <- highcharter::highchart() %>%
    highcharter::hc_chart(type = "bar", zoomType = zoom) %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = subtitle) %>%
    highcharter::hc_yAxis(title = list(text = xLab), labels  = list(rotation = "-45")) %>%
    highcharter::hc_xAxis(title = list(text = yLab), align = "left", showFirstLabel = FALSE, 
             showLastLabel = FALSE, labels = list(useHTML = TRUE), opposite = FALSE) %>%
    highcharter::hc_legend(enabled = legend) %>%
    highcharter::hc_exporting(enabled = export,
                 printMaxWidth = 2000,
                 scale=8,
                 filename = filename,
                 formAttributes = list(target = "_blank"))
  
  
  
  if( is.null(tooltip) ){
    hc <- highcharter::hc_tooltip(hc, crosshairs = crosshair, shared = TRUE, borderWidth = 0, delayForDisplay = 1500)
  } else {
    hc <- highcharter::hc_tooltip(hc, crosshairs = crosshair, shared = TRUE, borderWidth = 0, delayForDisplay = 1500,
                     useHTML = TRUE, headerFormat = "<table>", pointFormat = tooltip, footerFormat = "</table>")
  }
  
  if( is.null(cols) ) bCols <- FALSE else bCols <- TRUE
  
  for( i in 1:length(seriesNames) ){
    d <- data[, c(catName, seriesNames[i], "qval", "genes")]
    colnames(d) <- c("name", "y", "qval", "genes")
    hc <- highcharter::hc_add_series(hc, type = "bar", name = seriesNames[i], data = list_parse(d),
                        colorByPoint = bCols, colors = cols, color = col[i], dataLabels = list(enabled = TRUE, format = '{point.name}', style=  list(fontSize = '11px') ) )
  }
  
  if( !is.null(anno) ){
    anno <- list_parse(anno)
    hc <- highcharter::hc_add_series(hc, data = anno, name = "annotations", type = "column",
                        color = "transparent", showInLegend = FALSE, enableMouseTracking = FALSE,
                        dataLabels = list(enabled = TRUE, y = 10, format = "{point.text}",
                                          style = list(fontSize = "10px", color =  'rgba(0,0,0,0.70)')) )
  }
  
  
  hc <- highcharter::hc_add_theme(hc, hc_theme_google2())
  
  
  return(hc)
}






###################
#### Plot_area ####
###################
# core function for plotting area plots with highcharter
# necessary arguments are seriesNames, catName, data
# arguments   seriesNames   chr arr of names of series to be plotted as they appear in data
#             catName       chr of name for categories arr as it appears in data
#             data          data.frame having named num arr of seriesNames and named arr of catName
#             tooltip       NULL for default tooltip or chr HTML code for tooltip in table envir
#             title         chr for title
#             subtitle      chr for subtitle
#             xLab          chr for title of x axis
#             yLab          chr for title of y axis
#             zoom          chr "x", "y", or "xy" defining zoom type
#             pol           bool whether to use polar coords (radar plot)
#             crosshair     bool whether to show crosshairs
#             legend        bool whether to show legend
#             export        bool whether to enable and show export
# value       highcharter plot object
#
Plot_area <- function( seriesNames, catName, data,
                  tooltip = NULL, title = "", subtitle = "", xLab = "", yLab = "", zoom = "x",
                  pol = FALSE, crosshair = TRUE, legend = TRUE, export = TRUE, filename = NULL ){
  export <- TRUE # subsequent correction
  
  hc <- highcharter::highchart() %>%
    highcharter::hc_chart(type = "area", zoomType = zoom, polar = pol) %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = subtitle) %>%
    highcharter::hc_xAxis(title = list(text = xLab), categories = data[[catName]],labels  = list(rotation = "-45")) %>%
    highcharter::hc_yAxis(title = list(text = yLab), align = "left", showFirstLabel = FALSE, 
      showLastLabel = FALSE, labels = list(useHTML = TRUE), opposite = FALSE, 
      gridLineInterpolation = "polygon", lineWidth = 0, min = 0) %>%
    highcharter::hc_legend(enabled = legend) %>%
    highcharter::hc_exporting(enabled = export,
                 printMaxWidth = 2000,
                 scale=8,
                 filename = filename,
                 formAttributes = list(target = "_blank"))
  
  if( is.null(tooltip) ){
    hc <- highcharter::hc_tooltip(hc, crosshairs = crosshair, shared = TRUE, borderWidth = 0, delayForDisplay = 1500)
  } else {
    hc <- highcharter::hc_tooltip(hc, crosshairs = crosshair, shared = TRUE, borderWidth = 0, delayForDisplay = 1500,
      useHTML = TRUE, headerFormat = "<table>", pointFormat = tooltip, footerFormat = "</table>")
  }
    
  for( i in 1:length(seriesNames) ){
    d <- data[, c(catName, seriesNames[i])]
    colnames(d) <- c("name", "y")
    hc <- highcharter::hc_add_series_df(hc, type = "area", name = seriesNames[i], data = d, #list_parse(d)
                       fillOpacity = 0.5, lineWidth = 0)
  }

 # if( length(seriesNames) <= 4 ){
    hc <- highcharter::hc_add_theme(hc, hc_theme_google2())
  #}

  return(hc)
}



################################
#### Plot_coverage_unmapped ####
################################
# plot function for highcharter plot of unmapped genes/ sgRNAs
# necessary agruments are data, type
# arguments   data    data.frame of unmappedGenes
#             type    chr "sgRNA", "gene", "non", or "pos" for plotting missing genes,
#                     non-targeting ctrls or positive ctrls
#             bApp    bool TRUE will adjust appearance for shiny app
# value       highcharter plot object
Plot_coverage_unmapped <- function( data, type, bApp = TRUE, filename = NULL){
  
  df <- switch(type,
    sgRNA = data$basic,
    gene = data$basic,
    non = data$nontarget,
    pos = data$pos
  )
  cat <- "dataset"
  series <- switch(type,
    sgRNA = "sgRNA",
    gene = "gene"
  )
  
  xl <- "dataset"
  yl <- switch(type,
    sgRNA = "Number of sgRNAs",
    gene = "Number of genes",
    non = "Percentage of sgRNAs",
    pos = "Percentage of sgRNAs"
  )
  
  tit <- switch(type,
                sgRNA = "Missing sgRNAs",
                gene = "Missing Genes",
                non = "% missing non-targeting sgRNAs",
                pos = "% missing positive sgRNAs"
  )
  
  if( bApp == FALSE ){
    
    sub <- switch(type,
      sgRNA = "These sgRNAs were not mapped and are therefore considered missing.",                 
      gene = "These genes were not mapped and are therefore considered missing.",
      non = "Percentage of missing sgRNAs among the genes specified as non-targeting controls.",
      pos = "Percentage of missing sgRNAs among the genes specified as positive controls."
    )
    ex <- FALSE
  } else {
    #tit <- ""
    sub <- ""
    ex <- TRUE
  }
  
  if( type == "sgRNA" || type == "gene" ){
    leg <- FALSE
  } else {
    leg <- TRUE
    df <- spread(df, gene, missing)
    series <- names(df)
    series <- series[which(series != "dataset")]
  }
  file = filename
  
  p <- Plot_column( series, cat, df, title = tit, subtitle = sub, xLab = xl, yLab = yl, 
                    legend = leg, export = ex , filename = file)
  
  return(p)
}








##################################
#### Plot_coverage_readDistro ####
##################################
# function to plot read distribution of all datasets with highcharter
# necessary argument is data
# arguments   data  data.frame of results$readDistribution
#             bApp  bool TRUE will adjust appearance for shiny app
# value       highcharter plot object
Plot_coverage_readDistro <- function( data, bApp = TRUE , filename = NULL){
  tit <- "Read Count Distribution"
  if( bApp == TRUE ){
    ex <- TRUE
    #tit <- ""
    
  } else {
    ex <- TRUE # subsequent correction
  }
  
  df <- data
  Ds <- list()
  for( i in 1:length(df) ){
    d <- diff(df[[i]][["plot"]]$breaks)
    Ds[[i]] <- list_parse(dplyr::data_frame(x = df[[i]][["plot"]]$mids, y = df[[i]][["plot"]]$counts, 
      name = sprintf("Readcount from: %s to %s", 
          round(2^(df[[i]][["plot"]]$mids - d / 2), digits = 0), 
          round(2^(df[[i]][["plot"]]$mids + d / 2)), digits = 0)))
  }

  hc <- highcharter::highchart() %>%
    highcharter::hc_chart(type = "area", zoomType = "x") %>%
    highcharter::hc_title(text = tit) %>%
    highcharter::hc_plotOptions(area = list(dashStyle = "Dash", 
          marker = list(enabled = FALSE, symbol = "circle", radius = 2, 
          states = list(hover = list(enabled = TRUE))))) %>%
    highcharter::hc_xAxis(title = list(text = "log2 Readcount"), align = "middle", crosshair = TRUE) %>%
    highcharter::hc_yAxis(title = list(text = "number of sgRNAs"), align = "left", crosshair = TRUE) %>%
    highcharter::hc_exporting(enabled = ex,
                   printMaxWidth = 3000,
                   scale=8,
                   filename = filename,
                   formAttributes = list(target = "_blank")) %>%
    highcharter::hc_tooltip(enabled=FALSE) %>%
    highcharter::hc_size(height="600px")
  
  #if( length(Ds) <= 4 ){
    hc <- highcharter::hc_add_theme(hc, hc_theme_google2())
  #}

  for( i in 1:length(Ds) ){
      hc <- highcharter::hc_add_series(hc, name = attr(df, which = "name")[i], data = Ds[[i]], 
                          type = "area", fillOpacity = 0.3)
  }
  
  return(hc)
}


#### Read Distribution Box Plot

Plot_coverage_readDistroBox <- function( data, bApp = TRUE, type = "sgRNA", filename = NULL){
  
  tit <- "Normalized Readcount Distribution Box Plot"
  if( bApp == TRUE ){
    ex <- TRUE
    # tit <- ""
    
  } else {
    ex <- TRUE # subsequent correction
    
  }
  
  
  
  hc <- highcharter::highchart()
  hc <- highcharter::hc_chart(hc, zoomType = "x")
  hc <- highcharter::hc_title(hc, text = tit)
  hc <- highcharter::hc_legend(hc, enabled = TRUE) 
  hc <- highcharter::hc_add_series_boxplot(hc, x = data$value, by = data$variable, name="Read Counts")
  hc <- highcharter::hc_add_theme(hc,hc_theme_google2())
  hc <- highcharter::hc_xAxis(hc, title = list(text = "Samples"), align = "middle", labels  = list(rotation = "-45"))
  hc <- highcharter::hc_yAxis(hc, title = list(text = paste(type, "log2-transformed Read Count", sep=" ")), align = "middle")
  hc <- highcharter::hc_exporting(hc, enabled = ex,
               printMaxWidth = 3000,
               scale=8,
               filename = filename,
               formAttributes = list(target = "_blank")) 
  hc <- highcharter::hc_tooltip(hc, enabled=FALSE)
  hc <- highcharter::hc_size(hc, height="1000px")
  hc <- hc_add_theme(hc, hc_theme_google2())
  
  return(hc)
}





#################################
#### Plot_coverage_readDepth ####
#################################
# function to plot read depth of one dataset with highcharter
# necessary arguments are data, dataset
# arguments   data    data.frame of results$readDistribution
#             dataset chr of dataset generic name as created by user
#             bNon    bool whether a non-targeting ctrl was used
#             bPos    bool whether a positive ctrl was used
#             bApp    bool TRUE will adjust appearance for shiny app
# value       highcharter plot object
Plot_coverage_readDepth <- function( data, dataset, bNon, bPos, bApp = TRUE , filename = NULL){

  df <- data[[dataset]] 
  
  df2 <- data.frame( x = nrow(df) * 0.9, y = quantile(df$persgRNA, 0.999),
          text = paste("Median", round(median(df$persgRNA), 2), "+/-", round(mad(df$persgRNA)), 2))

  df$labelgene[which(df$labelgene == "red3")] <- "positive"
  df$labelgene[which(df$labelgene == "blue")] <- "non-targeting"
  df$labelgene[which(df$labelgene == "#000000A6")] <- "no-ctrl"
  df <- spread(df, labelgene, persgRNA)
  
  series <- "no-ctrl"
  colours <- "#808080"
  if( bNon == TRUE ){
    series <- c("non-targeting", series)
    colours <- c("#3369E8", colours)
  }
  if( bPos == TRUE ){
    series <- c("positive", series)
    colours <- c("#D50F25", colours)
  }
  cats <- "gene"
  
  tit <- paste("Readcount per sgRNA for", dataset)
  if( bApp == TRUE ){
    #tit <- ""
    ex <- TRUE
    df2 <- NULL
  } else {
    
    ex <- FALSE
  }
  xl <- "gene"
  yl <- "readcount/sgRNA"
  file <- filename

  p <- Plot_column(series, cats, df, anno = df2, col = colours, title = tit,
                   xLab = xl, yLab = yl, export = ex , filename = file)

  return(p)
}








###################################
#### Plot_coverage_geneDesigns ####
###################################
# function to plot designs per gene of one dataset with highcharter
# necessary arguments are data, dataset
# arguments   data    data.frame of results$geneDesigns
#             dataset chr of dataset generic name as created by user
#             nBreaks int of how many bins will be used
#             bApp    bool TRUE will adjust appearance for shiny app
# value       highcharter plot object
Plot_coverage_geneDesigns <- function( data, dataset, nBreaks = 20, bApp = TRUE , filename = NULL){
  
  df <- data[[dataset]]$ratio
  tit <- paste("Designs per Gene for", dataset)
  if( bApp == TRUE ){
    #tit <- ""
    ex <- TRUE
  } else {
    
    ex <- TRUE # subsequent correction
  }
  xl <- "% of sgRNAs per gene present"
  yl <- "Number of genes"
  
  p <- highcharter::hchart(df, breaks = nBreaks) %>%
  # p <- highchart() %>% 
    highcharter::hc_yAxis(title = list(text = yl), align = "left") %>%
    highcharter::hc_title(text = tit) %>%
    highcharter::hc_xAxis(title = list(text = xl), align = "middle") %>%
   #  hc_add_series(data = df) %>%
    highcharter::hc_legend(enabled = FALSE) %>%
    highcharter::hc_exporting(enabled = ex,
                 printMaxWidth = 2000,
                 scale=8,
                 filename = filename,
                 formAttributes = list(target = "_blank")) %>%
    highcharter::hc_add_theme(hc_theme_google2())  
  
  return(p)
}








###############################
#### Plot_Replicates_SPLOM ####
###############################
# function to plot a scatter plot matrix comparing each dataset compared in analysis.r
# necessary arguments are readCountVS, dsNames
# arguments     readCountVS data.frame of results$readCountVS
#               dsNames     chr arr of generic names of datasets given by user
#               bLog        bool whether to use log10 scale axis
#               bAggro      bool whether to aggregate data to genes
#               cCtrl       chr "pos" or "neg" defining whether positive ctrls are highlighted(red)
#                           or nontargeting ctrls are highlighted (blue)
# value         NULL
# side effects  prints a plot
#
#### Loop for paired plot:
# computes positions for plots, correl coeffs, and names
#
# n Datasets
# span n x n grid
# walk through grid row by row, column by column
# compute positions where plots/ correl coeff/ dataset names should appear
# 
# N = 1
# while N < n
#     dataset name pos = (N - 1) * N * n
#     K = N + 1
#     while K <= n
#         Nth vs Kth dataset plot pos = (N - 1) * n + K
#         Nth vs Kth dataset correl coeff pos = (K - 1) * n + N
#         K++
#     N++
#
# then actually draw grid and walk from 1 to n * n
# at each position check whether plot, name or correl coeff should be drawn here
#
Plot_Replicates_SPLOM <- function( readCountVS, dsNames, 
                                   bLog = FALSE, bAggro = FALSE, cCtrl = "pos" ){
  
  nds <- length(dsNames)

  if( bAggro ) df <- readCountVS$gene else df <- readCountVS$sgRNA
  
  # if(cCtrl == "pos")
  # {
  #   marker <- "pos"
  #   color <- "#D50F25"
  # } else if (cCtrl == "neg")
  # {
  #   marker <- "neg"
  #   color <- "#3369E8"
  # } else
  # {
  #   marker <- ""
  #   color <- "#000000A6"
  # }
  
  nDf1 <- numeric() # Kth dataset
  nDf2 <- numeric() # Nth dataset
  nPlot <- numeric() # pos for plot of Kth over Nth dataset
  nCC <- numeric() # pos for CC of Kth over Nth dataset
  nDs <- numeric() # pos for name of Nth dataset
  CCs <- numeric(2) # store CCs. left: pearson, right: spearman
      N <- 1
  while( N < nds ){
    nDs <- c(nDs, (N - 1) * nds + N)
    K <- N + 1
    while( K <= nds ){
      nDf1 <- c(nDf1, K)
      nDf2 <- c(nDf2, N)
      nPlot <- c(nPlot, (N - 1) * nds + K)
      nCC <- c(nCC, (K - 1) * nds + N)
      CCs <- rbind(CCs, c(cor(df[[dsNames[N]]], df[[dsNames[K]]], method = "pearson"),
                          cor(df[[dsNames[N]]], df[[dsNames[K]]], method = "spearman")))
      K <- K + 1
    }
    N <- N + 1
  }
  nDs <- c(nDs, (nds - 1) * nds + nds)
  CCs <- CCs[-1,]
  
  par(mfrow = c(nds, nds), mar = c(0,0,0,0))

  for( i in 1:(nds * nds) ){
    if( i %in% nDs ){
      plot(1, 1, type = "n", xaxt = 'n', yaxt = 'n', bty = "n")
      box(lwd = 1)
      text(1, 1, dsNames[which(nDs == i)], cex = 3)
  
    } else if( i %in% nPlot ){
      if( bLog == FALSE ){
        df1 <- df[[dsNames[nDf1[which(nPlot == i)]]]]
        df2 <- df[[dsNames[nDf2[which(nPlot == i)]]]]
      } else {
        df1 <- log10(df[[dsNames[nDf1[which(nPlot == i)]]]])
        df2 <- log10(df[[dsNames[nDf2[which(nPlot == i)]]]])
      }
      cols <- df$labelgene
      
      cols[cols == ""] <- adjustcolor("#8f8f8f", alpha.f = 0.3) 
      cols[cols == "pos"] <- "#D50F25"
      cols[cols == "neg"] <- "#3369E8"
      
      # plottingcolor <- adjustcolor("#8f8f8f", alpha.f = 0.3) 
      # if(marker != "") # everything grey except the selected marker
      # {
      #   cols[cols != marker] <- plottingcolor
      #   cols[cols == marker] <- color
      # } else { # everything grey!
      #   cols[cols == marker] <- plottingcolor
      #   cols[cols != marker] <- plottingcolor
      # }
      #cols[which(df$labelgene == marker)] <- rep(color, length(which(df$labelgene == marker))) # add color to labels
      #cols[which(df$labelgene == "")] <- rep("#000000A6", length(which(df$labelgene == ""))) # add color to rest
      
      
      plot(df1, df2, pch = 20, col = cols, xaxt = 'n', yaxt = 'n', bty = "n")
  
    } else if( i %in% nCC ){
      plot(1, 1, type = "n", xaxt = 'n', yaxt = 'n', bty = "n")
      t <- paste0("Pearson: ", round(CCs[which(nCC == i), 1], 2), 
                  "\nSpearman: ", round(CCs[which(nCC == i), 2], 2))
      text(1, 1, t, cex = 2)
    }
  }  
}  








######################
#### Plot_scatter ####
######################
# core function for plotting scatter plots with highcharter
# necessary arguments are dataList, seriesNames
# arguments   dataList      list of data.frames where each data.frame will be used to create a series
#                           so each data.frame must contain "x" and "y" value possible values
#                           used as tooltip information
#             seriesNames   chr arr of series names in same order and length as dataList
#             tooltip       NULL for default tooltip or chr HTML code for tooltip in table envir
#             title         chr for title
#             subtitle      chr for subtitle
#             xLab          chr for title of x axis
#             yLab          chr for title of y axis
#             zoom          chr "x", "y", or "xy" defining zoom type
#             crosshair     bool arr of length 2 whether crosshairs for x and/or y axis is shown
#             legend        bool whether to show legend
#             export        bool whether to enable and show export
#             col           NULL or chr arr of length seriesNames which specifies color for each 
#                           series, for NULL colors from theme will be used
#             anno          NULL or data.frame. if not NULL it must be a dataframe used for
#                           writing annotations ("x", "y", "text")
#             xTrans        chr "linear", "logarithmic", "datetime", or "category"
#             yTrans        chr "linear", "logarithmic", "datetime", or "category"
#             sym           bool whether to scale axis symmetrically
# value       highcharter plot object
#
Plot_scatter <- function( dataList, seriesNames,
                  tooltip = NULL, title = "", subtitle = "", xLab = "", yLab = "", zoom = "xy",
                  crosshair = c(FALSE, FALSE), legend = TRUE, export = TRUE, turboT = 0, anno = NULL, 
                  col = NULL, xTrans = "linear", yTrans = "linear" , filename = NULL, sym = FALSE){
  export <- TRUE # subsequent correction

  xLim <- range(do.call(rbind, dataList)$x)
  yLim <- range(do.call(rbind, dataList)$y)
  if(sym){
    xLim[1] <- min(xLim[1], yLim[1])
    yLim[1] <- min(xLim[1], yLim[1])
    xLim[2] <- max(xLim[2], yLim[2])
    yLim[2] <- max(xLim[2], yLim[2])
  }
  
  
  hc <- highcharter::highchart() %>%
    highcharter::hc_chart(zoomType = zoom) %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = subtitle) %>%
    highcharter::hc_plotOptions(series = list(turboThreshold = turboT, 
        marker = list(symbol = "circle", radius = 2))) %>%
    highcharter::hc_xAxis(title = list(text = xLab), type = xTrans, min = xLim[1], max = xLim[2], crosshair = crosshair[1]) %>%
    highcharter::hc_yAxis(title = list(text = yLab), type = yTrans, min = yLim[1], max = yLim[2], crosshair = crosshair[2]) %>%
    highcharter::hc_legend(enabled = legend) %>%
    highcharter::hc_exporting(enabled = export,
                 printMaxWidth = 2000,
                 filename = filename,
                 scale=8,
                 formAttributes = list(target = "_blank")
                 )
  
  for( i in 1:length(seriesNames) ){
    d <- list_parse(dataList[[i]])
    hc <- highcharter::hc_add_series(hc, data = d, type = "scatter",
        name = seriesNames[i], zIndex = 1, color = col[i], marker = list(radius = 3))
  }

  if( !is.null(anno) ){
    anno <- highcharter::list_parse(anno)
    hc <- highcharter::hc_add_series(hc, data = anno, name = "annotations", type = "scatter",
              color = "transparent", showInLegend = FALSE, enableMouseTracking = FALSE,
              dataLabels = list(enabled = TRUE, y = 10, format = "{point.text}",
              style = list(fontSize = "10px", color =  'rgba(0,0,0,0.70)'))
    )
  }
if(tooltip != FALSE)
{
  if( is.null(tooltip) ){
    hc <- highcharter::hc_tooltip(hc, enabled = TRUE, borderWidth = 0, delayForDisplay = 1500)
  } else {
    hc <- highcharter::hc_tooltip(hc, ensabled = TRUE, shared = TRUE, borderWidth = 0, delayForDisplay = 1500,
                                  useHTML = TRUE, headerFormat = "<table>", pointFormat = tooltip, footerFormat = "</table>")
  }
} else {
  hc <- highcharter::hc_tooltip(hc, enabled = FALSE)
}
  

  
    hc <- highcharter::hc_add_theme(hc, hc_theme_google2())
 
  
  return(hc)
}








##############################
#### Plot_replicates_pair ####
##############################
# function for scatter plot comparing 2 datasets highlighting controls
# necessary arguments are xx, yy, data
# arguments   xx    chr of generic name of dataset given by user for x axis
#             yy    chr of generic name of dataset given by user for y axis
#             data  list of results$readCountVS
#             cCtrl chr "neg" or "pos" for whether non-targeting of positive ctrls are highlighted
#             aggro bool whether genes instead of sgRNAs are shown
#             bLog  bool whether to show on log10 scale
#             bApp  bool if TRUE adjustments for plotting in app are made
# value       highcharter plot object
Plot_replicates_pair <- function( xx, yy, data, cCtrl = "neg",
                                  aggro = TRUE, bLog = FALSE, bApp = TRUE , filename = NULL, labelgene = NULL, results = NULL){
  
 #Plot_replicates_pair(xx="Untreated", yy="Treated", data = out$readCountVS, cCtrl = "neg", bLog = FALSE, aggro = FALSE, bApp = TRUE, filename = paste("QualityControl_Replicates_", "_VS_", sep="") , labelgene = c("CASP8","FADD"), results = out$compare)
  
  # if( cCtrl == "neg" ) {
  #   if( aggro == TRUE ) df <- data$nontargetAgg else df <- data$nontarget
  #   col <- "#3369E8"
  #   ctrlName <- "non-targeting control"
  # } else if( cCtrl == "pos" ){
  #   if( aggro == TRUE ) df <- data$posAgg else df <- data$pos
  #   col <- "#D50F25"
  #   ctrlName <- "positive control"
  # }
  
  if( aggro ) 
  {
    if(bLog) df <- data$gene_log2 else df <- data$gene
  } else
  {
    if(bLog) df <- data$sgRNA_log2 else df <- data$sgRNA
  }
  
  ctrlName <- c("positive control", "non-targeting control")
  color <- c("#D50F25", "#3369E8")
  
  # if(cCtrl == "pos")
  # {
  #   marker <- "pos"
  #   color <- "#D50F25"
  #   ctrlName <- "positive control"
  # } else if (cCtrl == "neg")
  # {
  #   marker <- "neg"
  #   color <- "#3369E8"
  #   ctrlName <- "non-targeting control"
  # } else
  # {
  #   marker <- ""
  #   color <- "#000000A6"
  # }
  
  xx2 <- xx
  yy2 <- yy
  
  # check if groups
  if(xx %in% names(results))
  {
    xx <- results[[xx]]
    df[which(df[xx] <= 0), xx] <- NA
    # add mean of replicates
    df[,xx2] <- rowMeans(subset(df, select = xx, na.rm = TRUE))
    groups_x <- TRUE
    
  } else
  {
    df[[xx]][which(df[[xx]] <= 0)] <- NA
    groups_x <- FALSE
  }
  
  if(yy %in% names(results))
  {
    yy <- results[[yy]]
    df[which(df[yy] <= 0),yy] <- NA
    # add mean of replicates
    df[,yy2] <- rowMeans(subset(df, select = yy, na.rm = TRUE))
    groups_y <- TRUE
  } else
  {
    df[[yy]][which(df[[yy]] <= 0)] <- NA
    groups_y <- FALSE
  }
  
  
  df <- na.omit(df)
  
  # if( bLog== TRUE ){
  #   type <- "logarithmic"
  # } else {
    type <- "linear"
  # }

  if(groups_x)
  {
    xx <- xx2
  }
  if(groups_y)
  {
    yy <- yy2
  }
  
  rand <- df[, ]
  rand <- data.frame("x" = round(rand[[xx]], digits=2), "y" = round(rand[[yy]], digits=2), "ID" = rand$gene, stringsAsFactors = FALSE)
  
  pos <- df[which(df$labelgene == "pos" ), ]
  pos <- data.frame("x" = round(pos[[xx]], digits=2), "y" = round(pos[[yy]], digits=2), "ID" = pos$gene, stringsAsFactors = FALSE)

  neg <- df[which(df$labelgene == "neg" ), ]
  neg <- data.frame("x" = round(neg[[xx]], digits=2), "y" = round(neg[[yy]], digits=2), "ID" = neg$gene, stringsAsFactors = FALSE)
  
  rand[["plotRatio"]] <- round(rand$y / rand$x, digits=2)
  randLow <- rand[which(rand$plotRatio <= 2), ] 
  randLow <- randLow[which(randLow$plotRatio >= 0.5), ]
  randHigh <- rbind(rand[which(rand$plotRatio >= 2), ], rand[which(rand$plotRatio <= 0.5), ])
  
  # remove plotRatio
  randLow$plotRatio <- NULL
  randHigh$plotRatio <- NULL
  
  # add labeled gene
  if(!is.null(labelgene))
  {
    genelabel  <- df[which(df$gene %in% labelgene), ]
    genelabel <- data.frame("x" = genelabel[[xx]], "y" = genelabel[[yy]], "ID" = genelabel$gene, stringsAsFactors = FALSE)
    
    data <- list(randLow, randHigh, pos, neg, genelabel)
    series <- c("ratio <= 2", "ratio > 2", ctrlName, paste(labelgene, collapse = " ; "))
    colours <- c("rgba( 143, 143, 143, 0.3 )", "rgba( 10, 10, 10, 0.5 )", color, "rgba(255, 152, 0, 0.9)")
  } else {
    data <- list(randLow, randHigh, pos, neg)
    series <- c("ratio <= 2", "ratio > 2", ctrlName)
    colours <- c("rgba( 143, 143, 143, 0.3 )", "rgba( 10, 10, 10, 0.5 )", color)
  }
  
  
  # } else if (marker != "")
  # {
  #   data <- list(randLow, randHigh, hlight)
  #   series <- c("ratio <= 2", "ratio > 2", ctrlName)
  #   colours <- c("rgba( 143, 143, 143, 0.3 )", "rgba( 10, 10, 10, 0.5 )", color)
  # } else {
  #   
  #   data <- list(randLow, randHigh)
  #   series <- c("ratio <= 2", "ratio > 2")
  #   colours <- c("rgba( 143, 143, 143, 0.3 )", "rgba( 10, 10, 10, 0.5 )")
  # }
  
  
  tt <- "<strong>Gene ID: {point.ID}</strong><br/>x : {point.x}<br/>y : {point.y}"
  
  if( aggro == TRUE ) a <- "Genes" else a <- "sgRNAs"
  b <- "Non-targeting and positive" # if( cCtrl == "neg" ) b <- "Non-targeting" else b <- "Positive"
  
  if( bApp == TRUE ){
    tit <- ""
    sub <- ""
    ex <- TRUE
  } else {
    tit <- paste("Readcount of", a, "of", yy, "over", xx)
    sub <- paste(b, "controls are highlighted")
    ex <- FALSE
  }
  
  file <- filename
  
  
  p <- Plot_scatter( data, series, col = colours, tooltip = tt, turboT = 0,
    xTrans = type, yTrans = type, xLab = xx, yLab = yy, title = tit, subtitle = sub, 
    export = ex , filename = file, sym = TRUE)

  return(p)
}








##########################
#### Plot_performance ####
##########################
# function for visualizing p value distributions of each analysis method
# necessary arguments are xx, yy, data
# arguments   data    object where results of used analyis method are stored
#             thresh  num value of p value threshold
#             method  chr "wilcox", "deseq", "mageckEn", "mageckDe", "sgrseaEn",
#                     "sgrseaDe", "edger" for defining analysis method
#             bApp    bool if TRUE adjustments for plotting in app are made
# value       highcharter plot object
Plot_performance <- function(data = NULL, thresh = NULL, bApp = TRUE,  method = NULL, type = NULL, filename=NULL){
  if(is.null(type))
  {
    df <- data
    xl <- "sorted genes"
    yl <- "-log10 of adjusted p value"
    
    tt <- tagList(
      shiny::tags$strong("{point.ID}"), shiny::tags$br(),
      shiny::tags$div("p value : {point.pVal}")
    ) %>% as.character()
    
    if( method == "wilcox" ){
      yy <- "p.value"
      df <- df[order(df[[yy]], na.last = TRUE), ]
      threshPrint <- round(thresh, 4)
      thresh <- -log10(thresh)
      df <- data.frame("y" = -log10(df[[yy]]), "x" = 1:nrow(df), "ID" = rownames(df), "pVal" = df[[yy]])
      rand <- df[which(df$y <= thresh), ]
      hlight <- df[which(df$y > thresh), ]
      
      tit <- "Ranked P-Values according to"
      sub <- "The p-value threshold is"
      
      methodName <- "Wilcox"
    } else if( method == "deseq" ){
      yy <- "padj"
      df <- df[order(df[[yy]], na.last = TRUE), ]
      threshPrint <- round(thresh, 4)
      thresh <- -log10(thresh)
      df <- data.frame("y" = -log10(df[[yy]]), "x" = 1:nrow(df), "ID" = df$genes, "pVal" = df[[yy]])
      rand <- df[which(df$y <= thresh), ]
      hlight <- df[which(df$y > thresh), ]
      
      tit <- "Ranked P-Values according to"
      sub <- "The p-value threshold is"
      
      methodName <- "DESeq2"
    } else if( method == "mageckEn" ){
      yy <- "pos"
      df <- df[order(df[[yy]], na.last = TRUE), ]
      threshPrint <- round(thresh, 4)
      thresh <- -log10(thresh)
      df <- data.frame("y" = -log10(df[[yy]]), "x" = 1:nrow(df), "ID" = df$genes, "pVal" = df[[yy]])
      rand <- df[which(df$y <= thresh), ]
      hlight <- df[which(df$y > thresh), ]
      
      tit <- "Ranked P-Values according to"
      sub <- "The p-value threshold is"
      
      methodName <- "MAGeCK for Enriched Genes"
    } else if( method == "mageckDe" ){
      yy <- "neg"
      df <- df[order(df[[yy]], na.last = TRUE), ]
      threshPrint <- round(thresh, 4)
      thresh <- -log10(thresh)
      df <- data.frame("y" = -log10(df[[yy]]), "x" = 1:nrow(df), "ID" = df$genes, "pVal" = df[[yy]])
      rand <- df[which(df$y <= thresh), ]
      hlight <- df[which(df$y > thresh), ]
      
      tit <- "Ranked P-Values according to"
      sub <- "The p-value threshold is"
      
      methodName <- "MAGeCK for Depleted Genes"
    } else if( method == "sgrseaEn" ){
      yy <- "FDR.pos"
      df <- df[order(df[[yy]], na.last = TRUE), ]
      threshPrint <- round(thresh, 4)
      thresh <- -log10(thresh)
      df <- data.frame("y" = -log10(df[[yy]]), "x" = 1:nrow(df), "ID" = rownames(df), "pVal" = df[[yy]])
      rand <- df[which(df$y <= thresh), ]
      hlight <- df[which(df$y > thresh), ]
      
      tit <- "Ranked P-Values according to"
      sub <- "The p-value threshold is"
      
      methodName <- "sgRSEA for Enriched Genes"
    } else if( method == "sgrseaDe" ){
      yy <- "FDR.neg"
      df <- df[order(df[[yy]], na.last = TRUE), ]
      threshPrint <- round(thresh, 4)
      thresh <- -log10(thresh)
      df <- data.frame("y" = -log10(df[[yy]]), "x" = 1:nrow(df), "ID" = rownames(df), "pVal" = df[[yy]])
      rand <- df[which(df$y <= thresh), ]
      hlight <- df[which(df$y > thresh), ]
      
      tit <- "Ranked P-Values according to"
      sub <- "The p-value threshold is"
      
      methodName <- "sgRSEA for Depleted Genes"
    } else if( method == "edger" ){
      yy <- "FDR"
      df <- df[order(df[[yy]], na.last=TRUE), ]
      threshPrint <- round(thresh, 4)
      thresh <- -log10(thresh)
      df <- data.frame("y" = -log10(df[[yy]]), "x" = 1:nrow(df), "ID" = rownames(df), "pVal" = df[[yy]])
      rand <- df[which(df$y <= thresh), ]
      hlight <- df[which(df$y > thresh), ]
      
      tit <- "Ranked P-Values according to"
      sub <- "The p-value threshold is"
      
      methodName <- "EdgeR"
    } else if( method == "zratio" ){
      yy <- "zratio"
      df <- df[order(df[[yy]], na.last=TRUE), ]
      threshPrint <- round(thresh, 4)
      df <- data.frame("y" = df[[yy]], "x" = 1:nrow(df), "ID" = df$gene, "Z-Ratio" = df[[yy]])
      rand <- df[which(abs(df$y) <= thresh), ]
      hlight <- df[which(abs(df$y) > thresh), ]
      yl <- "Z-Ratio"
      
      tit <- "Ranked Z-Ratios according to"
      sub <- "The Z-Ratio threshold is"
      
      methodName <- "Z-Ratio"
      
      tt <- shiny::tagList(
        shiny::tags$strong("{point.ID}"), shiny::tags$br(),
        shiny::tags$div("Z-Ratio : {point.Z-Ratio}")
      ) %>% as.character()
      
    } else if( method == "screenbeam" ){
      yy <- "FDR"
      df <- df[order(df[[yy]], na.last = TRUE), ]
      threshPrint <- round(as.numeric(thresh, 4))
      thresh <- -log10(thresh)
      df <- data.frame("y" = -log10(df[[yy]]), "x" = 1:nrow(df), "ID" = df$gene, "FDR" = df[[yy]])
      rand <- df[which(df$y <= thresh), ]
      hlight <- df[which(df$y > thresh), ]
      
      tit <- "Ranked P-Values according to"
      sub <- "The p-value threshold is"
      
      methodName <- "ScreenBEAM"
      
      tt <- tagList(
        tags$strong("{point.ID}"), tags$br(),
        tags$div("FDR : {point.FDR}")
      ) %>% as.character()
      
    }  else if( method == "bagel" ){
      yy <- "BF"
      df <- df[order(df[[yy]], na.last=TRUE, decreasing = TRUE), ]
      threshPrint <- round(thresh, 4)
      df <- data.frame("y" = df[[yy]], "x" = 1:nrow(df), "ID" = df$GENE, "BayesFactor" = df[[yy]])
      rand <- df[which(df$y <= thresh), ]
      hlight <- df[which(df$y > thresh), ]
      yl <- "log2 Bayes Factor"
      xl <- "Ranked Genes"
      
      tit <- "Gene Essentiality Scoring according to"
      sub <- "The calculated log2 Bayes Factor cutoff is"
      
      methodName <- "Bagel"
      
      tt <- tagList(
        tags$strong("{point.ID}"), tags$br(),
        tags$div("log2 Bayes Factor : {point.BayesFactor}")
      ) %>% as.character()
      
    }
    
    # remove tooltips for testing
    #tt <- FALSE
    
    if( bApp == TRUE ){
      tit <- ""
      sub <- ""
      ex <- TRUE
    } else {
      tit <- paste(tit, methodName)
      sub <- paste(sub, threshPrint)
      ex <- FALSE
    }
    file <- filename
    
    p <- Plot_scatter( list(rand, hlight), c("not significant", "significant"), zoom = "x", 
                       col = c("rgba( 0 , 0 , 0 , 0.3)", "#D50F25"), tooltip = tt, turboT = 12000,
                       xLab = xl, yLab = yl, title = tit, subtitle = sub, export = ex , filename = file, crosshair = c(TRUE,TRUE))
    
  }
  
  if(!is.null(type))
  {
    # plot distribution
    d <- data
    if(method == "Z-Ratio")
    {
      p <- highcharter::hchart(d,breaks = 50, name= method) %>% 
        highcharter::hc_title(text = "Z-Ratio Distribution") %>%
        highcharter::hc_subtitle(text = method) %>%
        highcharter::hc_yAxis(title = list(text = "Number of Genes") ) %>%
        highcharter::hc_xAxis(title = list(text = "Z-Ratio") ) %>%
        highcharter::hc_tooltip(enabled=FALSE) %>%
        highcharter::hc_exporting(enabled = TRUE,
                     printMaxWidth = 2000,
                     scale=8,
                     formAttributes = list(target = "_blank")) %>%
        highcharter::hc_add_theme(hc_theme_google2())
    } else if (method == "bagel") {
      p <- highcharter::hchart(d,breaks = 50, name= method) %>% 
        highcharter::hc_title(text = "log2 Bayes Factor Distribution") %>%
        highcharter::hc_subtitle(text = method) %>%
        highcharter::hc_yAxis(title = list(text = "Number of Genes") ) %>%
        highcharter::hc_xAxis(title = list(text = "log2 Bayes Factor") ) %>%
        highcharter::hc_tooltip(enabled=FALSE) %>%
        highcharter::hc_exporting(enabled = TRUE,
                     printMaxWidth = 2000,
                     scale=8,
                     formAttributes = list(target = "_blank")) %>%
        highcharter::hc_add_theme(hc_theme_google2())
    } else {
      p <- highcharter::hchart(d,breaks = 20, name = method) %>% 
        highcharter::hc_title(text = "P-Value Distribution") %>%
        highcharter::hc_subtitle(text = method) %>%
        highcharter::hc_yAxis(title = list(text = "Number of Genes") ) %>%
        highcharter::hc_xAxis(title = list(text = "P-Value") ) %>%
        highcharter::hc_tooltip(enabled=FALSE) %>%
        highcharter::hc_exporting(enabled = TRUE,
                     printMaxWidth = 2000,
                     scale=8,
                     filename = filename,
                     formAttributes = list(target = "_blank")) %>%
        highcharter::hc_add_theme(hc_theme_google2())
    }
    
    
  }
  

  return(p)
}








#########################
#### Plot_candidates ####
#########################
# function for creatig volcano plots of each analysis method
# necessary arguments are xx, yy, data
# arguments   df      data.frame of results$hitOverview_info
#             data    data.frame of results$hitOverview
#             method  chr "wilcox", "deseq", "mageckEn", "mageckDe", "sgrseaEn",
#                     "sgrseaDe", "edger" for defining analysis method
#             bApp    bool if TRUE adjustments for plotting in app are made
# value       highcharter plot object
Plot_candidates <- function( df, data, thresh, method, bApp = FALSE , filename = NULL){

  d2 <- data
  
  xl <- "log2 of foldchange"
  yl <- "-log10 of adjusted p value"
  
  tit <- "P-Values over phenotype according to"
  sub <- "P-Value threshold is"
  
  if( method == "wilcox" ){
    pval <- "wilcox.pval"
    l2fc <- "wilcox.log2fc"
    methodName <- "Wilcox"
  } else if( method == "deseq" ){
    pval <- "deseq.pval"
    l2fc <- "log2FoldChange"
    methodName <- "DESeq2"    
  } else if( method == "mageckEn" ){
    pval <- "mageck.pval.enriched"
    l2fc <- "log2FoldChange"
    methodName <- "MAGeCK for Enriched Genes"    
  } else if( method == "mageckDe" ){
    pval <- "mageck.pval.depleted"
    l2fc <- "log2FoldChange"
    methodName <- "MAGeCK for Depleted Genes"    
  } else if( method == "sgrseaEn" ){
    pval <- "rsea.enriched.pval"
    l2fc <- "log2FoldChange"
    methodName <- "sgRSEA for Enriched Genes"    
  } else if( method == "sgrseaDe" ){
    pval <- "rsea.depleted.pval"
    l2fc <- "log2FoldChange"
    methodName <- "sgRSEA for Depleted Genes"    
  } else if( method == "edger" ){
    pval <- "edger.pval"
    l2fc <- "log2FoldChange"
    methodName <- "EdgeR"    
  } else if( method == "zratio" ){
    pval <- "zratio"
    l2fc <- "zscore.treated"
    methodName <- "Z-Ratio"   
    yl <- "Z-Ratio"
    xl <- "Z-Score Treated"
  }
   
  threshPrint <- round(thresh, 4)
  thresh.old <- thresh
  thresh <- -log10(thresh)
  df[["y"]] <- -log10(d2[[pval]])
  df[["x"]] <- d2[[l2fc]]
  non <- df[which(df$y <= thresh), ]
  sign <- df[which( df$y > thresh ), ]
  
  if(method == "zratio")
  {
    df[["y"]] <- d2[[pval]]
    thresh <- thresh.old
    
    non <- df[which(abs(df$y) <= thresh), ]
    sign <- df[which(abs(df$y) > thresh ), ]
  }
  
  

  tt <- shiny::tagList(
    shiny::tags$h3("{point.genes}"),
    shiny::tags$table(style = "width:100%", 
                      shiny::tags$tr(shiny::tags$td(strong("Method")), shiny::tags$td(strong("Hit"))),
                      shiny::tags$tr(shiny::tags$td("Wilcox"), shiny::tags$td("{point.wilcoxSign}")),
                      shiny::tags$tr(shiny::tags$td("DESeq2"), shiny::tags$td("{point.deseqSign}")),
                      shiny::tags$tr(shiny::tags$td("MAGeCK"), shiny::tags$td("{point.mageckSign}")),
                      shiny::tags$tr(shiny::tags$td("sgRSEA"), shiny::tags$td("{point.rseaSign}")),
                      shiny::tags$tr(shiny::tags$td("EdgeR"), shiny::tags$td("{point.edgerSign}"))
    )
  ) %>% as.character()
  
  
  if( bApp == TRUE ){
    tit <- ""
    sub <- ""
    ex <- TRUE
  } else {
    tit <- paste(tit, methodName)
    sub <- paste(sub, threshPrint)
    ex <- FALSE
  }
  file <- filename
  
  p <- Plot_scatter( list(non, sign), c("not significant", "significant"), zoom = "xy", 
    col = c("rgba( 0 , 0 , 0 , 0.3)", "#D50F25"), tooltip = tt, turboT = 0,
    xLab = xl, yLab = yl, title = tit, subtitle = sub, export = ex , filename = file)
  
  return(p)
}








###################
#### Plot_venn ####
###################
# plotting a venn diagram as overview of analysis method results
# necessary arguments are vennObj, type
# arguments   vennObj   venn diagram object as of results$vennEnriched
#             ids       chr arr of element names of vennObj to be plotted
#             type      chr "en" or "de" whether enriched or depleted results are shown
#             bApp      bool TRUE will adjust plot for rendering in app
#             cols      chr arr of preset cols (length(cols) >= length vennObj) for diagram
# value       NULL
# side effects  a venn diagram is drawn
Plot_venn <- function( vennObj, ids, type,
                       bApp = TRUE,
                       cols = list("lightgreen", "lightblue2", "lightgray", "lightyellow", "orange") )
{
  names(cols) <- names(vennObj)
  fillObj <- unlist(cols[ids])
  
  vennObj <- lapply(ids, function(x) vennObj[[x]])
  names(vennObj) <- ids
  
  if( type == "en" ) a <- "Enriched" else a <- "Depleted"
  if( bApp == TRUE ){
    tit <- ""
  } else {
    tit <- paste("Significantly", a, "Genes according to different Methods")
  }
  
  flog.threshold(ERROR) # supress log file
  grid::grid.newpage()
  grid::grid.draw(VennDiagram::venn.diagram(vennObj, fill = fillObj, file = NULL, 
                                            na = "remove", cex = 1.5, lty = 1, lwd = 1, cat.cex = 1, main = tit, 
                                            main.cex = 1.5, main.fontfamily = 2, cat.fontfamily = 2))
}








##############################
#### Plot_sgRNA_readcount ####
##############################
# plot function for plotting sgRNA readcounts for a single gene
# necessary agruments are gene, dsNames, norm, raw
# arguments   gene    chr value of HGNC symbol of gene to be plotted
#             dsNames chr arr of dataset names which were used in analysis.r
#             norm    data.frame of cp$normalized.readcount
#             raw     data.frame of cp$readcount
#             bPolar  bool whether to make a radar plot. default is a bar plot
#             bNorm   bool whether to use normalized readcount
#             bApp    bool TRUE will adjust appearance for shiny app
# value       highcharter plot object
Plot_sgRNA_readcount <- function( gene, dsNames, norm, raw, 
                          bPolar = FALSE, bNorm = TRUE, bApp = TRUE , filename = NULL){

  catName <- "design"
  
  if( bNorm == TRUE ){ 
    df <- norm                                    
    tit <- paste("Normalized Readcount of sgRNAs for", gene)
    sub <- paste("For data sets" , paste(dsNames, collapse = ","))
    yl <- "normalized readcount"
  } else {
    df <- raw                                        
    tit <- paste("Readcount of sgRNAs for", gene)
    sub <- paste("For data sets" , paste(dsNames, collapse = ","))
    yl <- "readcount"
  }  
  
  if( bApp == TRUE ){
    tit <- ""
    sub <- ""
    ex <- TRUE
  } else {
    ex <- FALSE
  }
  
  df <- df[ df$gene == gene, ]
  file <- filename
  if( bPolar == TRUE ){
    p <- Plot_area( dsNames, catName, df, pol = TRUE, title = tit, subtitle = sub, export = ex , filename = file)
  } else {
    p <- Plot_column( dsNames, catName, df, title = tit, subtitle = sub, yLab = yl, export = ex , filename = file)
  }
  
  return(p)
}








################################
#### Plot_sgRNA_foldchanges ####
################################
# plot function for plotting replicates mean of normalized foldchanges of sgRNAs with highcharter
# necessary arguments are gene, compare, data
# arguments   gene    chr value of HGNC symbol of gene to be plotted
#             compare chr arr of names of both compared groups in analysis
#             data    data.frame of raw.genes
#             bApp    bool TRUE will adjust appearance for shiny app
#             bZratio bool whether z ratio instead of foldchange should be shown
#             bSort   bool whether data should be displayed sorted by value
# value       highcharter plot object
Plot_sgRNA_foldchanges <- function( gene, compare, data, 
                            bApp = TRUE, bZratio = FALSE, bSort = TRUE , filename = NULL){
  
  df <- data        

  if( bZratio == TRUE ){
    tit <- paste("Z ratio of sgRNAs for", gene)
    sub <- paste(compare, collapse = " vs. ")
    xl <- "sgRNA"
    yl <- "z ratio"
    dsName <- "z.ratio"
  } else {
    tit <- paste("Normalized Foldchange of sgRNAs for", gene)
    sub <- paste(compare, collapse = " vs. ")
    xl <- "sgRNA"
    yl <- "log2 of normalized foldchange"
    dsName <- "log2foldchange"
  }

  if( bApp == TRUE ){
    tit <- ""
    sub <- ""
    xl <- ""
    ex <- TRUE
  } else {
    ex <- FALSE
  }
   
  catName <- "designs"
  
  df <- df[df$genes == gene, ]
  df <- df[, c(catName, dsName)]
  
  if( bSort == TRUE ) df <- df[order(df[[dsName]], decreasing = TRUE), ]
  
  colours <- character(nrow(df))
  colours[which(as.numeric(df[[dsName]]) >= 0)] <- "#db3236"
  colours[which(as.numeric(df[[dsName]]) < 0)] <- "#4885ed"
  
  file <- filename
  
  p <- Plot_column( dsName, catName, df, title = tit, subtitle = sub, export = ex,
                      yLab = yl, xLab = xl, cols = colours, legend = FALSE , filename = file)
  
  return(p)
}








############################
#### Plot_sgRNA_zScores ####
############################
# plot function for plotting Z scores of sgRNAs of gene for compared groups
# necessary arguments are gene, compare, data
# arguments   gene    chr value of HGNC symbol of gene to be plotted
#             compare chr arr of names of both compared groups in analysis
#             data    data.frame of raw.genes
#             bApp    bool TRUE will adjust appearance for shiny app
#             bSort   bool whether data should be displayed sorted by value
# value       highcharter plot object
Plot_sgRNA_zScores <- function( gene, compare, data, bApp = TRUE, bSort = TRUE , filename = NULL){
  
  df <- data        

  tit <- paste("Z-Scores of sgRNAs for", gene)
  sub <- paste(compare, collapse = " vs. ")
  xl <- "sgRNA"
  yl <- "Z-Score"

  if( bApp == TRUE ){
    tit <- ""
    sub <- ""
    xl <- ""
    yl <- ""
    ex <- TRUE
  } else {
    ex <- FALSE
  }
   
  catName <- "designs"
  dsNames <- c("z.score.untreated", "z.score.treated", "z.score.foldchange")
  
  df <- df[df$genes == gene, ]
  df <- df[, c(catName, dsNames)]
  if( bSort == TRUE ) df <- df[order(df[["z.score.treated"]], decreasing = TRUE), ]
  
  dsNames <- c(compare,"log2 Foldchange")
  names(df) <- c("designs", dsNames)
  
  file <- filename
  
  p <- Plot_column( dsNames, catName, df, title = tit, subtitle = sub, export = ex,
                      yLab = yl, xLab = xl , filename = file)
  
  return(p)
}








############################
#### Plot_sgRNA_scores ####
############################
# plot function for plotting various scores of sgRNAs of gene for compared groups
# necessary arguments are gene, dsNames, data, type
# arguments   gene    chr value of HGNC symbol of gene to be plotted
#             dsNames chr arr of scores to be plotted as they appear in data as colnames
#             data    data.frame of raw.genes
#             type    chr "ecrisp" or "effic" control titles/subtitles
#             bPolar  bool whether to make a radar plot
#             bApp    bool TRUE will adjust appearance for shiny app
# value       highcharter plot object
Plot_sgRNA_scores <- function( gene, dsNames, data, type, 
                          bPolar = FALSE, bApp = TRUE , filename = NULL){

  catName <- "designs"
  df <- data                                    
  yl <- "score"
  
  if( type == "ecrisp" ){
    tit <- paste("E-CRISP sgRNA Scores for", gene)
    sub <- paste("All scores are derived from E-CRISP")
  } else if( type == "effic" ){
    tit <- paste("sgRNA Efficiency Scores for", gene)
    sub <- paste("All scores are derived from E-CRISP")
  }
  
  if( bApp == TRUE ){
    tit <- ""
    sub <- ""
    ex <- TRUE
  } else {
    ex <- FALSE
  }
  
  df <- df[ df$genes == gene, ]
  df <- df[, c(catName, dsNames)]
  
  map <- data.frame(
    from = c("Spec.Score", "Anno.Score", "Eff.Score", "CDS_score", 
           "exon_score", "seed_GC", "doench_score", "xu_score"),
    to = c("Specificity", "Annotation", "Efficiency", "CDS", "EXON", 
           "Seed_%GC", "DOENCH", "XU"),
  stringsAsFactors = FALSE)
  
  dsNames <- as.character(sapply(dsNames, function(x) map$to[map$from == x]))
  names(df) <- c(catName, dsNames)
  
  file <- filename
  
  if( bPolar == TRUE ){
    p <- Plot_area( dsNames, catName, df, pol = TRUE, title = tit, subtitle = sub, export = ex, filename = file )
  } else {
    p <- Plot_column( dsNames, catName, df, title = tit, subtitle = sub, yLab = yl, export = ex , filename = file)
  }
  
  return(p)
}








##############################
#### Plot_sgRNA_offtarget ####
##############################
# plot function for plotting bar chart with highcharter showing No of offtargets for each sgRNA of gene
# necessary arguments are gene, dsName, data
# arguments   gene    chr value of HGNC symbol of gene to be plotted
#             dsName  chr of value to be plotted as it appears in data
#             data    data.frame of raw.genes
#             bSort   bool whether to display sgRNAs sorted by offtargets
#             bPolar  bool whether to make a radar plot
#             bApp    bool TRUE will adjust appearance for shiny app
# value       highcharter plot object
Plot_sgRNA_offtarget <- function( gene, dsName, data,
                          bSort = TRUE, bPolar = FALSE, bApp = TRUE , filename = NULL){

  catName <- "designs"
  df <- data
  
  yl <- "number of predicted genomic binding sites"
  if( bApp == TRUE ){
    tit <- ""
    sub <- ""
    ex <- TRUE
  } else {
    tit <- paste("Predicted genomic binding sites for", gene)
    sub <- "Conditions: 5' unspecific leading bases = 1, allowed mismatches = 2"
    ex <- FALSE
  }
  
  df <- df[ df$genes == gene, ]
  
  file <- filename
  
  if( bSort == TRUE ) df <- df[order(df[[dsName]]), ]
  
  if( bPolar == TRUE ){
    p <- Plot_area( dsName, catName, df, pol = TRUE, title = tit, subtitle = sub,
                    export = ex, legend = FALSE , filename = file)
  } else {
    p <- Plot_column( dsName, catName, df, title = tit, subtitle = sub, 
                      yLab = yl, export = ex, legend = FALSE, filename = file )
  }
  
  return(p)
}





#####################
#### Table_blank ####
#####################
# creating a placeholder for DT datatables
# arguments   msg   chr for which message to be displayed as placeholder
#             col   font colour of msg
# value   DT datatable object
Table_blank <- function( msg = config$messages$noanalysisrunyet$String, col = "#37474F" ){
  df <- data.frame(data = msg)
  colnames(df) <- ""
  d <- datatable(df, class = "", rownames = FALSE, options = list(
    dom = "t", columnDefs = list(list(className = 'dt-center', targets = 0))
  )) %>%
    formatStyle(1, color = col, fontWeight = "bold")
  return(d)
}








##################
#### Table_DT ####
##################
# core function for rendering DT Datatables
# necessary argument is data
# arguments data        dataframe to be rendered as table
#           colNames    NULL or chr arr of same length as rows in data. if not NULL chr arr is
#                       taken as column names instead of column names of data
#           bRownames   bool whether to show rownames
#           bScroll     bool whether xScroll is enabled (horizontal)
#           style       chr defining style. currently there is only "default" and "bootstrap"
#           class       chr defining class. "display" is default, "stripe hover" is also nice
#                       there are many stlye combinations possible (DT website)
#           dom         chr for dom arguments. define table elements in order. f filtering, 
#                       r processing, t table, i information, p pagination, "frtip" is default
#           ordering    NULL or list of num arr and chr 'asc' or 'desc'. e.g. list(4, 'asc')
#           alignment   list of 3 elements centre, justify, and left. they can each be NULL(default) or
#                       a num arr for which cols to be aligned accordingly. other cols are right aligned
#           formatCurr  NULL or list of 2 elements cols (num arr) and curr (chr) for 
#                       currency formatting of cols columns
#           formatPerc  NULL or list of 1 element cols (num arr) for percentage formatting of cols columns
#           formatRoun  NULL or list of 2 elements cols (num arr) and digits (num) for rounding numbers
#                       of cols columns to number of digits
#           buttons     NULL or chr arr defining download buttons used in table ('copy', 'csv', 'excel', 
#                       'pdf', 'print') only works in DT development version
#           bResponsive bool whether responsive is activated: if table is to narrow, columns are excluded
#                       and there is a ugly plus symbol where they can be displayed by clicking
#           pageLen     num for how many rows are displayed
#           filename    chr filename for downloaded data
# value DT datatable object
#
Table_DT <- function( data, colNames = NULL, bRownames = FALSE, style = "default", class = "display", 
    dom = "flrtip", ordering = NULL, alignment = list(centre = NULL, justify = NULL, left = NULL), 
    formatCurr = NULL, formatPerc = NULL, formatRoun = NULL, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), bResponsive = FALSE,
    pageLen = 15, bScroll = FALSE, filename = "*" ){
  
  if( !is.null(colNames) ) names(data) <- colNames
  if( !is.null(buttons) ) dom <- paste0("B", dom)
  
  colDefs <- list()
  i <- 1
  if( !is.null(alignment$centre) ){
    colDefs[[i]] <- list(className = 'dt-center', targets = alignment$centre)
    i <- i + 1
  }
  if( !is.null(alignment$justify) ){
    colDefs[[i]] <- list(className = 'dt-justify', targets = alignment$justify)
    i <- i + 1
  }
  if( !is.null(alignment$left) ){
    colDefs[[i]] <- list(className = 'dt-left', targets = alignment$left)
    i <- i + 1
  }
  
  # set buttons with filename,but it does not work yet
  if(!is.null(buttons)){
    names(buttons) <- buttons
    buttons <- as.list(buttons)
    for(i in 1:length(buttons)){
      button <- list("extend" = names(buttons)[i], "text"=names(buttons)[i], "filename" = filename, "title" = filename)
      buttons[[i]] <- button
    }  
  }
  

  opts <- list( dom = dom, columnDefs = colDefs ,
                lengthMenu = list(c(5, 15, 50, 100, -1), c('5', '15', '50', '100', 'All')), 
                pageLength = pageLen, scrollX = bScroll)
  if( !is.null(ordering) ) opts[["order"]] <- ordering
  if( !is.null(buttons) ) opts[["buttons"]] <- list("copy","print", list("extend" = 'csv', "text"='csv', "filename" = filename, "title" = filename), list("extend" = 'excel', "text"='Excel', "filename" = filename, "title" = filename), list("extend" = 'pdf', "text"='pdf', "filename" = filename, "title" = filename))#buttons
  
  
  ext <- character(0)
  if( !is.null(buttons) ) ext <- c(ext, "Buttons")
  if( bResponsive == TRUE ) ext <- c(ext, "Responsive")
  
  d <- DT::datatable( data, rownames = bRownames, style = style, class = class, 
                      options = opts, extensions = ext )
    
  if( !is.null(formatCurr) ){
    d <- DT::formatCurrency(d, formatCurr$cols, currency = formatCurr$curr, interval = 3, mark = " ")
  }
  if( !is.null(formatPerc) ){
    d <- DT::formatPercentage(d, formatPerc$cols, digits = 2)
  }
  if( !is.null(formatRoun) ){
    d <- DT::formatRound(d, formatRoun$cols, digits = formatRoun$digits)
  }
  
  return(d)
}








#####################
#### Table_small ####
#####################
# create small, clean DT tables for sqStats server
# necessary argument data
# arguments data    object from results$statsGeneral
#           type    chr "basic" or "ctrl" for basic stats or controls
#           bApp    bool TRUE for DT datatable, FALSE for data.frame
# value     DT datatable object for bApp == TRUE, data.frame otherwise
Table_small <- function( data, type = "basic", bApp = TRUE , filename = "Table"){
  
  if( type == "basic" ){
    df <- data
    rn <- TRUE
    cn <- c("Mean", "Median", "Min", "Max", "SD", "Missing")
  } else if( type == "ctrl" ){
    df <- data
    dfn <- do.call("rbind", df)
    dfn[["Dataset"]] <- rep(names(df), each = nrow(df[[1]]))
    df <- dfn  
    rn <- FALSE
    cn <- c("Name", "Mean", "Median", "SD", "Min", "Max", "Dataset")
  }

  file <- filename
  
  if( nrow(df) > 10 ) dm <- "rtip" else dm <- "rt"

  if( bApp == TRUE ){
    d <- Table_DT( df, colNames = cn, bRownames = rn, dom = dm, class = "stripe hover" , filename = file)
  } else {
    d <- df
  }
  
  return(d)
}








###################
#### Table_big ####
###################
# create bigger, clean DT tables for sqStats server
# necessary argument data
# arguments data    object (list or data.frame) from results$statsGeneral or results$readcount
#           dataset NULL or chr defining element name of data, in case data is list of data.frames
#           bApp    bool TRUE for DT datatable, FALSE for data.frame
# value     DT datatable object for bApp == TRUE, data.frame otherwise
Table_big <- function( data, dataset = NULL, bApp = TRUE, filename = "Table" ){
  
  if( is.null(dataset) ){
    df <- data
    cn <- c("Design", colnames(df)[2:(ncol(df) - 1)], "Name")
  } else {
    df <- data[[dataset]]
    cn <- c("Mean", "Median", "SD", "Min", "Max", "Name")
  }
  
  file <- filename
  
  if( bApp == TRUE ){
    d <- Table_DT( df, colNames = cn, bRownames = FALSE, class = "stripe hover", filename = file )
  } else {
    d <- df
  }
  return(d)
}









##########################
#### Table_hcOverview ####
##########################
# creating datatables for hit calling candidates and performance for each method
# necessary arguments are data, bSign, thresh, direction, method, type
# arguments   data      data.frame of results$hitOverview
#             dir       chr "en" or "de" for enriched or depleted values
#             sign      list of elements named wilcox, deseq, mageck, rsea, edger. all are bool
#                       whether significant statement of relevant method is included
#             thresh    list of elements named wilcox, deseq, mageck, rsea, edger. all have num
#                       of respective method's p value threshold
#             bApp      bApp    bool TRUE for DT datatable, FALSE for data.frame
# value     DT datatable object for bApp == TRUE, data.frame otherwise
Table_hcOverview <- function( data, dir, sign, thresh, bApp = TRUE, filename ="*" ){
  
    if( dir == "en" ){
      data <- data.frame("Genes" = data$genes,
                 "FoldChange" = 2^data$log2FoldChange,
                 "PValueWilcox" = data$wilcox.pval,
                 "PValueDeseq" = data$deseq.pval,
                 "PValueMageck" = data$mageck.pval.enriched,
                 "PValueRsea" = data$rsea.enriched.pval,
                 "PValueEdger" = data$edger.pval,
                 "Direction" = data$edger.direction)
    } else {
      data <- data.frame("Genes" = data$genes,
                 "FoldChange" = 2^data$log2FoldChange,
                 "PValueWilcox" = data$wilcox.pval,
                 "PValueDeseq" = data$deseq.pval,
                 "PValueMageck" = data$mageck.pval.depleted,
                 "PValueRsea" = data$rsea.depleted.pval,
                 "PValueEdger" = data$edger.pval,
                 "Direction" = data$edger.direction)
    }
    rownames(data) <- data$Genes
  
    if( sign$wilcox == TRUE ){
      if( dir == "en" ){
          data <- data[data$FoldChange >= 1, ]
      } else {
          data <- data[data$FoldChange < 1, ]
      }
      data <- data[data$PValueWilcox < thresh$wilcox, ]
    }
  
    if( sign$deseq == TRUE ){
      if( dir == "en" ){
          data <- data[data$FoldChange >= 1, ]
      } else {
          data <- data[data$FoldChange < 1, ]
      }
      data <- data[data$PValueDeseq < thresh$deseq, ]
    }
    
    if( sign$mageck == TRUE ) data <- data[data$PValueMageck < thresh$mageck, ]
  
    if( sign$rsea == TRUE ) data <- data[data$PValueRsea < thresh$rsea, ]
  
    if( sign$edger == TRUE ){
      test <- switch(dir, en = "Up", de = "Down")
      data <- data[data$Direction == test, ]
      data <- data[data$PValueEdger < thresh$edger, ]
    }
    data$Direction <- NULL
    
  data$Genes <- NULL
  cn <- c("Fold Change", "Wilcox", "DESeq2", "MAGeCK", "sgRSEA", "EdgeR")
  orderCol <- 1
  
  file <- filename
  
  if( bApp == TRUE ){
    d <- Table_DT( data, colNames = cn, bRownames = TRUE, class = "stripe hover", 
        ordering = list(orderCol, 'desc'), formatRoun = list(cols = 1:6, digits = 4), bScroll = TRUE , filename = file)
  } else {
    d <- data
  }
  
  return(d)
}








#########################
#### Table_Candidate ####
#########################
# creating datatables for hit calling candidates for each method
# necessary arguments are data, bSign, thresh, direction, method
# arguments   data        data.frame of results$hitOverview
#             dir         chr "en" or "de" for enriched or depleted values
#             sign        list of elements named wilcox, deseq, mageck, rsea, edger. all are bool
#                         whether significant statement of relevant method is included
#             thresh      list of elements named wilcox, deseq, mageck, rsea, edger. all have num
#                         of respective method's p value threshold
#             direction   chr "en" or "de" for showing only enriched or depleted values
#             method      chr "wilcox", "deseq", "mageck", "sgrsea", "edger" for chosing analysis method
#             bApp        bApp not used yet
# value     DT datatable object
Table_hcCandidate <- function( data, bSign, thresh, direction, method, bApp = TRUE, filename ="*" ){
  
  cNames <- c("Foldchange", "P Value")
  orderCol <- 2
  roundCol <- 1
  mageckPval <- switch(direction, en = "mageck.pval.enriched", de = "mageck.pval.depleted")
  rseaPval <- switch(direction, en = "rsea.enriched.pval", de = "rsea.depleted.pval")
  idFC <- switch(method, wilcox = "wilcox.log2fc", "log2FoldChange")
  idPval <- switch(method, 
      wilcox = "wilcox.pval",
      deseq = "deseq.pval",
      mageck = mageckPval,
      rsea = rseaPval
  )
  
  if( method != "edger" ){
    df <- data.frame(fc = 2^data[[idFC]], pval = round(data[[idPval]], digits=6))
    rownames(df) <- data$genes
  } else {
    df <- data.frame(dir = data$edger.direction, fc = 2^data[[idFC]], pval = round(data$edger.pval, digits=6))
    sortID <- switch(direction, en = "Up", de = "Down")
    rownames(df) <- data$genes
    df <- df[df$dir == sortID, ]
    cNames <- c("Direction", "Foldchange", "P Value")
    orderCol <- 3
    roundCol <- 2
  }
  
  if( method == "deseq" || method == "wilcox" ){
    df <- switch(direction, en = df[df$fc >= 1, ], de = df[df$fc < 1, ]) 
  }

  if( bSign == TRUE ) df <- df[df$pval < thresh, ]  
  if( nrow(df) < 1 ) orderCol <- orderCol - 1
  
  file <- filename
  
  d <- Table_DT( df, colNames = cNames, bRownames = TRUE, class = "stripe hover", 
        ordering = list(orderCol, 'asc'), formatRoun = list(cols = roundCol, digits = 2) , filename = file)
  
  return(d)
}








######################
#### Table_hcPerf ####
######################
# creating datatables for hit calling performance for each method
# necessary arguments are data, bSign, thresh, direction, method
# arguments   data        data.frame of respective method
#             dir         chr "en" or "de" for enriched or depleted values
#             sign        list of elements named wilcox, deseq, mageck, rsea, edger. all are bool
#                         whether significant statement of relevant method is included
#             thresh      list of elements named wilcox, deseq, mageck, rsea, edger. all have num
#                         of respective method's p value threshold
#             direction   chr "en" or "de" for showing only enriched or depleted values
#             method      chr "wilcox", "deseq", "mageck", "sgrsea", "edger" for chosing analysis method
#             bApp        bool whether used inside App or for printing table
# value     DT datatable object of bApp == TRUE, data.frame of table otherwise
Table_hcPerf <- function( data, bSign, thresh, direction, method, bApp = TRUE, filename ="*" ){
  
  if( method == "wilcox" ){
    
    idFC <- "foldchange"
    idPval <- "p.value"
    data <- switch(direction, en = data[data[[idFC]] >= 1, ], de = data[data[[idFC]] < 1, ])
    if( bSign == TRUE ) data <- data[data[[idPval]] < thresh, ]
    df <- data[c(1, 2, 3, 5, 4)]
    cNames <- c("Untreated", "Treated", "Foldchange", "P-Value", "adjusted P-Value")
    orderCol <- 5
    roundCol <- 1:3
    df[,1:3] <- round(df[,1:3], digits=2)
    df[,4:5] <- round(df[,4:5], digits=5)
    
  } else if( method == "deseq" ){
    
    idFC <- "log2FoldChange"
    idPval <- "padj"
    data[[idFC]] <- 2^data[[idFC]]
    rownames(data) <- data$genes
    data <- switch(direction, en = data[data[[idFC]] >= 1, ], de = data[data[[idFC]] < 1, ])
    if( bSign == TRUE ) data <- data[data[[idPval]] < thresh, ]
    df <- data[1:6]
    cNames <- c("Base Mean", "Fold Change", "SE", "Stat", "P-Value", "adjusted P-Value")
    orderCol <-6
    roundCol <- 1:4
    df[,1:4] <- round(df[,1:4], digits=3)
    df[,5:6] <- round(df[,5:6], digits=5)
    
  } else if( method == "mageck" ){
    
    rownames(data) <- data$genes  
    data$genes <- NULL
    idPval <- switch(direction, en = "pos", de = "neg")
    if( bSign == TRUE ) data <- data[data[[idPval]] < thresh, ]
    df <- data[c(5, 6, 4, 2, 3, 1)]
    cNames <- c("Depleted sgRNA", "Enriched sgRNA", "Depleted Rank", "Enriched Rank",
                   "Depleted P-Value", "Enriched P-Value")
    orderCol <- switch(direction, en = 6, de = 5)
    roundCol <- NULL
    df[,5:6] <- round(df[,5:6], digits=4)
    
    
  } else if( method == "sgrsea" ){
    
    idData <- switch(direction, en = "gene.pos", de = "gene.neg")
    data <- as.data.frame(data[[idData]])
    idPval <- switch(direction, en = "FDR.pos", de = "FDR.neg")
    if( bSign == TRUE ) data <- data[data[[idPval]] < thresh, ]
    df <- data[c(1, 2, 5, 3, 4)]
    cNames <- c("M", "Score", "Rank", "P-Value", "adjusted P-Value")
    orderCol <- 5
    roundCol <- 2
    df[,c(4,5)] <- round(df[,c(4,5)], digits=4)
    
  } else if( method == "edger" ){
    
    idDir <- switch(direction, en = "Up", de = "Down")
    idPval <- "FDR"
    data <- data[data$Direction == idDir, ]
    if( bSign == TRUE ) data <- data[data[[idPval]] < thresh, ]
    df <- data[c(1, 2, 3, 4, 7, 8, 5, 6)]
    cNames <- c("N Gene", "Up", "Down", "Direction", "Mixed P Value",
                   "Mixed adjusted P-Value", "P-Value", "adjusted P-Value")
    orderCol <- 8
    roundCol <- 2:3
    df[,5:8] <- round(df[,5:8], digits=4)
    
  } else if( method == "bagel" ){
    
    idPval <- "BF"
    
    if( bSign == TRUE ) data <- data[data[[idPval]] > thresh, ]
    df <- data[c(1,2,3,4)]
    cNames <- c("Gene","log2 Bayes Factor","Standard Deviation","Bootstrap Iterations")
    orderCol <- 2
    roundCol <- 2:3
    df[,2:3] <- round(df[,2:3], digits=3)
    
  } else if( method == "screenbeam" ){
    
    idPval <- "FDR"
    
    if( bSign == TRUE ) data <- data[data[[idPval]] < thresh, ]
    df <- data[c(1,2,3,4,5,6)]
    cNames <- c("gene","n.sgRNAs.passed","B","Z","pval","FDR")
    orderCol <- 6
    roundCol <- 3:6
    df[,3:5] <- round(df[,3:5], digits=3)
    df[,6] <- round(df[,6], digits=5)
    
  }
  
  if( nrow(df) < 1 ) orderCol <- orderCol - 1
  
  file <- filename
  
  if( bApp == TRUE ){
    d <- Table_DT( df, colNames = cNames, bRownames = TRUE, class = "stripe hover", 
        ordering = list(orderCol, 'asc'), formatRoun = list(cols = roundCol, digits = 2) , filename = file)
  } else {
    d <- df
  }
  
  return(d)
}




###############################################
######## Sample Comparison Plot ###############
###############################################
# samplelist: list of combination as it is calculated by 
# type: genes or sgrna
# plot: scatter, column, boxplot
# sample: element of samplelist
# sorted: TRUE/FALSE
# info: log2foldchange or zscore
# drilldown: TRUE/FALSE -> drilldown function for genes REMOVED
# addline: TRUE/FALSE -> will add line of no change at all





highcharts.sampleComp <- function(samplelist = NULL, sample = NULL, type = "genes", plot = "scatter", sorted = TRUE, info = "log2fc" , drilldown = FALSE, theme = TRUE, addline = TRUE, filename = NULL)
{
  # samplelist: list of combination
  # type: genes or sgrna
  # plot: scatter, column, boxplot
  # sample: element of samplelist
  # sorted: TRUE/FALSE
  # info: log2foldchange or zratio
  # drilldown: TRUE/FALSE -> drilldown function for genes
  # addline: TRUE/FALSE -> will add line of no change at all
  
  if(is.null(samplelist))
  {stop("No sample data list provided.")}
  
  # Prepare stucture variables
  if(type == "genes")
  {
    id <- "gene"
  }
  if(type == "sgrna")
  {id <- "design"}
  
  
  if(plot == "scatter")
  {
    zoom <- "xy"
  }
  else
  {
    zoom <- "x"
  }
  
  
  # make names for tooltip
  if(info == "log2fc")
  {
    plotinfo <- "log2 Foldchange"
    line <- 0
  }
  if(info == "zratio" && type== "genes")
  {
    plotinfo <- "Z-Ratio"
    line <- 0
  } else {
    plotinfo <- "log2 Foldchange"
    info <- "log2fc"
    line <- 0
  }
  
  
  # Initialize highcharter
  hc <- highcharter::highchart()
  
  
  #### Start plotting Data
  # plot individual sample if sample is not NULL
  if(!is.null(sample))
  {
    if(type == "genes")
    {
      hc <- highcharter::hc_chart(hc, type=plot, zoomType = zoom)
    }
    else
    {
      hc <- highcharter::hc_chart(hc, type=plot, zoomType = zoom)
    }
    hc <- highcharter::hc_title(hc, text = sample)
    
    # Get sample
    df <- samplelist[names(samplelist) == sample][[1]][type][[1]]
    
    
    if(identical(sorted,TRUE))
    {
      # Sort according to type foldchange
      df <- df[order(df[,info], decreasing = FALSE),]
      
    }
    # get data to highcharts
    df.plot <- df[,c(id, info, "gene")]
    colnames(df.plot) <- c("name","y","gene")
    
    hc <- highcharter::hc_xAxis(hc, categories = df.plot$name)
    hc <- highcharter::hc_yAxis(hc, 
                                title = list(text = plotinfo),
                                lineWidth = 0,
                                min = min(df[,info]))

    
    # Add data
    hc <- highcharter::hc_add_series(hc, type = plot, name = plotinfo, data = df.plot, fillOpacity = 1, lineWidth=0)

    
    # add tooltip
    hc <- highcharter::hc_tooltip(hc, useHTML = TRUE,
                     pointFormat = paste("<span><h3>{point.gene}</h3></span><span>", plotinfo ,": <strong>{point.y}</strong></span><br/><span>Gene: <strong>{point.gene}</strong></span>"
                     ),
                     borderwidth=0,
                     delayForDisplay = 1500)
    
    
    ## ## Add drilldown if genes are plotted
    ## This means we will pass on sgRNA data and name it exaclty as the gene before, so wee need to put this in a loop for all genes!
    # if(type == "genes" && info == "log2fc")
    # {
    #   drilldownlist <- list()
    #   # we are in gene plotting mode, so we need to go and take the corresponding sgrna dataset
    #   for(u in 1:nrow(df))
    #   {
    #     
    #     df.sgrna <- samplelist[names(samplelist) == sample][[1]]["sgrna"][[1]]
    #     df.sgrna <- df.sgrna[df.sgrna$gene %in% df[u,"gene"],c("design", "log2fc", "gene")]
    #     colnames(df.sgrna) <- c("name", "y", "gene")
    #     # Add drilldown to information to list
    #     drilldownlist[[u]] = list(id = tolower(as.character(df[u,"gene"])), name=as.character(df[u,"gene"]), data = highcharter::list_parse(df.sgrna))
    #   }
    #   hc <- highcharter::hc_drilldown(hc,
    #                                   allowPointDrilldown = TRUE,
    #                                   series = drilldownlist)
    # }
    
    
    
    # Add 0 line
    if(addline)
    {
      df <- data.frame(name = df.plot$name,
                       y = rep.int(x = 0, times = nrow(df.plot)),
                       stringsAsFactors = FALSE)
      hc <- highcharter::hc_add_series(hc, type="line", name = "No Change", data = df)
    }
  }
  
  ## Plot boxplot if sample is null -> so everything is plotted
  if(is.null(sample))
  {
    hc <- highcharter::hc_chart(hc, type=plot, zoomType = zoom)
    
    hc <- highcharter::hc_title(hc, text = paste(info, "Distribution", sep=" "))
    #hc <- highcharter::hc_yAxis(hc, plotLines = list(value = 0, color= "red", width = 1 )) 
    
    ## add data
    for(i in 1:length(samplelist))
    {
      hc <- highcharter::hc_add_series_boxplot(hc, x=samplelist[[i]][[type]][,info], name=names(samplelist)[i], outlier=FALSE)
    }
    # boxplot change colors
    hc <- highcharter::hc_plotOptions(hc, boxplot = list(fillColor = "white", lineWidth = 1))
    hc <- highcharter::hc_xAxis(hc, title = list(text = "Dataset"))
    
  }
  
  ## Add highcharter layouts
  
  # add legend
  hc <- highcharter::hc_legend(hc, enabled = legend)
  
  # add exporting
  hc <- highcharter::hc_exporting(hc, enabled = TRUE,
                     printMaxWidth = 2000,
                     scale=8,
                     filename = filename,
                     formAttributes = list(target = "_blank"))
  
  
  # add google theme color
  if(theme)
  {
    hc <- highcharter::hc_add_theme(hc,hc_theme_google2())
  }
  
  # give back plotting function
  return(hc)
  
}


################################
### Principal component ananlysis plot
################################
# Derived from analysis.r
# will use pincomp class as input

Plot_pca <- function(pca = NULL, title = NULL, subtitle = NULL, export = TRUE, legend = TRUE, scale=8, filename = NULL) {
  
  shiny::incProgress(amount = 0.1, message = "Calculating Information")
  
  
  #hc <- highcharter::highchart() %>%
    hc <- highcharter::hchart(pca) %>%
    highcharter::hc_chart(zoomType="xy") %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = subtitle) %>%
    highcharter::hc_xAxis(title = list(text = "Principal Component 1")) %>%
    highcharter::hc_yAxis(title = list(text = "Principal Component 2")) %>%
    highcharter::hc_legend(enabled = legend) %>%
    highcharter::hc_exporting(enabled = export,
                              printMaxWidth = 2000,
                              #Width = 2000,
                              scale=scale,
                              filename = filename,
                              formAttributes = list(target = "_blank")) %>%
    highcharter::hc_size(width = NULL, height = 800) %>%
    highcharter::hc_add_theme(hc_theme_google2())
  shiny::incProgress(amount = 0.4, message = "Preparing Plot")
  return(hc)
  
}


#################################
#### Gene Overview Plots ########
#################################

## Plot of log2FC/ZRatios of genes used in screen
gene.plotdistribution <- function(data = NULL, gene = NULL , type = "log2fc", sgrna=FALSE, # or "zratio"
                                  tooltip = NULL, title = "", subtitle = "", xLab = "", yLab = "", zoom = "x",
                                  crosshair = TRUE, legend = TRUE, export = TRUE, cols = NULL, filename = NULL ){
  
  shiny::incProgress(amount = 0.2,detail = "Prepare Data")
  
  hc <- highcharter::highchart() %>%
    highcharter::hc_chart(type = "column", zoomType = zoom) %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = subtitle) %>%
    highcharter::hc_yAxis(title = list(text = yLab), align = "left", showFirstLabel = FALSE, 
             showLastLabel = FALSE, labels = list(useHTML = TRUE), opposite = FALSE) %>%
    highcharter::hc_legend(enabled = legend) %>%
    highcharter::hc_exporting(enabled = export,
                 printMaxWidth = 2000,
                 scale=8,
                 filename = filename,
                 formAttributes = list(target = "_blank"))
  shiny::incProgress(amount = 0.4,detail = "Creating Plot")
  if( is.null(tooltip) ){
    hc <- highcharter::hc_tooltip(hc, crosshairs = crosshair, shared = TRUE, borderWidth = 0, delayForDisplay = 1500)
  } else {
    hc <- highcharter::hc_tooltip(hc, crosshairs = crosshair, shared = TRUE, borderWidth = 0, delayForDisplay = 1500,
                     useHTML = TRUE, headerFormat = "<table>", pointFormat = tooltip, footerFormat = "</table>")
  }
  
  if(!sgrna)
  {
    if(type == "log2fc")
    {
      d <- as.data.frame(data)
      d <- d[order(d$log2FoldChange, decreasing = FALSE),]
      #set axis
      hc <- highcharter::hc_xAxis(hc, title = list(text = xLab), categories = d$genes, labels  = list(rotation = "-45"))
      # Sort DF from small to large
      
      # Set color for highlighting selected gene
      d$color <- "#BDBDBD"
      d[d$genes == gene,"color"] <- "#F44336"
      hc <- highcharter::hc_add_series_df(hc, data = d, type = "column", x=genes, y=log2FoldChange, colorByPoint=TRUE, colors = color, name="log2FC")
    } else {
      d <- as.data.frame(data[,c("gene", "zratio")])
      colnames(d) <- c("genes", "zratio")
      # Sort DF from small to large
      d <- d[order(d$zratio, decreasing = FALSE),]
      # Set axis
      hc <- highcharter::hc_xAxis(hc, title = list(text = xLab), categories = d$genes, labels  = list(rotation = "-45"))
      
      
      # Set color for highlighting selected gene
      d$color <- "#BDBDBD"
      d[d$genes == gene,"color"] <- "#F44336"
      hc <- highcharter::hc_add_series_df(hc, data = d, type = "column", x=genes, y=zratio, colorByPoint=TRUE, colors = color, name="zratio")
    }
    
    
  } else {
    #Set axis
    d <- as.data.frame(data[,c("sgRNA", "log2foldchange")]) #d <- as.data.frame(data[,c("sgRNA", "log2FoldChange")])
    # Sort DF from small to large
    d <- d[order(d$log2foldchange, decreasing = TRUE),]#d <- d[order(d$log2FoldChange, decreasing = TRUE),]
    hc <- highcharter::hc_xAxis(hc, title = list(text = xLab), categories = d$sgRNA, labels  = list(rotation = "-45"))
    
    
    hc <- highcharter::hc_add_series_df(hc, data = d, type = "column", x=sgRNA, y=log2foldchange, name="log2FC")
  }
  
  
  hc <- highcharter::hc_add_theme(hc, hc_theme_google2())
  
  
  return(hc)
}



################################
## Highcharts plot COSMIC data##
################################

cosmicdb <- function(genes = NULL, database="ENSEMBL_MART_ENSEMBL", dataset="homo_sapiens", host="www.ensembl.org", new.identifier = annos()$IDnew, COSMICdb = COSMIC, proxyurl = NULL, proxyport = NULL){
  
  # new.identifier to check whether genes contains HGNC_SYMBOL information, if this is not the case we need to convert it!
  if(new.identifier != "hgnc_symbol")
  {
    shiny::incProgress(amount = 0.1, detail = "Prepare Data")
    # check dataset
    if(dataset == "homo_sapiens") { dataset <- "hsapiens_gene_ensembl"}
    if(dataset == "mus_musculus") { dataset <- "mmusculus_gene_ensembl"}
    if(dataset == "dario_rerio") { dataset <- "drerio_gene_ensembl"}
    
    shiny::incProgress(amount = 0.1, detail = "Query biomaRt")
    #print("Start genomecrispr handling")
    # Get data from Biomart
    
    handling <- try(httr::with_config(httr::use_proxy(url = proxyurl, port = as.numeric(proxyport)),  
                                        biomaRt::useEnsembl(database, dataset, host, version = NULL, mirror = NULL, verbose = FALSE)
      ))
    
    #handling <- biomaRt::useEnsembl(database, dataset, host, version = NULL, mirror = NULL, verbose = FALSE)
    if(class(handling) == "try-error")
    {
      stop("biomaRt access failed")
    }
    
    # Call biomaRt
    shiny::incProgress(amount = 0.1, detail = "Get Gene Information")
    # gene.info <- biomaRt::getBM(
    #   filters = new.identifier,
    #   attributes = "hgnc_symbol",
    #   values = unique(genes),
    #   mart = handling)
    
   
    gene.info <- try(httr::with_config(httr::use_proxy(url = proxyurl, port = as.numeric(proxyport)),  
                                         biomaRt::getBM(
                                           filters = new.identifier,
                                           attributes = "hgnc_symbol",
                                           values = unique(genes),
                                           mart = handling)
    ))
    
    
    genes <- gene.info[1,1] # might be empty if no HGNC-symbol exists
  }
  shiny::incProgress(amount = 0.1, detail = "Get COSMIC information")
  
  # comisc database information is stored in COMISC
  # Following columns do exist
  # [1] "Gene name"                "Accession Number"         "Gene CDS length"          "HGNC ID"                  "Sample name"              "ID_sample"               
  #  [7] "ID_tumour"                "Primary site"             "Site subtype 1"           "Site subtype 2"           "Site subtype 3"           "Primary histology"       
  # [13] "Histology subtype 1"      "Histology subtype 2"      "Histology subtype 3"      "Genome-wide screen"       "Mutation ID"              "Mutation CDS"            
  # [19] "Mutation AA"              "Mutation Description"     "Mutation zygosity"        "LOH"                      "GRCh"                     "Mutation genome position"
  # [25] "Mutation strand"          "SNP"                      "Resistance Mutation"      "FATHMM prediction"        "FATHMM score"             "Mutation somatic status" 
  # [31] "Pubmed_PMID"              "ID_STUDY"                 "Sample source"            "Tumour origin"            "Age"                     
  #   
  
  # user input was HGNC Symbol, so we look for it
  #print(genes)
  data <- dplyr::filter(COSMICdb, `Gene name` == genes)
  
  
  ##### Prepare different outputs
  output <- list()
  
  # Data Table rdy output for Mutations
  output["mutation"] <- list(dplyr::select(data, `Gene name`, `Sample name`, `Primary site`, `Mutation AA`, `Mutation Description`, `Mutation genome position`, `Resistance Mutation`))
  output["hcmutation"] <- list(dplyr::select(data, `Mutation Description`, `Tumour origin`, `Primary site`, `Sample name`))
  
  
  return(output)
}




plot_cosmic <- function(data = NULL, plottype=NULL, title = "", subtitle = "", xlab ="", ylab = "", filename = NULL) {
  
  if(is.null(data) || nrow(data) <= 1 || is.null(plottype))
  {
    return(Plot_blank(device = "hc", msg= "No data available"))
    
  } else
  {
    data <- data[,plottype] # Subset
   
    hc <- highcharter::hchart(data[[plottype]], type="column") %>% 
      highcharter::hc_chart(zoomType="x", type="column") %>% 
      highcharter::hc_title(text = title) %>%
      highcharter::hc_subtitle(text = subtitle) %>%
      highcharter::hc_xAxis(title = list(text = xlab), labels  = list(rotation = "-90")) %>%
      highcharter::hc_yAxis(title = list(text = ylab), align = "left", showFirstLabel = FALSE, 
                            showLastLabel = FALSE, labels = list(useHTML = TRUE), opposite = FALSE) %>%
      highcharter::hc_legend(enabled = FALSE) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                printMaxWidth = 2000,
                                scale=8,
                                filename = filename,
                                formAttributes = list(target = "_blank")) %>%
      highcharter::hc_add_theme( hc_theme_google2())
    return(hc)
  }
  
  
}



#################################
#### Google Highcharts theme ####
#################################

hc_theme_google2 <- function(...){
  
  theme <-
    list(
      colors =  c("#F44336", "#2196F3", "#4CAF50", "#FFEB3B","#9C27B0","#03A9F4","#8BC34A","#FF9800","#3F51B5","#E91E63","#CDDC39","#FF5722"),
      chart = list(
        style = list(
          fontFamily = "Roboto",
          color = '#212121'
        )
      ),
      xAxis = list(
        gridLineWidth = 1,
        gridLineColor = '#F3F3F3',
        lineColor = '#F3F3F3',
        minorGridLineColor = '#F3F3F3',
        tickColor = '#F3F3F3',
        tickWidth = 1
      ),
      yAxis = list(
        gridLineColor = '#F3F3F3',
        lineColor = '#F3F3F3',
        minorGridLineColor = '#F3F3F3',
        tickColor = '#F3F3F3',
        tickWidth = 1
      ),
      plotOptions = list(
        boxplot = list(
          fillColor = "#FAFAFA"
        )
      ),
      
      legendBackgroundColor = 'rgba(0, 0, 0, 0.5)',
      background2 = '#505053',
      dataLabelsColor = '#B0B0B3',
      textColor = '#C0C0C0',
      contrastTextColor = '#F0F0F3',
      maskColor = 'rgba(255,255,255,0.3)'
    )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  } 
  
  theme
  
}

###############################
## get enrichr information ####
###############################

getenrichr <- function(gene.list = NULL, dataset=NULL, database = config$car.bm.database, host="www.ensembl.org", identifier = NULL,  db.list = c("WikiPathways_2016",
                                                                                                                         "KEGG_2016",
                                                                                                                         "Biocarta_2016",
                                                                                                                         "Reactome_2016",
                                                                                                                         "NCI-Nature_2016",
                                                                                                                         "Panther_2016",
                                                                                                                         "ChEA_2015",
                                                                                                                         "TRANSFAC_and_JASPAR_PWMs",
                                                                                                                         "ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X",
                                                                                                                         "TargetScan_microRNA",
                                                                                                                         "Transcription_Factor_PPIs",
                                                                                                                         "GO_Biological_Process_2015",
                                                                                                                         "GO_Cellular_Component_2015",
                                                                                                                         "GO_Molecular_Function_2015",
                                                                                                                         "OMIM_Disease",
                                                                                                                         "Cancer_Cell_Line_Encyclopedia",
                                                                                                                         "NCI-60_Cancer_Cell_Lines"
), fdr.cutoff = 0.05, proxurl = NULL, proxport = NULL)
{
  # check gene list
  # Make sure it has HGNC symbols to use
  if( identifier != "hgnc_symbol")
  {
    
    # call biomaRt to get hgnc_symbol
    if(dataset == "homo_sapiens") { dataset <- "hsapiens_gene_ensembl"}
    if(dataset == "mus_musculus") { dataset <- "mmusculus_gene_ensembl"}
    if(dataset == "dario_rerio") { dataset <- "drerio_gene_ensembl"}
    
    handling <- biomaRt::useEnsembl(database, dataset, host, version = NULL, GRCh = NULL, mirror = NULL, verbose = FALSE)
    if(!exists("handling"))
    {stop("biomaRt connection is not working. This can be a connectivity issue (e.g. proxy settings, internet connection) or the biomaRt service is currently not avaible.")}
    
    # check if filter is in attributes, if not we will add it.
    # also make sure it is unique
    
    # Call biomaRt
    gene.infos <- biomaRt::getBM(
      filters = identifier,
      attributes = "hgnc_symbol",
      values = unique(gene.list),
      mart = handling)
    
    # get new genelist
    gene.list <- gene.infos$hgnc_symbol
    
  }
  
  # get enrichr information
  enrichment <-  try(enrichGeneList(gene.list, db.list, fdr.cutoff, proxyurl = proxurl, proxyport = proxport))
  
  return(enrichment)
}


###############################
## get StringDB information ###
###############################

protein_interactions <- function(genes = NULL, graphplot = NULL, mapped = NULL, interactions = NULL, stringdb = NULL,database="ENSEMBL_MART_ENSEMBL", dataset="homo_sapiens", host="www.ensembl.org", new.identifier = annos()$IDnew , deseq2 = as.data.frame(results()$deseq$data$genes), threshold = 999, title = "", subtitle = "", database_path = config$database_path, interactive=TRUE, filename = "", cutscore = 100){
  if(is.null(genes) )
  {
    stop("No gene provided.")
  }
  
  if(dataset == "homo_sapiens") { dataset <- "hsapiens_gene_ensembl"}
  if(dataset == "mus_musculus") { dataset <- "mmusculus_gene_ensembl"}
  if(dataset == "dario_rerio") { dataset <- "drerio_gene_ensembl"}
  
  #print(genes)
  #print(str(genes))
  
  if(is.null(graphplot) || is.null(stringdb))
  {
    # set up stringDB
    string_db <- STRINGdb::STRINGdb$new( version="10", species=9606 , score_threshold=400, input_directory=database_path )
    graphplot <- string_db$get_graph()
    graphplot <- igraph::simplify(graphplot, remove.multiple=TRUE, remove.loops=TRUE)
  }
  
  shiny::incProgress(amount = 0.1, detail = "Generate StringDB (might take a while)")
  
  if(is.null(mapped))
  {
    # map genes provided
    genes <- string_db$mp(unique(genes))
  } else {
    genes <- mapped
  }
  
  
  shiny::incProgress(amount = 0.1, detail = "Get Involved Proteins")
  
  if(is.null(interactions))
  {
    # get neighbors and the interactions
    neighbors <- string_db$get_neighbors( c(genes) )
    shiny::incProgress(amount = 0.1, detail = "Get Protein Interactions")
    interactions <- string_db$get_interactions( c(neighbors, genes) )
  }
  
  
  #print(str(interactions))
  
  # filter those interactions in which the protein is directly involved
  interactions1 <- dplyr::filter(interactions, from == genes)
  interactions2 <- dplyr::filter(interactions, to == genes)

  
  
  interactions.all <- dplyr::anti_join(interactions1, interactions2, by = "from")
  
  #print(str(interactions.all))
  
  interactions2 <- dplyr::anti_join(interactions1, interactions2, by = "to")
  
  shiny::incProgress(amount = 0.1)
  
  interactions.all <- dplyr::bind_rows(interactions.all, interactions2)
  
  #print(str(interactions.all))
  
  # filter by score
  interactions.all <- dplyr::filter(interactions.all, combined_score >= threshold)
  

  #print(str(interactions.all))
  #print(nrow(interactions.all))
  
  shiny::incProgress(amount = 0.2)
  
  # check if Threshold is to high and we dont have any data left
  if(nrow(interactions.all) == 0)
  {
    return("threshold")
    stop("threshold")
  } else
  {
    # cut number of interactions by score
    if(!is.null(cutscore)){
      # just take the top 100 at maximum
      interactions.all <- dplyr::top_n(interactions.all, as.numeric(cutscore), combined_score)
    }
    
    # Create highcharts?
    if(identical(interactive, TRUE) && nrow(interactions.all) <= 100)
    {
      shiny::incProgress(amount = 0.1, message="Get stringDB graph information")
      
      
      # use stringdb for subsetting and put this to the graph
      graphplot2 <- igraph::induced.subgraph(graph=graphplot,vids=unique(interactions.all$to))
      
      interactions.call <- sub(pattern = "\\d+?\\.(.+)", replacement = "\\1", x = interactions.all$to, perl=TRUE)
      
      shiny::incProgress(amount = 0.1, message="Retrieve biomaRt information")
      
      handling <- biomaRt::useEnsembl(database, dataset, host, version = NULL, mirror = NULL, verbose = FALSE)
      # Call biomaRt
      #print(new.identifier)

      gene.info <- try(biomaRt::getBM(
        filters = "ensembl_peptide_id",
        attributes = c("ensembl_peptide_id", new.identifier),
        values = unique(interactions.call),
        mart = handling))
      # join it
      interactions.all <- data.frame("StringDB" = interactions.all$to, "ensembl_peptide_id" = interactions.call, stringsAsFactors = FALSE)
      
      interactions.all <- dplyr::left_join(interactions.all, gene.info, by = "ensembl_peptide_id")
      interactions.all <- interactions.all[interactions.all$StringDB == unique(interactions.all$StringDB),]
      
      
      # Add labels
      igraph::V(graphplot2)$label <- as.character(interactions.all[,3])
      #igraph::V(graphplot2)$color <- highcharter::colorize(deseq2[deseq2$genes %in% gene.info[,2], "log2FoldChange"])
      
      shiny::incProgress(amount = 0.2, message = "Creating Plot")
      
      # Create Highcharter plot
      hc <- highcharter::hchart(graphplot2, layout = igraph::layout_with_fr) %>%
        highcharter::hc_title(text = title) %>%
        highcharter::hc_subtitle(text = subtitle) %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_exporting(enabled = TRUE,
                                  printMaxWidth = 2000,
                                  scale=8,
                                  filename = filename,
                                  formAttributes = list(target = "_blank")) %>%
        highcharter::hc_tooltip(shared = TRUE, borderWidth = 0, delayForDisplay = 1500,
                                useHTML = TRUE, headerFormat = "<table>", pointFormat = shiny::tagList(
                                  shiny::tags$h3("{point.label}") ), footerFormat = "</table>") %>%
      highcharter::hc_add_theme( hc_theme_google2())
      
      return(hc)
      
    } else
    {
      # only create regular stringdb plot
      #network <- string_db$map(unique(genes), "gene", removeUnmappedRows = TRUE )
      shiny::incProgress(amount = 0.2, message = "Creating Plot")
      return(string_db$plot_network(interactions.all$StringDB) )
    }
  }
  
  
  
  
}




### Make CDF Plots
Plot_CDF <- function(data = NULL, filenames = extractedFiles()$gen_names, readtype = "gene", xlab = "", ylab = "", title = "", subtitle="", filename="")
{
  
  # prepare plot output
  if(readtype == "sgrna")
  {
    xlab = "Log2 Normalized sgRNA Read Counts"
    ylab = "Cumulative Frequency"
  } else {
    xlab = "Log2 Normalized Gene Read Counts"
    ylab = "Cumulative Frequency"
  }
  
  #print(str(data))
  
  # in highcharts
  library(highcharter)
  
  # Initialize plots
  hc <- highcharter::highchart() %>%  
    highcharter::hc_chart(type = "spine" ,zoomType="xy") %>%
    highcharter::hc_legend(enabled = TRUE)
  
  # add data
  for(i in 1:length(data))
  {
    if(readtype == "sgrna" && i != 1)
    {
      hc <- highcharter::hc_add_series(hc, name = filenames[i], data = data[[i]][[1]], type = "spline", visible = FALSE)
      
    } else {
      hc <- highcharter::hc_add_series(hc, name = filenames[i], data = data[[i]][[1]], type = "spline", visible = TRUE)
    }
    
  }
  
  
  # set highchart options
  hc <- highcharter::hc_title(hc, text = title) %>%
    highcharter::hc_subtitle(text = subtitle) %>%
    highcharter::hc_xAxis(title = list(text = xlab), crosshair = TRUE) %>%
    highcharter::hc_yAxis(title = list(text = ylab), crosshair = TRUE, align = "left", showFirstLabel = FALSE, 
                          showLastLabel = FALSE, labels = list(useHTML = TRUE), opposite = FALSE) %>%
    highcharter::hc_tooltip(enabled = FALSE, crosshairs = TRUE,
                            shared = TRUE, borderWidth = 0) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              printMaxWidth = 2000,
                              scale=8,
                              filename = filename,
                              formAttributes = list(target = "_blank")) %>%
  highcharter::hc_add_theme( hc_theme_google2())
  
  return(hc)
  
  
}



#### About page and Status
#

# check version

check_version <- function(url = "https://rawgit.com/boutroslab/CRISPRAnalyzeR/master/version.txt", proxyurl = NULL, proxyport = NULL, version = NULL)
{
  ## latest version available and version installed
    # get version info from GitHub
  if(!is.null(proxyurl) && !is.null(proxyport))
  {
     #getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
    #versionfile <- try(httr::with_config(httr::use_proxy(url = proxyurl, port = as.numeric(proxyport)), readr::read_tsv(file = url, col_names = FALSE)) )
    versionfile <- try(httr::with_config(httr::use_proxy(url = proxyurl, port = as.numeric(proxyport)), httr::GET(url)) )
  } else {
    versionfile <- try(httr::GET(url))
  }
    
    if(class(versionfile) != "try-error")
    {
      # Compare Version Info
      versionfile <- as.numeric(httr::content(versionfile))
      out <- paste("<span class='text'>Installed Version: <strong>", version , "</strong></span></br><span class='text'>The latest version is <strong>", versionfile ,"</strong></span>", sep="")
      # Output Version with notice if new version is available
    } else {
      
      # output installed version
      out <- paste("<span class='text'>Installed Version: <strong>", version , "</strong></span>", sep="")
    }
    
    return(out)
  
}

## Help Popover
helpPopup <- function(title, content,
                      placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#", class = "btn btn-mini", `data-toggle` = "popover",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      
      tags$i(class="icon-question-sign")
    )
  )
}


