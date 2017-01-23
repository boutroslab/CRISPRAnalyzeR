# car.genome
# used to call CLD RE-Evaluation as shown here:
# https://github.com/boutroslab/Supplemental-Material/tree/master/crispr-reannotation

car.genome <- function(outputdir=NULL, sequencefiles=NULL, databasepath=NULL, organism = NULL, nonseedlength = 1, mismatchesallowed = 1, reannotatescriptpath = NULL){
  
  # organism can be homo_sapiens, mus_musculus or dario_rerio
  if(is.null(organism))
  {
    organism <- "homo_sapiens"
  }
  
  
  # Write cp$libFile, which is now converted, to a fasta file that can be loaded to call CLD evaluation
  #oligos <- as.list(cp$libFILE$sequence)
  #names(oligos) <- cp$libFILE$design
  #seqinr::write.fasta(sequences = oligos,names = names(oligos), file.out = file.path(outputdir, sequencefiles))
  
  ## function to call perl with system2
  #unspecific_leading_bases = unspecific_bases,
  #edit_distance_allowed = edit_distance,
  #data_type = "fasta",
  #pasted_seq = sgrna.fasta,
  #PAM = cas,
  # make arguments
  args <- c(paste("--outputdir=", file.path(outputdir), sep=""),
            paste("--sequence-files=", file.path(outputdir, sequencefiles), sep=""),
            paste("--databasepath=", file.path(databasepath), sep=""),
            paste("--organism=", organism, sep=""),
            paste("--non-seed-length=", nonseedlength, sep=""),
            paste("--mismatches-allowed=", mismatchesallowed, sep=""),
            paste("2>",file.path(outputdir, "ecrisp.log"), sep= " ") ) # will catch the stderr to gather mapping information
  command <- paste("perl ", file.path(reannotatescriptpath, "reannotate_crispr.pl") , sep="")
  
  # make system call
  reannotate <- system2(command, args)
  #print(command)
  #print(args)
  #print(test)
  #perl reannotate_crispr.pl --output-dir=/home/b110-ws01/janwinter --sequence-files=libFile --databasepath=/data/DATABASEFILES/ --organism=homo_sapiens
  
  # Once finished, point to results table created, this lies in a logfile stored in the user directory
  # .*\/(.*?)\/primary_out.bwt.*$
  ## Open file ecrisp.log
  top <- readLines(con = file.path(outputdir, "ecrisp.log"))
  
  ## apply regex
  resultsfile <- sub(pattern = ".*\\/(.*?)\\/primary_out.bwt.*$", replacement = "\\1", x= top, perl=TRUE, fixed=FALSE)
  
  ## open resultsfile and convert it to ecrisp table as before with cargenome
  if(file.exists(file.path(outputdir,resultsfile,"results.tab")))
  {
    cp$ecrisp <- read.table(file.path(outputdir,resultsfile, "results.tab"), header=TRUE, sep="\t", comment.char = "", stringsAsFactors = FALSE, colClasses = c("character","character","numeric","numeric","character","numeric","numeric","numeric","character","character","character","numeric","numeric","numeric","numeric","numeric","character"))
    
    
  } else { stop(paste("CRISPR Re-Annotation failed. The Resultsfile", file.path(outputdir,resultsfile, "results.tab") ,"could not be found."))}
  
  ## Convert and Modify outputtable
  colnames(cp$ecrisp) <- c("design", "chr","Start", "End", "Gene.targets", "Spec.Score",
                           "Anno.Score","Eff.Score","Matchstring","Sequence","Direction","CDS_score",
                           "exon_score","seed_GC","doench_score","xu_score","doench_30_mer")
  
  if(identical(cp$miaccs$g.convert, TRUE) )
  {
    
    #ecrispresult$design <-
    sgrnas <- data.frame("design.old" = rownames(cp$readcount),
                         "design.new" = cp$readcount$design)
    colnames(sgrnas) <- c("design.old","design.new")
    #rownames(sgrnas) <- sgrnas$design
    
    cp$ecrisp <- merge(x = cp$ecrisp, y = sgrnas, by.x="design", by.y="design.old", all.x = TRUE)
    cp$ecrisp$design <- cp$ecrisp$design.new
    cp$ecrisp$design.new <- NULL
    cp$ecrisp$design.old <- NULL
    sgrnas <- NULL
    
  }
  cp$ecrisp <- merge.data.frame(cp$ecrisp, cp$readcount[,c("gene","design")], by.x = "design", by.y="design",all.x = TRUE, all.y=FALSE)
  
  #remove missing strand informaiton with *
  cp$ecrisp[cp$ecrisp$Direction == "","Direction"] <- "*"
  cp$ecrisp[cp$ecrisp$Direction == "fw","Direction"] <- "+"
  cp$ecrisp[cp$ecrisp$Direction == "rc","Direction"] <- "-"
  # make chromosomes work with gviz
  
  if(!grepl(pattern = "^chr.*" , x=cp$ecrisp$chr, perl=TRUE))
  {

    cp$ecrisp$chr <- sapply(cp$ecrisp$chr, function(x){
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
  
  # finished, return TRUE, cp environment needs to be saved afterwards and cp$ ecrisp stored separately
  return(TRUE)
  
}
