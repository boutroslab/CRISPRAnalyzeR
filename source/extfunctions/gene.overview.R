# gene.overview.R
# used for gene overview in CRISPRAnalyzeR
# contains of four function
# gene.annotation -> general gene overview
# gene.gviz -> used for gviz genomic View
# sgrna.gviz -> used for sgRNA Gviz
# genomecrispr -> used to call genomecrispr for annotations

#library(dplyr)
#library(httr)
#library(GenomicRanges)
#library(IRanges)
#library(RamiGO)


gene.annotation <- function(genes = NULL, database="ensembl", dataset="homo_sapiens", filter = NULL, attributes = NULL, host="www.ensembl.org", proxyurl = NULL, proxyport = NULL, genomecrispruri = NULL, GRCh = NULL, progresslog = NULL, workdir = getwd())
{
  #set working dir as backup
  # print(getwd())
  # # set options
  # 
  # if(!is.null(proxyurl) && !is.null(proxyport))
  # {
  #   options(RCurlOptions = list(
  #     proxy=paste(proxyurl, proxyport, sep=":"))
  #   )
  #   httr::config(proxy = proxyurl, proxyport = proxyport)
  # } else {
  #   options(RCurlOptions = list(
  #     proxy="")
  #   )
  #   httr::config(proxy = NULL, proxyport = NULL)
  # }
  # 
  # print(options())
  
  shiny::incProgress(amount = 0.2,detail = "Prepare Data")
  # check dataset
  if(dataset == "homo_sapiens") { dataset <- "hsapiens_gene_ensembl"}
  if(dataset == "mus_musculus") { dataset <- "mmusculus_gene_ensembl"}
  if(dataset == "dario_rerio") { dataset <- "drerio_gene_ensembl"}
  
  
  
  
  #system2("echo", args = c(genes, filter, attributes), stdout = "/tmp/id_geneannotation1")
  # progresslog: must be the path to the progress file
  
  # mainly expects genes as a vector of identiiers as set in filter argument.
  # attributes is also a vector of characters
  
  # prepare and check
  if(!is.null(genes) && !is.null(filter) && !is.null(attributes))
  {
    #print("Gene Annotation Handling")
    # Get data from Biomart
    shiny::incProgress(amount = 0.2,detail = "Access biomaRt")
  
    #system2("echo", args = c("Acces Biomart - ", paste(attributes, collapse = "-")), stdout = "/tmp/id_geneannotation2")
    
    if(length(attributes) > 2)
    {
      # make for loop with max 3 attributes
      annotation.list <- split(attributes, ceiling(seq_along(attributes)/2))
      reslist <- c(1:length(annotation.list))
      reslist <- as.list(reslist)
      
      filter_used <- FALSE
      
      # call biomart in loop
      for(i in 1:length(annotation.list)){
        handling <- try(biomaRt::useEnsembl(biomart=database, dataset = dataset, host = host, version = NULL, GRCh, mirror = NULL, verbose = FALSE))
        #handling <- try(biomaRt::useEnsembl(biomart = database, dataset, host, version = NULL, GRCh, mirror = NULL, verbose = FALSE))
        
        #system2("echo", args = c(getwd(), "-" , print(handling)), stdout = "/tmp/id_geneannotation3")
        
        if(class(handling) == "try-error")
        {
          #system2("echo", args = c(class(handling)), stdout = "/tmp/id_geneannotation4")
          stop("biomaRt connection is not working. This can be a connectivity issue (e.g. proxy settings, internet connection) or the biomaRt service is currently not avaible.")}
        
        # check if filter is in attributes, if not we will add it.
        # also make sure it is unique
        if(filter %in% annotation.list[[i]])
        {
          attributes <- unique(annotation.list[[i]])
        }
        else
        {
          attributes <- unique(c(filter,annotation.list[[i]]))  
        }
        
        #system2("echo", args = c(filter,attributes), stdout = "/tmp/id_geneannotation_handling2")
        
        gene.infos <- try(
          biomaRt::getBM(
            filters = filter,
            attributes = c(attributes),
            values = unique(genes),
            mart = handling)
        )
        
        #system2("echo", args = c(gene.infos), stdout = "/tmp/id_geneannotation_handling_geneinfos")
        # put result in list
          reslist[i] <- list(gene.infos)
        
        # merge lists into one single data frame using dplyr
        if(i==1){
          
          gene.info <- reslist[[i]]
        } else {
          
          gene.info <- dplyr::full_join(gene.info, reslist[[i]], by = filter)
        }
        
          #system2("echo", args = c(gene.info), stdout = "/tmp/id_geneannotation_handling_geneinfo")
      }
      
    }
    
    else {
      handling <- try(biomaRt::useEnsembl(biomart = database, dataset = dataset, host = host, version = NULL, GRCh, mirror = NULL, verbose = FALSE))
      #system2("echo", args = c(handling, "NO SPLIT"), stdout = "/tmp/id_geneannotation_handling")
      
      if(class(handling) == "try-error")
      {stop("biomaRt connection is not working. This can be a connectivity issue (e.g. proxy settings, internet connection) or the biomaRt service is currently not avaible.")}
      
      # check if filter is in attributes, if not we will add it.
      # also make sure it is unique
      if(filter %in% attributes)
      {
        attributes <- unique(attributes)
      }
      else
      {
        attributes <- unique(c(filter,attributes))
      }
      #system2("echo", args = c(filter,attributes), stdout = "/tmp/id_geneannotation_handling2")
    
      #print("Call Gene Annotation BiomaRt")
      shiny::incProgress(amount = 0.3, detail = "Retrieve data")
      # Call biomaRt
      gene.info <- biomaRt::getBM(
        filters = filter,
        attributes = c(filter,attributes),
        values = unique(genes),
        mart = handling)
      
    }
    
    #system2("echo", args = c(gene.info), stdout = "/tmp/id_geneannotation_handling_geneinfo")
    shiny::incProgress(amount = 0.2)
    ## Return
    return(unique(gene.info))
    
    # use proxy?
    if(!is.null(proxyuri) && !is.null(proxyport))
    {
      
    }
  } else
  {
    stop("Either no genes, filters or attributes have been passed on to the function.")
  }
  
}

### GVIZ MODEL plotting
# region defines the region arroung the gene to look at

gene.gviz <- function(genes = NULL, database="ensembl", dataset="homo_sapiens", filter = NULL, host="www.ensembl.org", proxyurl = NULL, proxyport = NULL, progresslog = NULL, region = 10000, data.only = FALSE, deseq=NULL)
{
  
  shiny::incProgress(amount = 0.1,detail = "Prepare Data")
  # check dataset
  if(dataset == "homo_sapiens") { dataset <- "hsapiens_gene_ensembl"
  genome <- "hg38"}
  if(dataset == "mus_musculus") { dataset <- "mmusculus_gene_ensembl"}
  if(dataset == "dario_rerio") { dataset <- "drerio_gene_ensembl"}
  
  # Get biomart gene region track and load data from cp$ecrisp if available
  
  # prepare and check
  if(is.null(genes) || is.null(filter))
  {
    stop("Either no genes, filters or attributes have been passed on to the function.")
  }
  shiny::incProgress(amount = 0.1,detail = "Query biomaRt")
  #print("Gene.Gviz Start")
  handling <- biomaRt::useEnsembl(biomart = database, dataset = dataset, host = host, version = NULL,  mirror = NULL, verbose = FALSE)
  if(class(handling) == "try-error")
  {stop("biomaRt connection is not working. This can be a connectivity issue (e.g. proxy settings, internet connection) or the biomaRt service is currently not avaible.")}
  
  attributes <- c("chromosome_name", "start_position", "end_position","strand")
  shiny::incProgress(amount = 0.1,detail = "Get Gene Information")
  # Call biomaRt
    gene.info <- biomaRt::getBM(
        filters = filter,
        attributes = c(filter,attributes),
        values = unique(genes),
        mart = handling)
    
  shiny::incProgress(amount = 0.1,detail = "Get Gene Region Information")
  # Get region infomration
  region.info <- biomaRt::getBM(
    filters = c("chromosomal_region"),
    attributes = c(filter,attributes),
    values = paste(unique(gene.info$chromosome_name),":",min(gene.info$start_position-region, na.rm=TRUE),":",max(gene.info$end_position+region, na.rm=TRUE), sep=""),
    mart = handling)
  
  # replace empty with "unknown
  region.info[region.info[,1] == "",1] <- "no identifier"
  
  region.info$color <- sapply(region.info[,1], function(x){
    if(x == genes) { return("red")}
    else { return("green")}
  })
  region.info$strand <- sapply(region.info$strand,function(x){
    if(x == 1) {return("+")}
    if(x == -1) {return("-")}
  })
  
  shiny::incProgress(amount = 0.1,detail = "Create Gene Model")
  #print("Itrack")
  # Make ideom Track
  itrack <- Gviz::IdeogramTrack(genome = genome, chromosome = paste("chr", unique(gene.info$chromosome_name), sep=""))
  #print(str(itrack))
  #print("Reftrack")
  # make gene overview
  
  ref.track <- Gviz::GenomeAxisTrack(GenomicRanges::GRanges(paste("chr", unique(gene.info$chromosome_name), sep=""), IRanges::IRanges(min((gene.info$start_position-region), na.rm=TRUE), max((gene.info$end_position+region), na.rm=TRUE))), lwd=4, fontsize=20)
  shiny::incProgress(amount = 0.2)
  gene.region <- GenomicRanges::makeGRangesFromDataFrame(region.info,
                                                         seqnames.field = "chromosome_name",
                                                         start.field = "start_position",
                                                         end.field = "end_position",
                                                         strand.field = "strand",
                                                         keep.extra.columns=TRUE,
                                                         ignore.strand=FALSE,
                                                         starts.in.df.are.0based=TRUE)

  GenomicRanges::elementMetadata(gene.region)$log2fc <- sapply(GenomicRanges::elementMetadata(gene.region)[,1], function(x){
    toreturn = NULL
    toreturn <- deseq$data$genes[deseq$data$genes$genes == as.character(x),"log2FoldChange"]
    if(!is.null(toreturn))
    {return(toreturn)} else {
      return(NA)
    }
  })
  GenomicRanges::elementMetadata(gene.region)$log2fc <- as.numeric(GenomicRanges::elementMetadata(gene.region)$log2fc)
  # make gene region custom
  #print("Make Gviz.Gene Generegiontrack")
  
  generegiontrack <- Gviz::AnnotationTrack(range=gene.region, genome = genome,
                                           stacking = "squish",
                                           #shape="box",
                                           name = "Genes",
                                           strand = gene.region$strand,
                                           id=region.info[,colnames(region.info)[1]]
  )
  
  Gviz::feature(generegiontrack) <- region.info$color
  shiny::incProgress(amount = 0.2)
  # make BiomartGeneregion for transcripts
  #print("biomTrack")
   
  biomTrack <-  Gviz::BiomartGeneRegionTrack(chromosome = paste("chr",unique(gene.info$chromosome_name), sep="",collapse=""),
                                                 start = min(region.info$start_position, na.rm=TRUE), end = max(region.info$end_position, na.rm=TRUE),
                                                 biomart = biomaRt::useEnsembl(biomart="ensembl", dataset=dataset),
                                                 name = "Transcripts",
                                                 genome = genome,
                                                 transcriptAnnotation = "symbol",
                                                 showExonId = FALSE,
                                                 showId=TRUE,
                                                 collapseTranscripts=TRUE,
                                                 shape="box",
                                                 stacking = "squish")
  
  # make datatrack for log2 FC and Z-Ratio of particular gene
  #print("dTrack")
  
  dtrack <- Gviz::DataTrack(range = gene.region,
                            strand = "*",
                            data = gene.region$log2fc,
                            type = "histogram",
                            name ="Log2 Foldchange")
  
  Gviz::feature(dtrack) <- region.info$color
  shiny::incProgress(amount = 0.1,detail = "Create Plot")
  # plot track and give back dataframe with all results
  return.list <- list("info" = gene.info, "region" = region.info, "iTrack" = itrack, "refTrack" = ref.track , "generegionTrack" = generegiontrack, "biomTrack" = biomTrack, "dTrack" = dtrack, "chromosome" = paste("chr", unique(gene.info$chromosome_name), sep="") )
  
  
  ## Return
  if(data.only == TRUE)
  {
    return(return.list)
  } else {
    Gviz::plotTracks(c(itrack, ref.track, generegiontrack, biomTrack, dtrack),  groupAnnotation = "id", red="darkred", green="grey")
  }
  
  
  # needs to be added to call later: groupAnnotation = "id", red="darkred", green="grey"
  
}

### Gene detailed with sgRNA
sgrna.gviz <- function(genes = NULL, database="ensembl", dataset="homo_sapiens", filter = NULL, host="www.ensembl.org", region = 1000, data.only=FALSE, deseq=NULL, readcount = NULL, ecrisp = NULL, GRCh = NULL, proxyurl = NULL, proxyport = NULL)
{
  
  #sgrna.gviz(genes = "BAX", database="ENSEMBL_MART_ENSEMBL", dataset="homo_sapiens", filter = "hgnc_symbol", host="www.ensembl.org", region = 1000, data.only=FALSE, deseq=out$deseq, readcount = out$readcount, ecrisp = out$ecrisp)
  shiny::incProgress(amount = 0.1, detail = "Prepare Data")
  # check dataset
  if(dataset == "homo_sapiens") { dataset <- "hsapiens_gene_ensembl"
  dataset.reg <- "hsapiens_regulatory_feature"
  dataset.anno <- "hsapiens_annotated_feature"
  dataset.motif <- "hsapiens_motif_feature"
  genome <- "hg38"}
  if(dataset == "mus_musculus") { dataset <- "mmusculus_gene_ensembl"
  dataset.reg <- "hsapiens_regulatory_feature"
  dataset.anno <- "hsapiens_annotated_feature"
  dataset.motif <- "hsapiens_motif_feature"
  }
  if(dataset == "danio_rerio") { dataset <- "drerio_gene_ensembl"
  }
  
  # Get biomart gene region track and load data from cp$ecrisp if available
  
  
  # prepare and check
  if(is.null(genes) || is.null(filter))
  {
    stop("Either no genes, filters or attributes have been passed on to the function.")
  }
  shiny::incProgress(amount = 0.1,detail = "Query biomaRt")
  #print("sgRNA.gviz start")
  # Get data from Biomart
    handling <- biomaRt::useEnsembl(biomart = database, dataset = dataset, host = host, version = NULL, GRCh, mirror = NULL, verbose = FALSE)
  
  if(class(handling) == "try-error")
  {stop("biomaRt connection is not working. This can be a connectivity issue (e.g. proxy settings, internet connection) or the biomaRt service is currently not avaible.")}
  
  #print("Call biomaRt")
  attributes <- c("chromosome_name", "start_position", "end_position","strand")
  shiny::incProgress(amount = 0.1,detail = "Get Gene Information")
  # Call biomaRt
    gene.info <- biomaRt::getBM(
        filters = filter,
        attributes = c(filter,attributes),
        values = unique(genes),
        mart = handling)
  
  
  ###
  
 
  gene.info$strand <- sapply(gene.info$strand,function(x){
    if(x == 1) {return("+")}
    if(x == -1) {return("-")}
    if(x!=1 && x!=-1) {return("*")}
  })
  
  # activate UCSC
  options(ucscChromosomeNames=TRUE)
  shiny::incProgress(amount = 0.1,detail = "Generate sgRNA Model")
  # Make ideom Track
  itrack <- Gviz::IdeogramTrack(genome = genome, chromosome = paste("chr", unique(gene.info$chromosome_name), sep=""))
  
  # make gene overview
  ref.track <- Gviz::GenomeAxisTrack(GenomicRanges::GRanges(paste("chr", unique(gene.info$chromosome_name), sep=""), IRanges::IRanges(min((gene.info$start_position), na.rm=TRUE), max((gene.info$end_position), na.rm=TRUE))), lwd=4, fontsize=20)
  gene.region <- GenomicRanges::makeGRangesFromDataFrame(gene.info,
                                                         seqnames.field = "chromosome_name",
                                                         start.field = "start_position",
                                                         end.field = "end_position",
                                                         strand.field = "strand",
                                                         keep.extra.columns=TRUE,
                                                         ignore.strand=FALSE,
                                                         starts.in.df.are.0based=TRUE)
  # Set the selected gene for highlighting later
  #elementMetadata(gene.region)[,1] <- sapply(elementMetadata(gene.region)[,1], function(x) { 
  #  if(x == genes)
  #  {return("GOI")} else {
  #    return(x)
  #  } 
  #  } )
  # Get log2 FC from deseq$data$genes
  GenomicRanges::elementMetadata(gene.region)$log2fc <- sapply(GenomicRanges::elementMetadata(gene.region)[,1], function(x){
    toreturn = NULL
    toreturn <- deseq$data$genes[deseq$data$genes$genes == as.character(x),"log2FoldChange"]
    if(!is.null(toreturn))
    {return(toreturn)} else {
      return(NA)
    }
  })
  shiny::incProgress(amount = 0.1)
  GenomicRanges::elementMetadata(gene.region)$log2fc <- as.numeric(GenomicRanges::elementMetadata(gene.region)$log2fc)
  # make gene region custom
  generegiontrack <- Gviz::AnnotationTrack(range=gene.region, genome = genome,
                                           stacking = "squish",
                                           #shape="box",
                                           name = "Genes",
                                           strand = gene.region$strand,
                                           id=gene.info[,colnames(gene.info)[1]],
                                           groupAnnotation = "id"
  )
  
  # make BiomartGeneregion for transcripts
  shiny::incProgress(amount = 0.1)

    biomTrack <- Gviz::BiomartGeneRegionTrack(chromosome = paste("chr",unique(gene.info$chromosome_name), sep="",collapse=""),
                                   start = min(gene.info$start_position, na.rm=TRUE), end = max(gene.info$end_position, na.rm=TRUE),
                                   biomart = biomaRt::useEnsembl(biomart="ensembl", dataset=dataset),
                                   name = "Transcripts",
                                   genome = genome,
                                   transcriptAnnotation = "symbol",
                                   showExonId = FALSE,
                                   showId=TRUE,
                                   collapseTranscripts=FALSE,
                                   shape="box",
                                   stacking = "squish")
      
  #}
  
  # make sgRNAs 
  sgrnatrack <- NULL
  dtrack <- NULL
  featureTrack <- NULL
  motifTrack <- NULL
  #print("Set sgrna.gviz tracks to NULL")
  if(exists("ecrisp") && dataset != "drerio_gene_ensembl")
  {
    #print("sgrna.ranges")
    shiny::incProgress(amount = 0.1)
    deseq.sgrna <- as.data.frame(deseq$data$sgRNA)
    sgrna.ranges <- GenomicRanges::makeGRangesFromDataFrame(ecrisp[ecrisp$design %in% readcount[readcount$gene == genes,"design"] & ecrisp$chr == paste("chr",unique(gene.info$chromosome_name), sep=""),], 
                                                            seqnames.field = "chr",
                                                            start.field = "Start",
                                                            end.field = "End",
                                                            strand.field = "Direction",
                                                            keep.extra.columns=TRUE,
                                                            ignore.strand=TRUE,
                                                            starts.in.df.are.0based=TRUE)
    
    #print("Add log2FC")
     GenomicRanges::elementMetadata(sgrna.ranges)$log2fc <- sapply(GenomicRanges::elementMetadata(sgrna.ranges)$design, function(x) {
      return(deseq.sgrna[deseq.sgrna$sgRNA == x,"log2FoldChange"])
    })
    # sgRNAs
    #print("sgrna track")
    
    sgrnatrack <- Gviz::AnnotationTrack(range=sgrna.ranges, chromosome = paste("chr", unique(gene.info$chromosome_name), sep=""),
                                        stacking = "squish",
                                        #shape="box",
                                        name = "sgRNAs",
                                        genome = genome,
                                        id=GenomicRanges::elementMetadata(sgrna.ranges)$design,
                                        showFeatureId=FALSE
                                        
    )
    shiny::incProgress(amount = 0.1)
    # log2 FC plot
    #print("dtrack")
    dtrack <- Gviz::DataTrack(range = sgrna.ranges, chromosome = paste("chr", unique(gene.info$chromosome_name), sep=""),
                              type = c("p","g"),
                              genome = genome,
                              data = GenomicRanges::elementMetadata(sgrna.ranges)$log2fc
    )
    # regulatory features
    #print("annontate features")
    ensembl = biomaRt::useEnsembl(biomart="regulation", dataset=dataset.reg)
    annotate.features <- biomaRt::getBM(c("feature_type_name","chromosome_start","chromosome_end"),
                                        filters = c("chromosome_name","start","end"),
                                        #filters = c("chromosome_name"),
                                        values = list(unique(gene.info$chromosome_name),unique(gene.info$start_position),unique(gene.info$end_position)),
                                        #values = gene.load$chromosome_name,
                                        mart = ensembl)
    
    #sort
    annotate.features <- annotate.features[order(annotate.features$feature_type_name),]
    shiny::incProgress(amount = 0.1)
    featureTrack <- list()
    feature.color <- c("darkred","blue","red2","yellow","green")
    #print("feature track")
    featureTrack <- Gviz::AnnotationTrack(feature = annotate.features[, "feature_type_name"], chromosome = paste("chr", unique(gene.info$chromosome_name), sep=""), 
                                          start = annotate.features[,"chromosome_start"], 
                                          end = annotate.features[,"chromosome_end"], 
                                          name="Regulation", genome=genome, shape="box",
                                          showFeatureId=FALSE,
                                          "Promoter" = "darkred",
                                          "CTCF Binding Site" = "blue", 
                                          "Promoter Flanking Region" = "red2", 
                                          "Enhancer" = "yellow",                
                                          "Open chromatin" = "green",
                                          #showOverplotting=TRUE,
                                          group = annotate.features$feature_type_name,
                                          showId = TRUE)
    # Annotatetion
    #ensembl = biomaRt::useEnsembl(biomart="regulation", dataset=dataset.anno)
    
    
    # Motifs
    #print("motif track")
    
    ensembl <- biomaRt::useEnsembl(biomart="regulation", dataset=dataset.motif)
    
    ensembl <- biomaRt::useEnsembl(biomart="regulation", dataset=dataset.motif)
    
    
    motif.features <- biomaRt::getBM(c("binding_matrix_id", "display_label", "score", "feature_type_name","chromosome_start","chromosome_end"),
                                      filters = c("chromosome_name","start","end"),
                                      #filters = c("chromosome_name"),
                                      values = list(unique(gene.info$chromosome_name),min(gene.info$start_position, na.rm=TRUE), min(gene.info$end_position, na.rm=TRUE)),
                                      #values = gene.load$chromosome_name,
                                      mart = ensembl)
    
    motifTrack <- Gviz::AnnotationTrack(feature = motif.features[, "feature_type_name"], chromosome = paste("chr", unique(gene.info$chromosome_name), sep=""), 
                                        start = motif.features[,"chromosome_start"], 
                                        end = motif.features[,"chromosome_end"], 
                                        name="Binding Motifs", genome=genome, shape="box",
                                        showFeatureId=FALSE, 
                                        #showOverplotting=TRUE,
                                        group = motif.features$feature_type_name,
                                        showId = TRUE)
  }
  shiny::incProgress(amount = 0.1)
  # Return data
  return.list <- list("info" = gene.info, "gene" = gene.region, "motif" = motif.features, "regulation" = annotate.features, "iTrack" = itrack, "generegionTrack" = generegiontrack, "refTrack" = ref.track, "biomTrack" = biomTrack, "sgrnaTrack" = sgrnatrack, "dTrack" = dtrack, "featureTrack" = featureTrack, "motifTrack" = motifTrack, "from" = min((gene.info$start_position-region), na.rm = TRUE), "to" = min((gene.info$end_position+region), na.rm=TRUE), "chromosome" = paste("chr", unique(gene.info$chromosome_name), sep=""))
  # return.plot <- plotTracks(c(itrack, ref.track, generegiontrack, biomTrack),  groupAnnotation = "id", red="darkred", green="grey")
  
  ## Return
  if(data.only==TRUE)
  {
    return(return.list)
  } else {
    Gviz::plotTracks(c(itrack, generegiontrack, ref.track, biomTrack, sgrnatrack, dtrack, featureTrack, motifTrack), from=min((gene.info$start_position-region), na.rm = TRUE), to = max((gene.info$end_position+region), na.rm=TRUE), chromosome = paste("chr", unique(gene.info$chromosome_name), sep=""))
  }
  
  
  # needs to be added to call later: groupAnnotation = "id", red="darkred", green="grey"
  
}

#### GENOMECRISPR
# GenomeCRISPR requires HGNC_SYNBOL, we we use biomart to get it in case gene.identifier.new is NOT HGNC_SYMBOL
# Get Data from GenomeCRISPR
# what we want:
# - sgRNAs used for this gene
# - in which screens was this gene adressed
# - which phenotype did come up?
# - URL to genomecrispr

# Prepare Output
# merge genomecrispr with gene.info



genomecrispr <- function(genes = genes, database="ensembl", dataset="homo_sapiens", host="www.ensembl.org", new.identifier = cp$miaccs$g.identifier.new, readcount=NULL, ecrisp = NULL, proxyurl = NULL, proxyport = NULL){
  
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
    if(!is.null(proxyport) && !is.null(proxyport))
    {
      handling <- try(httr::with_config(httr::use_proxy(url = proxyurl, port = proxyport),  
                                        biomaRt::useEnsembl(biomart = database, dataset = dataset, host = host, version = NULL, mirror = NULL, verbose = FALSE)
      ))
    } else {
      handling <- try(httr::with_config(httr::use_proxy(url = proxyurl, port = proxyport),  
                                        biomaRt::useEnsembl(biomart = database, dataset = dataset, host = host, version = NULL, mirror = NULL, verbose = FALSE)
      ))
    }
      
   
    #handling <- biomaRt::useEnsembl(biomart = database, dataset = dataset, host = host, version = NULL, mirror = NULL, verbose = FALSE)
    # Call biomaRt
    shiny::incProgress(amount = 0.1, detail = "Get Gene Information")
      gene.info <- try(httr::with_config(httr::use_proxy(url = proxyurl, port = proxyport),  
                                         biomaRt::getBM(
                                           filters = new.identifier,
                                           attributes = "hgnc_symbol",
                                           values = unique(genes),
                                           mart = handling)
      ))
    # gene.info <- biomaRt::getBM(
    #   filters = new.identifier,
    #   attributes = "hgnc_symbol",
    #   values = unique(genes),
    #   mart = handling)
    
    genes <- gene.info[1,1]
  }
  shiny::incProgress(amount = 0.1, detail = "Call GenomeCRISPR")
  # Call genomeCRISPR tp get all information for that particular gene
  #print("Call genomecrispr DB for gene information")
  
  if(!is.null(proxyport) && !is.null(proxyport))
  {
    r <- try(httr::with_config(httr::use_proxy(url = proxyurl, port = proxyport),  
                               httr::POST('http://genomecrispr.dkfz.de/api/sgrnas/symbol', 
                                          body=list(query=genes),  # genes
                                          encode='json' 
                               )
    ))
  } else {
    r <- try(
              httr::POST('http://genomecrispr.dkfz.de/api/sgrnas/symbol', 
                                          body=list(query=genes),  # genes
                                          encode='json' 
              )
    )
  }
  
    
  
  # r <- httr::POST('http://genomecrispr.dkfz.de/api/sgrnas/symbol', 
  #                 body=list(query=genes),  # genes
  #                 encode='json' 
  # )
  
  output <- list()
  
  if(length(httr::content(r)) == 0 || !grepl(pattern =".*pubmed.*", x = httr::content(r), perl=TRUE))
  {
    output <- list(
      "sgrnas" = NA,
      "genes" = NA,
      "sgrnas2" = NA
    )
  } else
  {
    shiny::incProgress(amount = 0.1)
    #print("convert gene information")
    ## dplyr method bind_rows is much faster than do.call('rbind') but does not work here for some weird reason.
    
    sgrnas <- httr::content(r) %>% lapply(function(x) list(pubmed=x$pubmed, cellline=x$cellline, condition=x$condition, hit=x$hit, sequence=x$sequence)) %>% bind_rows
    # get Publications
    
    output <- list("sgrnas" = sgrnas)
    output$genes <- list()
    for(i in 1:length(unique(sgrnas$pubmed)))
    {
      
      if(!is.null(proxyport) && !is.null(proxyport))
      {
        author <- try(httr::with_config(httr::use_proxy(url = proxyurl, port = proxyport),  
                                        httr::POST('http://genomecrispr.dkfz.de/api/experiments/publication', 
                                                   body=list(id=as.character(unique(sgrnas$pubmed))[i]), #
                                                   encode='json' 
                                        )
        ))
      } else {
        author <- try(
                    httr::POST('http://genomecrispr.dkfz.de/api/experiments/publication', 
                                                   body=list(id=as.character(unique(sgrnas$pubmed))[i]), #
                                                   encode='json' 
                    )
        )
      }
      
       
      
      # author <- httr::POST('http://genomecrispr.dkfz.de/api/experiments/publication', 
      #                      body=list(id=as.character(unique(sgrnas$pubmed))[i]), #
      #                      encode='json' 
      # )
      author.screens <- httr::content(author) %>% lapply(function(x) list(pubmed=x$pubmed, title=x$title, abstract=x$abstract)) %>% bind_rows
      output$genes[i] <- list(author.screens)
    }
    
  # now we have all piublications in a list format in gene.publications for a particular gene
  # and all sgRNA information in sgrnas for that particular gene
    shiny::incProgress(amount = 0.1)
    
    # Call genomeCRISPR tp get all information for sgRNAs used for that gene
    # get all used sgrnas in the setting from cp$readcount and cp$libFILE
    # get sgRNA identifier for that gene
    sgrna.id <- as.character(readcount[readcount$gene == genes,"design"])
    
    #print("Call genomecrispr DB forsgrnas information")
    # get sgrna sequence
    sgrna.seq <- toupper(ecrisp[ecrisp$design %in% sgrna.id,"Sequence"])
    
    if(!is.null(proxyport) && !is.null(proxyport))
    {
      r <- try(httr::with_config(httr::use_proxy(url = proxyurl, port = proxyport),  
                                 httr::POST('http://genomecrispr.dkfz.de/api/sgrnas/sequence', 
                                            body=list(sequence=as.character(unique(sgrna.seq))),
                                            encode='json' 
                                 )
      ))
    } else {
      r <- try(
                                 httr::POST('http://genomecrispr.dkfz.de/api/sgrnas/sequence', 
                                            body=list(sequence=as.character(unique(sgrna.seq))),
                                            encode='json' 
                                 )
      )
    }
    
      
    
    # r <- httr::POST('http://genomecrispr.dkfz.de/api/sgrnas/sequence', 
    #                 body=list(sequence=as.character(unique(sgrna.seq))),#as.character(unique(sgrna.seq))), "CCAAATACTCCACACGCAAATTT"
    #                 encode='json' 
    # )
    
    shiny::incProgress(amount = 0.2)
    
    # Check if meaningful data comes back
    if(grepl(pattern =".*pubmed.*", x = httr::content(r), perl=TRUE ))
    {
      output$sgrnas2 <- httr::content(r) %>% lapply(function(x) list(log2fc=x$log2fc, pubmed=x$pubmed, condition=x$condition, score=x$score, scoredist=x$scoredist, hit=x$hit)) %>% bind_rows
    } else {
      output$sgrnas2 <- NA 
      
      #if()
      
    }
    
  }
 
  
  # Return to shiny
  # output$sgrnas <- has information about sgRNAs for that gene present in other screens: condition, pubmed, cellline, hit, sequence
  # output$genes <- lis with information about that gene: pubmed, title of publication, abstract
  # output$sgrnas2 <- list with information about sgRNAs used by the user: log2fc, pubmed, condition, score, scoredist, hit
  return(output)
}




###### GO Terms

goterms <- function(genes = genes, database="ensembl", dataset="homo_sapiens", host="www.ensembl.org", filter = NULL, userdir = userDir, proxyurl = NULL, proxyport = NULL){
  
 
    shiny::incProgress(amount = 0.1, detail = "Prepare Data")
    # check dataset
    if(dataset == "homo_sapiens") { dataset <- "hsapiens_gene_ensembl"}
    if(dataset == "mus_musculus") { dataset <- "mmusculus_gene_ensembl"}
    if(dataset == "dario_rerio") { dataset <- "drerio_gene_ensembl"}
    
    shiny::incProgress(amount = 0.1, detail = "Query biomaRt")
    #print("Start genomecrispr handling")
    # Get data from Biomart
    handling <- biomaRt::useEnsembl(biomart = database, dataset = dataset, host = host, version = NULL, mirror = NULL, verbose = FALSE)
    # Call biomaRt
    shiny::incProgress(amount = 0.1, detail = "Get Gene Information")
    
    attributes <- c("go_id","name_1006","definition_1006","go_linkage_type","namespace_1003")
    
    gene.info <- try(biomaRt::getBM(
      filters = filter,
      attributes = c(filter,attributes),
      values = unique(genes),
      mart = handling))
   
    if(class(gene.info) == "try-error")
    {
      stop("biomaRt query failed.")
    }
    
    go.namespace <- unique(gene.info$namespace_1003)
    go.namespace <- go.namespace[go.namespace !=""]
    
    shiny::incProgress(amount = 0.2, detail = "Prepare GO Terms")
    # create output in a way that it provides a list that seperates what we want to see:
    # first: seperated by NAMESPACE to see the functional class -> is always either molecular_function, biological_process or cellular_component
    # then by name (unique terms) with ALL experimental GO_linkage_types
    if(length(go.namespace) <=1)
    {
      return(list("table" = NA, "gene.info" = NA))
    } else
    {
      go.list <- as.list(go.namespace) # make list
      names(go.list) <- go.namespace
      # next level, we add unique function of each
      for(i in 1:length(go.list))
      {
        # get stuff from gene.info
        df <- dplyr::filter(gene.info, namespace_1003 == go.list[[i]])
        df_name <- unique(df$name_1006)
        df_name <- as.data.frame(df_name[df_name != ""], stringsAsFactors=FALSE)
        colnames(df_name) <- "name_1006"
        
        df_name$go_id <- sapply(df_name$name_1006, function(x) {
          getgo <- gene.info[gene.info$name_1006 %in% as.character(x),"go_id"]
          getgo <- paste(getgo, sep="", collapse = ";")
          return(as.character(getgo))
        })
        
        df_name$go_linkage_type <- sapply(df_name$name_1006, function(x) {
          getgo <- gene.info[gene.info$name_1006 %in% as.character(x),"go_linkage_type"]
          getgo <- paste(getgo, sep="", collapse = ";")
          return(as.character(getgo))
        })
        
        # add to go_list for return
        go.list[i] <- list(df_name)
      }
      
      shiny::incProgress(amount = 0.2)
      # return list with two things
      return(list("table" = go.list, "gene.info" = gene.info))
    }
   
}

goview <- function(golist = NULL, term = NULL, userdir = userDir){
  
  if(is.null(golist) || is.null(term) || is.na(golist$table) || is.na(golist$gene.info))
  {
    return(NA)
  }
  
  
  shiny::incProgress(amount = 0.1, detail = "Prepare Data")
  # make graph for each namespace and an overview of ALL
  go.plot <- as.list(term)
  names(go.plot) <- term
  
  for(i in 1:length(go.plot))
  {
    # get all GO-IDs and color them blue
    shiny::incProgress(amount = 0.1)
    # call RamiGO
    goIDs <- golist$gene.info[golist$gene.info$namespace_1003 == names(go.plot)[i],"go_id"]
    # set color to lightblue for all so we can identify them later
    goColor <- rep.int(x = "lightblue", times = length(goIDs))
    pngRes <- RamiGO::getAmigoTree(goIDs=goIDs, color=goColor, filename = file.path(userdir, paste("go_",names(go.plot)[i], ".png", sep="")), picType="png", saveResult=TRUE)
    
    go.plot[i] <- list(filepath = file.path(userdir, paste("go_",names(go.plot)[i],".png", sep="")))
    
  }
  return(list("plot" = go.plot))
}



#### KEGG


getkegg <- function(entrezgene = NULL, proxyurl = NULL, proxyport = NULL){
  
  ## Now we ask KEGG via KEGGREST for additional information
  ## We need: gene as ENTREZGENE ID and the organism (maybe)
  ## Return will be as additional lit in output$kegg
  
  if(is.null(entrezgene))
  {
    return(NULL)
  }
  if(!is.null(proxyurl) && !is.null(proxyport))
  {
    kegggene <- httr::with_config(httr::use_proxy(url = proxyurl, port = proxyport), KEGGREST::keggConv("genes", paste("ncbi-geneid:", as.character(entrezgene) , sep="") ))
    
    keggquery <- httr::with_config(httr::use_proxy(url = proxyurl, port = proxyport), KEGGREST::keggGet(as.character(kegggene)))
  } else {
    
    kegggene <- KEGGREST::keggConv("genes", paste("ncbi-geneid:", as.character(entrezgene) , sep="") )
    
    keggquery <- KEGGREST::keggGet(as.character(kegggene))
  }
  
  
  
  
  KEGG <- list("name" = "",
               "pathway" = "",
               "disease" = "",
               "motif" = "",
               "dblinks" = "",
               "structure" = "",
               "aa" = "")
  KEGG$name <- keggquery[[1]]$NAME
  
  # Pathways involved
  KEGG$pathway <- keggquery[[1]]$PATHWAY
  ## make link to KEGG pathway
  ## http://www.genome.jp/dbget-bin/www_bget?TERM
  ## TERM is names(KEGG.pathway) and name is KEGG.pathway
  ## needs iteration
  
  
  
  # Diseases
  KEGG$disease <- keggquery[[1]]$DISEASE
  ## make link to http://www.genome.jp/dbget-bin/www_bget?ds:TERM
  ## TERM is names(KEGG.disease) and name of diease is just KEGG.disease
  ## needs iteration
  
  # Motifs
  KEGG$motif <- keggquery[[1]]$MOTIF
  
  # DB LINKS / IDs
  # presence in other DBs
  KEGG$dblinks <- keggquery[[1]]$DBLINKS
  
  # Protein structures
  ## remove PDB and empty strings
  ## make URL to 
  # http://www.rcsb.org/pdb/explore/explore.do?structureId=TERM
  ## TERM is KEGG.structure
  ## NEEDS ITERATION
  KEGG$structure <- as.character(keggquery[[1]]$STRUCTURE)
  KEGG$structure <- unlist(strsplit(KEGG$structure ,split = " "))
  KEGG$structure <- KEGG$structure[!KEGG$structure %in% c("", "PDB:") ]
  
  
  
  # AA sequence
  KEGG$aa <- as.character(keggquery[[1]]$AASEQ)
  #print("Function getkegg")
  #print(KEGG)
  return(KEGG)
  
}














