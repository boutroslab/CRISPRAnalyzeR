# saved as enrichr_api.r

## used to retrive information from enrichr API
## http://amp.pharm.mssm.edu/Enrichr/help#api

#' Perform functional enrichment on a pair of "up"- and "down"- regulated genes sets,
#' such as those generated in a differential expression analysis.
#' 
#' This function wraps enrichGeneList to call Enrichr to perform functional enrichment tests on
#' the provided gene lists, for the databases specified in the databases argument. 
#' Databases are specified as seen in the web interface, with underscores for spaces
#' (e.g. "WikiPathways_2016", "KEGG_2016", "GO_Biological_Process"). There's no way to query Enrichr
#' to get these database names, so they can't be provided as options. You'll just have to guess. Sorry :/
#' 
#' @param up.genes a list of up-regulated gene symbols
#' @param dn.genes a list of down-regulated gene symbols
#' @param databases a list of Enrichr-fronted databases, as mentioned above
#' @param fdr.cutoff An FDR (adjusted p-value) threshold by which to limit the list of enriched pathways
#' @keywords functional enrichment Enrichr
#' @export
enrichFullGeneList <- function(up.genes, dn.genes, databases=db.list, fdr.cutoff=NULL, proxyurl = NULL, proxyport = NULL) {
  up.gene.res <- enrichGeneList(up.genes, databases, fdr.cutoff, proxyurl = proxyurl, proxyport = proxyport)
  up.gene.res$direction <- "UP"
  dn.gene.res <- enrichGeneList(dn.genes, databases, fdr.cutoff, proxyurl = proxyurl, proxyport = proxyport)
  dn.gene.res$direction <- "DN"
  return(rbind(up.gene.res, dn.gene.res))
}


#' Perform functional enrichment on a set of genes.
#' 
#' This function interacts with Enrichr's REST API in order to perform functional enrichment of a single
#' set of genes, for a set of specified databases which are already fronted by Enrichr.
#' Databases are specified as seen in the web interface, with underscores for spaces
#' (e.g. "WikiPathways_2016", "KEGG_2016", "GO_Biological_Process"). There's no way to query Enrichr
#' to get these database names, so they can't be provided as options. You'll just have to guess. Sorry :/
#' 
#' @param gene.list a list of gene symbols
#' @param databases a list of Enrichr-fronted databases, as mentioned above
#' @param fdr.cutoff An FDR (adjusted p-value) threshold by which to limit the list of enriched pathways
#' @keywords functional enrichment Enrichr
#' @export
enrichGeneList <- function(gene.list, databases=db.list, fdr.cutoff=NULL, proxyurl = NULL, proxyport = NULL) {
  ######Step 1: Post gene list to EnrichR
  shiny::incProgress(amount = 0.1, detail = "Query EnrichR")
  req.body <- list(list=paste(gene.list, collapse="\n"))
  if(!is.null(proxyurl) && !is.null(proxyport))
  {
    post.req <- httr::POST("http://amp.pharm.mssm.edu/Enrichr/enrich", encode="multipart", body=I(req.body), config = httr::use_proxy(url = proxyurl, port = as.numeric(proxyport)), httr::add_headers("Expect"=""))
  } else
  {
    post.req <- httr::POST("http://amp.pharm.mssm.edu/Enrichr/enrich", encode="multipart", body=I(req.body))
  }
  
  shiny::incProgress(amount = 0.2)
  #TODO: Real error handling..
  if (!grepl("success", httr::http_status(post.req)$category, ignore.case=T)) stop("Posting gene list to EnrichR failed")
  
  ######Step 2: Get results from posted gene list
  shiny::incProgress(amount = 0.2, detail = "Download Data")
  database.enrichments <- list()
  for (idx in 1:length(databases)) { 
    database <- databases[idx]
    
    if(!is.null(proxyurl) && !is.null(proxyport))
    {
      get.req <- httr::GET(paste("http://amp.pharm.mssm.edu/Enrichr/enrich?backgroundType=", database, sep=""), config = httr::use_proxy(url = proxyurl, port = as.numeric(proxyport)), httr::add_headers("Expect"=""))
    } else {
      get.req <- httr::GET(paste("http://amp.pharm.mssm.edu/Enrichr/enrich?backgroundType=", database, sep=""))
    }
    
    
    if (!grepl("success", httr::http_status(get.req)$category, ignore.case=T)) stop("Retrieving results from EnrichR failed")
    
    response.content <- mungeResponseContent(httr::content(get.req)[[database]])
    
    if (length(response.content) > 1) {
      database.res <- data.table::rbindlist(response.content)
      database.res[, 1] <- rep(database, nrow(database.res))
      database.enrichments[[idx]] <- database.res[, paste("V", c(1, 2, 3, 7, 4, 5, 6), sep=""), with=F]
    }
  }
  
  shiny::incProgress(amount = 0.2, detail = "Analyzing Data")
  
  query.results <- as.data.frame(data.table::rbindlist(database.enrichments))
  colnames(query.results) <- c("database", "category", "pval", "qval", "z-score", "score", "genes")
  
  if (!is.null(fdr.cutoff)) {
    query.results <- query.results[query.results$qval < fdr.cutoff, ]
  }
  
  return(query.results)
}



#' Munge the Enrichr API response so it'll squeeze neatly (if untidily) into a dataframe.
#' 
#' The response from the Enrichr API is a list of lists, where each nested list item represents an enriched
#' category. The 6th item of each category (i.e. response.content[[category.idx]][[6]]) corresponds to the 
#' genes that overlapped with the gene set behind that category. This function bascically collapses that list of 
#' genes into a single string. 
#' 
#' I'm sorry you ever had to look at this.
#' 
#' @param response.content result of calling httr::content on the GET request to the Enrichr API, after submitting a list for enrichment.
#' 
mungeResponseContent <- function(response.content) {
  munged.content <- response.content
  if (length(response.content) == 0) return(NA)
  
  for (idx in 1:length(response.content)) {
    munged.content[[idx]][[6]] <- paste(munged.content[[idx]][[6]], collapse=",")
  }
  
  return(munged.content)
}
