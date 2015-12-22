#' get matrix for CSV
#' 
#' given a verisr object, this will create a matrix of the data 
#' suitable for saving off as a CSV.  
#' 
#' @param veris a verisr object
#' @export
veris2csv <- function(veris) {
  vnames <- getvnames(veris)
  # setup venum with field names to represent as categories
  # the names must be passable to getenumlist
  enumfields <- c("^actor", "^action", "availability.variety", "integrity.variety")
  venum <- vnames[grep(paste(enumfields, collapse='|'), vnames)]
  venum <- venum[grep("cve|name|notes|country|industry", venum, invert=T)]
  venum <- c(venum, "actor", "action")
  # vtext are fields to just dump as text
  vtext <- vnames[grep(paste(venum, collapse='|'), vnames, invert=T)]
  # ignore the one offs for now (do these below)
  oneoff <- c("attribute.confidentiality.data\\.",
              "asset.assets",
              "impact.loss",
              "schema_version")
  # pull them out of vtext
  vtext <- vtext[grep(paste(oneoff, collapse='|'), vtext, invert=T)]
  # pull the text into a results for text field
  rtext <- sapply(vtext, function(x) {
    ## victim will be messed up
    rval <- sapply(getenumlist(veris, x), function(vrow) { unlist(vrow)[1] })
    rval[is.na(rval)] <- ""
    rval
  })
  # now do the categorical values
  renum <- do.call(cbind, sapply(venum, function(x) {
    # create a matrix for this one venum name, one column for each value pulled
    raw <- getenumlist(veris, x)
    rawname <- sort(unique(unlist(raw)))
    outmat <- matrix(data=c(0), nrow=length(veris), ncol=length(rawname))
    colnames(outmat) <- paste(x, rawname, sep='.')
    for(i in seq(raw)) {
      outmat[i, which(rawname %in% raw[[i]])] <- 1
    }
    outmat
  }))
  ## unique asset categories
  my.assets <- c("asset.assets", "asset.assets.variety")
  assets <- do.call(cbind, lapply(my.assets, function(x) {
    raw <- getenumlist(veris, x)
    rawname <- sort(unique(unlist(raw)))
    asset <- matrix(data=c(0), nrow=length(veris), ncol=length(rawname))
    colnames(asset) <- paste(x, rawname, sep='.')
    for(i in seq(raw)) {
      thisval <- table(unlist(raw[[i]]))
      asset[i, which(rawname %in% names(thisval))] <- as.vector(thisval)
    }
    asset
  }))  
  #sortvmatrix(cbind(rtext, renum, assets))
  sortvmatrix(cbind(data.frame(rtext), data.frame(renum), data.frame(assets)))
}

#' Internal: sort veris columns  
#' 
#' This will sort the columns of a matrix so Wade is happy.
#' 
#' @param x a matrix where columns are veris fields
sortvmatrix <- function(x) {
  sortorder <- c("incident_id", "source_id", "reference", "security_incident",
                 "confidence", "summary", "related_incident",
                 "notes", "victim", "actor.ext", "actor.int", "actor.par",
                 "actor.unk",
                 "action.mal", "action.hack", "action.soc", "action.mis",
                 "action.phy", "action.err", "action.env", "action.unk", 
                 "asset",
                 "attribute.conf", "attribute.int", "attribute.avail",
                 "timeline", "discovery", "target", "control", "correct",
                 "cost", "ioc", "impact", "plus")
  unsorted <- colnames(x)
  sortnames <- NULL
  for(i in seq_along(sortorder)) {
    sortnames <- c(sortnames, unsorted[grep(paste("^", sortorder[i], sep=''), unsorted, perl=T)])
  }
  missed <- grep(paste("^", sortnames, sep="", collapse="|"), unsorted, perl=T, invert=T)
  if(length(missed)) {
    sortnames <- c(sortnames, unsorted[missed])    
  }
  x[ , sortnames]
}

#' convert veris object to a matrix
#' 
#' given a verisr object, this will create a matrix of the data 
#' suitable for further PCA analysis 
#' 
#' @param veris a verisr object
#' @param industry either 2 or 3 digits for industry
#' @param unknown logical, whether to include Unknown/Other fields
#' @export
veris2matrix <- function(veris, industry=2, unknown=F) {
  vnames <- getvnames(veris)
  enumfields <- c("actor", "action")
  venum <- vnames[grep(paste('^',enumfields, sep='', collapse='|'), vnames)]
  venum <- venum[grep("cve|name|notes|country|industry", venum, invert=T)]
  venum <- c(venum, vnames[grep('timeline.*unit', vnames)] )
  if (industry==2) {
    venum <- c(venum, "victim.industry2")    
  } else {
    venum <- c(venum, "victim.industry3")    
  }
  venum <- c(venum, "actor", "action", 
             "victim.employee_count", 
             "security_incident", "asset.assets", "asset.assets.variety", "asset.cloud", 
             "asset.hosting", "asset.management", "asset.ownership",
             "attribute.confidentiality.data.variety", "attribute.confidentiality.data_disclosure",
             "discovery_method", "targeted",
             "attribute.integrity.variety", "attribute.availability.variety")
  # skipping "discovery_method", "targeted"
  # fixed that, not skipping discovery_method or targeted
  # now do the categorical values
  renum <- do.call(cbind, sapply(venum, function(x) {
    # create a matrix for this one venum name, one column for each value pulled
    raw <- getenumlist(veris, x)
    rawname <- sort(unique(unlist(raw)))
    if (length(rawname)==0) {
      return(NULL)
    }
    outmat <- matrix(data=c(0), nrow=length(veris), ncol=length(rawname))
    colnames(outmat) <- paste(x, rawname, sep='.')
    for(i in seq(raw)) {
      outmat[i, which(rawname %in% raw[[i]])] <- 1
    }
    outmat
  }))
  final.matrix <- sortvmatrix(renum)
  # strip out unknowns, it's the right thing to do.
  # careful here, also stripping out "NA" which may not be appropriate
  if(!unknown) {
    final.matrix <- final.matrix[ ,grep("Unknown|unknown|00|NA", colnames(final.matrix), invert=T)]
  }
  final.matrix[ ,colSums(final.matrix)!=nrow(final.matrix)]
}

#' compact rows for analysis
#' 
#' given a verisr object and a list of features, this will create a matrix of the data 
#' suitable for further PCA, hclust or other dist() analysis.  
#' 
#' @param pca a pca matrix object
#' @param feature a vector of features to fold on
#' @param min if min is > 1, this will check for a minimum amount of records for each feature, 
#' if it doesn't have the min, it will be set to zeros and cleaned if clean is set to TRUE
#' @param clean if TRUE, this will look for anything that will mess up a PCA or MDS function and remove it.
#' @export
foldmatrix <- function(pca, feature, min=1, clean=FALSE) {
  folded <- do.call(rbind, lapply(feature, function(x) {
    # pull slice of rows where that incident has feature x
    incidents <- which(pca[ ,x]==1)
    if (length(incidents) < max(2, min)) {
      ret <- pca[1, ]
      ret[ret>0] <- 0
    } else {
      pcas <- pca[incidents, ]
      ret <- colMeans(pcas)
    }
    ret
  }))
  rownames(folded) <- feature
  if (clean) {
    # removing zero-sum rows (addressing "min" above)
    allsums <- rowSums(folded)
    # and columns need non-zero variance
    allvars <- apply(folded, 2, var)
    folded <- folded[which(allsums>0), which(allvars>0)]
  }
  folded
}

#' fold rows of matrix for further analysis
#' 
#' given a verisr object and a list of features, this will create a matrix of the data 
#' suitable for further PCA analysis.  
#' 
#' @param veris a verisr object
#' @param filter an optional filter to apply
#' @export
pcamatrix <- function(veris, filter=NULL) {
  if (!is.null(filter)) {
    if (is.logical(filter)) {
      veris <- veris[filter]
    } else {
      stop("filter is not logical vector")
    }
  }
  # get the raw PCA matrix
  rawmatrix <- veris2matrix(veris)
  allnames <- colSums(rawmatrix)
  cat("Folding", nrow(rawmatrix), "rows by", ncol(rawmatrix), "cols.\n")
  # have at least 1% or 5, whichever is greater
  venum <- names(allnames[which(allnames>(max(nrow(rawmatrix)*0.01, 8)))])

  feature <- venum[grep("^actor|^action|^asset.assets.variety|data.variety", venum)]
  
  pca.ready <- foldmatrix(rawmatrix, feature)
  # want foldvmatrix to take in a list of features or a list of filters
  # for now will take on just the feature list.
  
  pca.ready
}

#' count incidents in clusters
#' 
#' given a verisr object and a list of labels, this will create a data.frame of the data 
#' suitable for plotting.  
#' 
#' @param veris a verisr object
#' @param label a list of labels to count
#' @param filter an optional filter to apply
#' @export
countMatrix <- function(veris, label, filter=NULL) {
  if (!is.null(filter)) {
    if (is.logical(filter)) {
      veris <- veris[filter]
    } else {
      stop("filter is not logical vector")
    }
  }
  rawmatrix <- veris2matrix(veris)
  
  folded <- do.call(rbind, lapply(seq_along(label), function(cluster) {
    do.call(rbind, lapply(label[[cluster]], function(x) {
      data.frame(cluster=cluster, enum=x, count=sum(rawmatrix[ ,x]==1))
    }))
  }))
  folded
}
