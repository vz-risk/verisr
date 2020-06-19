#' Fold a verisr object into a numeric matrix of proportions
#' 
#' given a verisr object and a list of features (columns) and objects (rows), this will 
#' create a numeric matrix of the data suitable for further veris, hclust or other dist() analysis. 
#' 
#' Each cell within the matrix will be a proprotion of incidents that either have the object 
#' or match the pattern passed in a names list, depending if the rows argument is a vector of column
#' names or a names list of logical filters respectively. 
#' 
#' @param veris a veris matrix object
#' @param cols a vector of features to use in the columns, if left empty, it will use all the 
#' logical columns in the input matrix.
#' @param rows a vector of column names or a named list of filters to use as the rows in the folded matrix
#' @param min the minimum number of matches a row must have to be considered, defaults to 1
#' @param clean if TRUE, this will look for anything that will mess up a veris or MDS function 
#' and remove it, this includes empty rows and columns as well as columns with zero variance.
#' @export
foldmatrix <- function(veris, cols=NULL, rows=NULL, min=1, clean=T) {
  all.logical <- getlogical(veris)
  if(is.null(cols)) {
    cols <- all.logical
  }
  if(is.null(rows)) {
    rows <- cols
  }
  if (!is.list(rows)) {
    # this tests if each of the values in rows
    # exists and is a logical value
    all.there <- sapply(rows, function(x) x %in% all.logical)
    if (!all(all.there)) {
      warning(paste("rows are not logical:", paste0(names(all.there)[!all.there], collapse=", ")))
    }
    rows <- veris[ , names(all.there)[all.there], with=F]
  }
  if (is.null(names(rows))) {
    message("List for the rows is unnamed, using defaults")
    names(rows) <- paste0("V", seq_along(rows))
  }
  outmat <- matrix(0, nrow=length(rows), ncol=length(cols), dimnames=list(names(rows), cols))
  for(i in seq_along(rows)) {
    if (sum(rows[[i]])>=0) {
      subveris <- veris[rows[[i]], cols, with=F]
      if (nrow(subveris)>=min) {
        thisrow <- colSums(subveris)
        # cat("looking at", names(rows)[i], "[", sum(rows[[i]]), "] - sum of", sum(thisrow),"\n")
        outmat[i, ] <- thisrow/sum(rows[[i]])
      }
    }
  }
  allcols <- colSums(outmat) > 0
  if (clean) {
    # removing zero-sum rows
    allsums <- rowSums(outmat)
    # and columns need non-zero variance
    allvars <- apply(outmat, 2, var)
    folded <- outmat[allsums>0, allvars>0 & allcols]
  } else {
    folded <- outmat[ ,allcols]
  }
  folded
}  

#' Determine the patterns from the 2014 Verizon DBIR
#' 
#' given a verisr object, this will determine which pattern the incident is in (or "Other" if no pattern is matched). 
#' Note the returned vector will be a factor with ordered levels for arranging the patterns in an order.
#' 
#' @param veris a verisr object
#' @export
#' @examples
#' data(veris.sample)
#' 
#' # produces a vector with 1-to-1 mapping to verisr object
#' pat <- getpattern(veris.sample)
#' 
#' # can summarize the results
#' table(pat)
getpattern <- function(veris) {
  skimmer <- veris[['action.physical.variety.Skimmer']] |
    (veris[['action.physical.variety.Tampering']] & veris[['attribute.confidentiality.data.variety.Payment']])
  espionage <- (veris[['actor.external.motive.Espionage']] | 
                  veris[['actor.external.variety.State-affiliated']])
  
  pos <- (veris[['asset.assets.variety.S - POS controller']] |
            veris[['asset.assets.variety.U - POS terminal']])
  dos <- veris[['action.hacking.variety.DoS']]
  webapp <- veris[['action.hacking.vector.Web application']]
  webapp <- webapp & !(webapp & dos)
  misuse <- veris[['action.Misuse']]
  
  vfilter <- skimmer | espionage | pos | dos | webapp | misuse  
  mal.tmp <- veris[['action.Malware']] & 
    !veris[['action.malware.vector.Direct install']]
  malware <- (mal.tmp & !vfilter)
  theftloss <- veris[['action.error.variety.Loss']] | 
    veris[['action.physical.variety.Theft']]
  vfilter <- vfilter | malware | theftloss
  errors <- veris[['action.Error']] & !vfilter
  vfilter <- vfilter | errors
  other <- !vfilter
  pats <- data.frame(pos, webapp, misuse, theftloss, errors, malware,
                     skimmer, dos, espionage, other)
  
  patcols  <- c("Point of Sale",
                "Web Applications",
                "Privilege Misuse",
                "Lost and Stolen Assets",
                "Miscellaneous Errors",
                "Crimeware",
                "Payment Card Skimmers",
                "Denial of Service",
                "Cyber-Espionage",
                "Everything Else")
  colnames(pats) <- patcols  
  # convert T/F to colname if True
  named.df <- do.call(cbind, lapply(colnames(pats), function(x) {
    ifelse(pats[ ,x], x, NA)
  }))
  # now reduce each row to a single label, return the vector
  retval <- apply(named.df, 1, function(x) {
    x[!is.na(x)][1]
  })
  colnames(pats) <- paste0("pattern.", patcols)
  #retval <- factor(retval, levels=patcols, ordered=T)  # this is the one, bloody, factor in the whole dataframe.  it's pointless.  Strongly considering removing - GDB 200619
  cbind(data.table(pattern=retval), pats)
}
