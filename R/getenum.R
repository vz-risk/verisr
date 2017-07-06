counto <- function(olist) {
  cnm <- names(olist)
  found.enum <- unlist(lapply(c('action', 'actor', 'attribute'), function(x) {
    paste(x, unique(getnth(cnm[grep(paste0("^", x), cnm)])), sep=".")
  }))
  for(x in found.enum) {
    olist[[x]] <- TRUE
  }
  assetmap <- c("S "="Server", "N "="Network", "U "="User Dev", "M "="Media", 
                "P "="Person", "T "="Kiosk/Term", "Un"="Unknown")
  outs <- cnm[grep('^asset.assets.variety', cnm)]
  if (length(outs) > 0) {
    found.enum <- paste('asset.assets', assetmap[substr(getlast(outs), 1,2)], sep=".")
    for(x in found.enum) {
      olist[[x]] <- TRUE
    }
  }
  # TODO : need to add more fields for convenience things like:
  # victim.industry2
  # victim.industry3
  # victim.orgsize, (Small/Large)
  # victim.region
  # victim.subregion
  olist
}

#' Get the last element from a column name
#' 
#' Givn a vector with one or more column names (veris fields), this will
#' return the last string in the name, as it is seperated by [.].
#' 
#' @param nm the vector of column names
getlast <- function(nm) {
  sapply(nm, function(x) {
    temp <- unlist(strsplit(x, '[.]'))
    temp[length(temp)]
  })
}

#' Get the last element from a column name, include action
#' 
#' Givn a vector with one or more column names (veris fields), this will
#' return the last string in the name, as it is seperated by [.].  It will
#' also assume this is the action and attempt to map nice names to the name.
#' 
#' @param nm the vector of column names
getlastaction <- function(nm) {
  fixlabel <- c("malware"="[Mal]", "hacking"="[Hack]", "social"="[Soc]",
                "error"="[Err]", "physical"="[Phys]", "misuse"="[Mis]",
                "environmental"="[Env]", "unknown"="[Unk]")
  sapply(nm, function(x) {
    temp <- unlist(strsplit(x, '[.]'))
    paste(temp[length(temp)], fixlabel[temp[2]])
  })
}

#' Get the nth element from a column name
#' 
#' Givn a vector with one or more column names (veris fields), this will
#' return the nth string in the name, as it is seperated by [.].
#' 
#' @param nm the vector of column names
#' @param which the nth vlue to return
getnth <- function(nm, which=2) {
  sapply(nm, function(x) {
    temp <- unlist(strsplit(x, '[.]'))
    temp[2]
  })
}

#' Get a data.frame of counts from an enumeration
#'
#' When exploring VERIS data, you may want to get a simple count of the values within a value or enumeration.  
#' Given one or more enumerations, this will return the subsequent underlying logical values in an ordered data frame.
#' 
#' Note there are some special values that can be set as the enumeration, 
#' that may not be obvious. :
#' * actor, action, attribute: will all return the next level down.  For example, just querying for "action" will return "malware", "hacking", and so on.
#' * action.variety: will return the variety enumerations across all actions (e.g. top N actions) (not in getenumby() yet)
#' * asset.variety: will return the type of assets, "Server", "Network, "User Dev" and so on
#' * victim.industry2: will return a short label of industries based on 2 values of NAICS code.
#' * victim.industry3: will return a short label of industries based on 3 values of NAICS code.
#' * pattern: will return the pattern the incidents are in.
#'
#' Change in 1.1: the "add.n" and "add.freq" options are now TRUE by default.
#' 
#' @param veris a verisr object
#' @param enum the field to count
#' @param filter limit what records are searched (optional)
#' @param add.n include a total count of variables found (denominator)
#' @param add.freq include a percentage (x/n)
#' @examples
#' \dontrun{
#' hacking <- getenum(veris, "action.hacking.variety")
#' external <- getenum(veris, "actor.external.motive")
#' }
getenum.single <- function(veris, enum, filter=NULL, add.n=T, add.freq=T) {
  if (is.null(filter)) {
    filter <- rep(T, nrow(veris))
  } else if (length(filter) != nrow(veris)) {
    warning(paste0("filter is not same length (", length(filter),
                   ") as object (", nrow(veris), ")."))
    return(NULL)
  }
  
  # get names from the veris object
  cnames <- colnames(veris)
  # extract by the enumeration
  # if field name exists as is, return it, else search.
  if(any(grepl(paste0('^', enum, "$"), cnames))) {
    # yes it exists
    if(is.logical(veris[[enum]])) {
      warning(paste0("single logical field requested: ", enum, ", skipping..."))  
    } else { # not a logical field, assuming factor
      out.table <- table(veris[[enum]])
      outdf <- data.frame(enum=names(out.table), x=as.vector(out.table))
      if (add.n) outdf$n <- sum(!is.na(veris[[enum]]))
      if (add.freq) outdf$freq <- outdf$x/outdf$n
      if (is.ordered(veris[[enum]])) {
        outdf$enum <- factor(outdf$enum, levels=levels(veris[[enum]]), ordered=T)
      } else {
        outdf$enum <- factor(outdf$enum, levels=outdf$enum, ordered=T)
      }
    }
  } else {
    # only match where there are one level of enumerations 
    # after the requested enum
    if(enum=="action.variety") {
      gkey <- paste0("^action.*.variety")
      thisn <- cnames[grep(gkey, cnames)]
      ret <- setNames(colSums(veris[filter ,thisn, with=F]), getlastaction(thisn))
      # TODO add "Action.variety" into getenumby()
    } else {
      gkey <- paste0("^", enum, ".[^.]+$")
      thisn <- cnames[grep(gkey, cnames)]
      colmodes <- sapply(thisn, function(x) is.logical(veris[[x]]) | is.numeric(veris[[x]]))
      if(!all(colmodes)) {
        warning(paste0("non-numeric/non-logical values requested: ", enum, ". Perhaps the enum is incorrect?"))
        return(data.frame())
      }
      ret <- setNames(colSums(veris[filter ,thisn, with=F]), getlast(thisn))
    }
    ret <- ret[ret>0]
    outdf <- data.table(enum=names(ret), x=ret)
    #outdf <- data.frame(enum=names(ret), x=ret)
    n <- sum(rowSums(veris[filter ,thisn, with=F]) > 0)
    if (n==0) return(data.frame())
    if (add.n) outdf$n <- n
    if (add.freq) outdf$freq <- outdf$x/n
    outdf <- outdf[order(rank(x), enum)]
    if (enum %in% c('actor', 'action', 'asset.variety', 'attribute')) {
      a4names <- names(geta4names())
      n.order <- getlast(a4names[grep(paste0('^', enum), a4names)])
      outdf$enum <- factor(outdf$enum, levels=rev(n.order), ordered=T)
    } else {
      outdf$enum <- factor(outdf$enum, levels=outdf$enum, ordered=T)
    }
  }
  outdf
}

#' Extract counts from one or more enumerations
#'
#' When exploring VERIS data, you may want to get a simple count of the values within a value or enumeration.  
#' Given one or more enumerations, this will return the subsequent underlying logical values in an ordered data frame.  
#' The data frame should be formatted for use in \code{ggplot2} graphics.
#' 
#' As of version 1.1: the \code{enum} variable may be a vector of one or more enumerations.  
#' This enables any number of dimensions to be specified.  This makes the \code{primary} and \code{secondary}
#' obsolete but are still supported for the time being.
#' 
#' Note there are some special values that can be set as the enumeration, 
#' that may not be obvious. :
#' * actor, action, attribute: will all return the next level down.  For example, just querying for "action" will return "malware", "hacking", and so on.
#' * action.variety: will return the variety enumerations across all actions (e.g. top N actions) (not in getenumby() yet)
#' * asset.variety: will return the type of assets, "Server", "Network, "User Dev" and so on
#' * victim.industry2: will return a short label of industries based on 2 values of NAICS code.
#' * victim.industry3: will return a short label of industries based on 3 values of NAICS code.
#' * pattern: will return the pattern the incidents are in.
#'
#' Change in 1.1: the "add.n" and "add.freq" options are now TRUE by default.
#' #' @aliases getenumby
#' @param veris a verisr object
#' @param enum the main enumeration field 
#' @param primary the primary enumeration to filter on
#' @param secondary the (optional) secondary enumeration to filter on
#' @param filter limit what records are searched (optional)
#' @param add.n include a total count of variables found (denominator)
#' @param add.freq include a percentage (x/n)
#' @param fillzero fill in missing matches with zeros
#' @param exclusive logical value, If true, will count the unknown value only if it exclusive and it will not count the Unknown if it is selected with other attributes in the enumeration.
#' @export
#' @import data.table
#' @examples
#' \dontrun{
#' # old method:
#' a2 <- getenum(veris, "action", primary="asset.variety")
#' # new method:
#' a4 <- getenum(veris, c("action", "asset.variety", "actor", "attribute"))
#' }
getenum2 <- function(veris, enum, primary=NULL, secondary=NULL, filter=NULL, 
                     add.n=T, add.freq=T, fillzero=T, exclusive=F) {
  if (missing(filter)) {
    filter <- rep(T, nrow(veris))
  } else if (length(filter) != nrow(veris)) {
    warning(paste0("filter is not same length (", length(filter),
                   ") as object (", nrow(veris), ")."))
    return(NULL)
  }
  cnames <- colnames(veris)
  enum <- c(enum, primary, secondary)
  if (length(enum)>1 & exclusive) {
    warning("Cannot retrieve multiple enumerations and have exclusive set to TRUE, ignoring exclusive argument.")
    exclusive <- FALSE
  }
  if(any(enum %in% c("asset.assets"))) {
    # message("getenumby(): as of version 1.1, asset.assets should be replaced by asset.variety")
    enum[which(enum %in% c("asset.assets"))] <- "asset.variety"
  }
  fullkey <- paste0('^', enum, "$")
  fulln <- sapply(fullkey, function(x) any(grepl(x, cnames)))
  if (length(enum)==1 & all(fulln)) {
    if(is.logical(veris[[enum]])) {
      warning(paste0("single logical field requested: ", enum, ", skipping..."))
      return(data.frame())
    } else { # not a logical field, assuming factor
      out.table <- table(veris[[enum]])
      if(!length(out.table)) {
        return(data.frame())
      }
      outdf <- data.frame(enum=names(out.table), x=as.vector(out.table))
      if (add.n) outdf$n <- sum(!is.na(veris[[enum]]))
      if (add.freq) outdf$freq <- outdf$x/outdf$n
      if (is.ordered(veris[[enum]])) {
        outdf$enum <- factor(outdf$enum, levels=levels(veris[[enum]]), ordered=T)
      } else {
        outdf$enum <- factor(outdf$enum, levels=outdf$enum, ordered=T)
      }
    }
  } else {
    gkey <- paste0("^", enum, ".[^.]+$")
    savethisn <- thisn <- lapply(gkey, function(x) cnames[grep(x, cnames)])
    allfound <- sapply(thisn, function(x) length(x)>0)
    if(!all(allfound)) {
      warning(paste0("getenumby(): No columns matched \"", enum[!allfound], "\"", collapse="\", \""))
      return(data.frame())
    }
    
    thisn$x <- 0
    outdf <- as.data.table(expand.grid(thisn))
    #outdf <- as.data.frame(expand.grid(thisn))
    cnm <- colnames(outdf)[1:(ncol(outdf)-1)]
    # just look in first enum (exclusive) for unknowns
    myunks <- unique(unlist(sapply(c("Unknown", " - Other", "unknown"), function(p) grep(p, thisn[[1]])), use.names=F))
    for(i in seq(nrow(outdf))) {
      this.comp <- as.character(unlist(outdf[i, cnm, with = F]))
      count <- rowSums(veris[filter, this.comp, with=F]) == length(enum)
      if (exclusive && i %in% myunks) {
        count <- sum(count & rowSums(veris[filter, thisn[[1]], with=F])==1)
      } else {
        count <- sum(count)
      }
      outdf[i, x:=count]
    }
    for(column in cnm) {
      tempcol <- getlast(as.character(unlist(outdf[ , column, with=F])))
      outdf[ , column:=tempcol, with=F]
    }
    extra.names <- NULL
    if (length(enum)>1) extra.names <- paste0('enum', seq((length(enum)-1)))
    setnames(outdf, c('enum', extra.names, 'x'))
    n <- sum(rowSums(veris[filter ,unlist(savethisn), with=F], na.rm=T) > 0, na.rm=T)
    if (n==0) return(data.frame())
    if (!fillzero) {
      outdf <- outdf[outdf$x>0,]
    }
    if (add.n) outdf$n <- n
    if (add.freq) outdf$freq <- outdf$x/n
    # how about we put some order to the chaos
    a4names <- names(geta4names())
    for(i in seq_along(enum)) {
      if (enum[i] %in% c('actor', 'action', 'asset.variety', 'attribute')) {
        n.order <- getlast(a4names[grep(paste0('^', enum[i]), a4names)])
        this.col <- colnames(outdf)[i]
        outdf[[this.col]] <- factor(outdf[[this.col]], levels=rev(n.order), ordered=T)
      }    
    }
  }
  # name the columns... enum enum1 enum2 (?)
  # print(outdf)
  #outdf <- outdf[order(-rank(x), enum)]
  #outdf$enum <- factor(outdf$enum, levels=outdf$enum, ordered=T)
  outdf
}

#' @export
getenumby <- function(...) {
  getenum(...)
}

#' @export
getenum <- function(...) {
  getenumCI(...)
}