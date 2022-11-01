#' Read in all the JSON files in directory
#'
#' This function will iterate through all the JSON files (regex pattern of "json$") in
#' the given directory and parse it as a VERIS object.
#' This will return a verisr object.
#'
#' @param dir the directory to list through
#' @keywords json
#' @import rjson
#' @examples
#' \dontrun{
#' veris <- json2veris(dir="~/vcdb")
#' }
json2veris.old <- function(dir=".") {
  # create listing of files
  jfiles <- unlist(sapply(dir, list.files, pattern = "json$", full.names=T))
  # now read them all
  veris <- lapply(jfiles, function(jfile) {
    fromJSON(file=jfile, method='C')
  })
  # set my class
  class(veris) <- "verisr"
  veris
}

#' testing some theories
#' 
#' @export
#' 
readjson.old <- function() {
  inf <- fromJSON(file="~/Documents/json/newfinal/2014/final/vzir/2013vzir0292.json")
  
}


#' return columns for a filter
#' 
#' @param veris a verisr object
#' @param field the base field to fold on
#' @export
vcol.old <- function(veris, field) {
  vnames <- colnames(veris)
  grep(field, vnames)
}

# # Count the existance of a field in all records
# #
# # This function will count where the field is not null
# #
# # @param veris a verisr object
# # @param field the field to count
# # @export
# # @examples
# # \dontrun{
# # hacking <- count(veris, "action.hacking")
# # external <- count(veris, "actor.external")
# # }
# count <- function(veris, field) {
#   sapply(field, function(f) {
#     vars <- unlist(strsplit(f, ".", fixed=T))
#     sum(unlist(lapply(veris, function(x) ifelse(is.null(x[[vars]]), 0, 1))))
#   })
# }

#' Get a vector of values from an enumeration
#'
#' This will collect the values for an enumation 
#'
#' @param veris a verisr object
#' @param enum the field to count
#' @param primary a primary field to split the enum by
#' @param secondary a secondary field to split the enum by
#' @param filter limit what records are searched (optional)
#' @param add.n include a total count of variables found (denominator)
#' @param add.freq include a percentage (x/n)
#' @param fillzero add in zeros for missing combinations (if primary or secondary specified)
#' @export
#' @examples
#' \dontrun{
#' hacking <- getenum(veris, "action.hacking.variety")
#' external <- getenum(veris, "actor.external.motive")
#' }
getenum.old <- function(veris, enum, primary=NULL, secondary=NULL, 
                    filter=NULL, add.n=F, add.freq=F, fillzero=T) {
  lifecycle::deprecate_soft("2.3.1.006", "getenum.old()", "getenumCI()")
  
  if(!missing(primary)) {
    return(getenumby(veris=veris, enum=enum, primary=primary, 
                     secondary=secondary, filter=filter, add.n=add.n,
                     add.freq=add.freq, fillzero=fillzero))
  }
  # get the internal list for the enumeration
  int.enum <- getenumlist(veris, enum)
  # and apply the filter, it one was passed in
  if (!is.null(filter)) {
    int.enum <- ifelse(filter, int.enum, NA)
  }
  # count and aggreagte it
  count.enum <- table(unlist(int.enum))
  if (any(dim(count.enum) == 0)) {
    warning(paste("No values found for enum \"", enum, "\"", sep=""))
    return(data.frame())
  }
  # convert to data.frame
  enum.df <- data.frame(enum=names(count.enum), x=as.vector(count.enum))
  # order it
  enum.df <- enum.df[with(enum.df, order(-x)), ]
  # reset the row names
  row.names(enum.df) <- 1:nrow(enum.df)
  # make the enum a factor
  enum.df$enum <- factor(enum.df$enum, levels=rev(enum.df$enum), ordered=T)
  if (add.n | add.freq) {
    # get the count of non-null values
    n <- sum(sapply(int.enum, function(x) {
      # it has length and is.na, return 0, else 1
      ifelse(length(x)==1, ifelse(is.na(x), 0, 1) ,1) })
    )
  }
  if (add.n) {
    enum.df$n <- n
  }
  if (add.freq) {
    enum.df$freq <- round(enum.df$x/n, 3)
  }
  enum.df
}

#' This will return a verisr filter object.
#' 
#' Get a filter given a list of "and" combinations and/or a list 
#' of "or" combinations.
#' Note: industry can be industryN
#' values can be "$exists" for that field being present
#' 
#' @param veris a verisr object
#' @param or list of criteria matching "or"
#' @param and list of criteria matching "and"
#' @param or.not list of criteria matching "or not"
#' @param and.not list of criteria matching "and not"
#' @export
#' @examples
#' \dontrun{
#' #tbd
#' hacking <- getenum(veris, "action.hacking.variety")
#' external <- getenum(veris, "actor.external.motive")
#' }
getfilter.old1 <- function(veris, and=NULL, or=NULL, or.not=NULL, and.not=NULL) {
  lifecycle::deprecate_soft("2.3.1.006", "getfilter.old1()")
  
  # this will return a matrix, one row for each
  # element in the list passed in, with a match
  # of the value in that list
  simple.match <- function(either) {
    sapply(names(either), function(x) {
      unlist(sapply(getenumlist(veris, x), function(y) {
        if (either[[x]]=="$exists") {
          match.list <- as.logical(sum(!is.na(y)))
        } else {
          # note this means any vector is treated as an "or"
          match.list <- as.logical(sum(ifelse(either[[x]] %in% y, TRUE, FALSE)))
        }
        match.list
      }))
    })
  }
  retval <- NULL
  if (!is.null(or)) {
    or.retval <- as.logical(apply(simple.match(or), 1, sum))
  } else {
    or.retval <- rep(T, length(veris))
  }
  if (!is.null(and)) {
    #if all match, include the record
    and.retval <- ifelse(apply(simple.match(and), 1, sum)==length(and), TRUE, FALSE)
  } else {
    and.retval <- rep(T, length(veris))
  }
  if (!is.null(and.not)) {
    # if all match, exclude the record
    and.not.retval <- ifelse(apply(simple.match(and.not), 1, sum)==length(and.not), FALSE, TRUE)
  } else {
    and.not.retval <- rep(T, length(veris))    
  }
  if (!is.null(or.not)) {
    #if any are true, set to false (!) 
    or.not.retval <- !as.logical(apply(simple.match(or.not), 1, sum))
  } else {
    or.not.retval <- rep(T, length(veris))
  }
  # defaulting to an AND match between them
  sendlist <- (or.retval & and.retval & or.not.retval & and.not.retval)
  class(sendlist) <- "verisr.filter"
  sendlist
}  

#' This will return a verisr filter object.
#' 
#' Get a filter given a list of "and" combinations and/or a list 
#' of "or" combinations.
#' Note: industry can be industryN
#' values can be "$exists" for that field being present
#' 
#' @param veris a verisr object
#' @param or list of criteria matching "or"
#' @param and list of criteria matching "and"
#' @param or.not list of criteria matching "or not"
#' @param and.not list of criteria matching "and not"
#' @export
#' @examples
#' \dontrun{
#' #tbd
#' hacking <- getenum(veris, "action.hacking.variety")
#' external <- getenum(veris, "actor.external.motive")
#' }
vfilter.old <- function(veris, and=NULL, or=NULL) {
  lifecycle::deprecate_soft("2.3.1.006", "vfilter.old()")
  
  #veris %>% getfilter("action.hacking.variety"="SQLi")
  #and=c("action.hacking.variety.SQLi", "asset.assets.variety.S - Mail")
  # what if I allow | and &
  # "action.hacking.variety"= "SQLi" & "victim.industry" = "^22"
  sendlist <- c("this is someting")
  class(sendlist) <- "verisr.filter"
  sendlist
}  


#' Get a count of enumerations values by some other enumeration
#'
#' This will collect an enumation and count it.
#'
#' @param veris a verisr object
#' @param enum the main enumeration field 
#' @param primary the primary enumeration to filter on
#' @param secondary the (optional) secondary enumeration to filter on
#' @param filter limit what records are searched (optional)
#' @param add.n include a total count of variables found (denominator)
#' @param add.freq include a percentage (x/n)
#' @param fillzero fill in missing matches with zeros
#' @export
#' @examples
#' \dontrun{
#' hacking <- getenum(veris, "action.hacking.variety")
#' external <- getenum(veris, "actor.external.motive")
#' }
getenumby.old <- function(veris, enum, primary, secondary=NULL, filter=NULL, add.n=F, add.freq=F, fillzero=T) {
  lifecycle::deprecate_soft("2.3.1.006", "getenumby.old()")
  
  if(length(by) > 2) {
    warning("by variable has more than 2, only taking first 2")
  }
  # get the internal main enum
  main <- getenumlist(veris, enum)
  # make sure we have something in this list
  if (!any(!is.na(unlist(main)))) {
    stop(paste("Primary enumeration \"", enum, "\": no incidents matching", sep=""))
  }
  # get the primary enum
  penum <- getenumlist(veris, primary)
  if (!any(!is.na(unlist(penum)))) {
    stop(paste("Primary enumeration \"", primary, "\": no incidents matching", sep=""))
  }
  # see if there is a secondary and prepare it (or null list if not)
  if (!is.null(secondary)) {
    senum <- getenumlist(veris, secondary)    
    if (!any(!is.na(unlist(senum)))) {
      stop(paste("Secondary enumeration \"", secondary, "\": no incidents matching", sep=""))
    }
  } else {
    senum <- rep(list(NULL), length(main))
  }
  ## apply the filter if one was passed in
  if (!is.null(filter)) {
    main <- ifelse(filter, main, NA)
    penum <- ifelse(filter, penum, NA)
    senum <- ifelse(filter, senum, NA)
    if (!any(!is.na(unlist(main)))) {
      stop(paste("Primary enumeration \"", enum, "\": no incidents matching after filter applied", sep=""))
    }
    if (!any(!is.na(unlist(penum)))) {
      stop(paste("Primary enumeration \"", primary, "\": no incidents matching after filter applied", sep=""))
    }
    if (!is.null(secondary)) {
      if (!any(!is.na(unlist(senum)))) {
        stop(paste("Secondary enumeration \"", secondary, "\": no incidents matching", sep=""))
      }
    }
  }
  
  full.df <- do.call(rbind, lapply(seq(length(main)), function(index) {
    # skip if we have any NA values across the board.
    if (!any(is.na(c(main[[index]], 
                     penum[[index]],
                     senum[[index]])))) {
      if (is.null(secondary)) {
        expand.grid(enum=unlist(main[[index]]), 
                    primary=unlist(penum[[index]]))
      } else {
        expand.grid(enum=unlist(main[[index]]), 
                    primary=unlist(penum[[index]]),
                    secondary=unlist(senum[[index]]))
      }
    }      
  }))
  full.df$x <- 1
  retval <- aggregate(x ~ ., data=full.df, FUN=sum)
  # now that we have a data frame
  # fill with zero's?
  if(fillzero) {
    if (is.null(secondary)) {
      zerofill <- expand.grid(enum=unique(retval$enum), 
                              primary=unique(retval$primary))
    } else {
      zerofill <- expand.grid(enum=unique(retval$enum), 
                              primary=unique(retval$primary),
                              secondary=unique(retval$secondary))
    }
    retval <- merge(retval, zerofill, all=T)
    retval$x[is.na(retval$x)] <- 0
  }
  sort.df <- aggregate(x ~ enum, data=retval, FUN=sum)
  sort.enum <- as.character(sort.df$enum[with(sort.df, order(x))])
  retval$enum <- factor(retval$enum, levels=sort.enum, ordered=T)
  if (add.n | add.freq) {
    pre.n <- sapply(seq(length(main)), function(index) {
      ifelse(!any(is.na(c(main[[index]],penum[[index]], senum[[index]]))), 1, 0)
    })
    n <- sum(pre.n)
  }
  if (add.n) {
    retval$n <- n
  }
  if (add.freq) {
    retval$freq <- round(retval$x/n, 3)
  }
  
  retval
}

# save_off <- fucntion(veris, enum, by)
#   # get the internal list for the enumeration
#   primary.enum <- getenumlist(veris, enum)
#   #secondary.df <- getenum(veris, by)
#   if(by=="industry2") {
#     secondary.enum <- getindustry(veris, 2)
#   } else {
#     secondary.enum <- getenumlist(veris, by)
#   }
#   by.list <- unique(unlist(secondary.enum))
#   by.list <- by.list[!is.na(by.list)]
#   rez <- do.call(rbind, lapply(by.list, function(x) {
#     local.filter <- getsimfilter(secondary.enum, x)
#     pri.count <- table(unlist(primary.enum[local.filter]))
#     if (length(pri.count)) {
#       int.df <- data.frame(enum=names(pri.count), 
#                            x=as.vector(pri.count), 
#                            primary=x)      
#     } else {
#       int.df <- NULL
#     }
#     int.df
#   }) )
#   if(by=="industry2") {
#     rez$primary <- merge(rez, industry2, by.x="primary", by.y="code")$title
#   }
#   rez
# }  
#foo <-getenumby(vcdb, "action.hacking.variety", "actor.external.variety")
#foo <-getenumby(vcdb, "action.hacking.variety", "industry2")
#ggplot(foo, aes(enum, x)) + geom_bar(stat="identity") + facet_wrap( ~ primary, ncol=2) + coord_flip() + theme_bw()
# 
# getindustry <- function(veris, len=2) {
#   int.enum <- getenumlist(veris, "victim.industry")
#   sapply(int.enum, function(x) {
#     sapply(x, function(y) {
#       ret.val <- NA
#       if (nchar(y) > len) {
#         ret.val <- substr(y, 1, len)
#         if (ret.val==rep("0", len)) {
#           ret.val <- NA
#         }
#       } 
#       ret.val      
#     })
#   })  
# }

#' return a list matching vcdb ordering and length with requested object
#' 
#' This will iterate through the veris object and return
#' a list of matches.  This is intented to maintain the orginal
#' indexes of the veris object so further manipulation can be done.
#' 
#' Note: Can do a special "industryN" request and it will chop
#' off the industry at the N value or return same length of zeros
#' if it isn't long enough.
#' 
#' @param veris a verisr object
#' @param enum the field to count
#' @export
getenumlist.old <- function(veris, enum) {
  lifecycle::deprecate_soft("2.3.1.006", "getenumlist.old()")
  
  # if the veris object has null names and yet length
  # it is an array, and we simply want to step into
  # and through it.  The top level veris object
  # is an array, as is things like victim and assets
  # and data variety
  if(is.null(names(veris)) & length(veris)) {
    return(lapply(veris, getenumlist, enum))
  }
  # now we are in the meat of the function
  # and we should have either a full slice
  # or a partial slice of a veris incident
  
  # look at the enum passed in, want to 
  # grab the first ("tag") and concatenate the rest
  vars <- unlist(strsplit(enum, "[.]"))
  tag <- vars[1]
  therest <- paste(vars[-1], collapse='.')
  # if the veris object is null at "tag", return NA
  if (is.null(veris[[tag]])) {
    retval <- NA
  } else if (therest == "") {
    # else if we are at the end of our enum, return the value?
    if (length(veris[[tag]])==0) {
      retval <- NA
    }
    # if we have names return those
    # it's an easy way to count actions, actors, etc.
    these.names <- names(veris[[tag]])

    #cat("the rest is blank, names:", these.names, "null:", is.null(these.names), "\n")
    if (tag=="assets") {
      assetmap <- c("S"="Server", "N"="Network", "U"="User Dev", "M"="Media", 
                    "P"="Person", "T"="Kiosk/Term", "Unknown"="Unknown")
      retval <- unique(unlist(sapply(veris[[tag]], function(asset) {
        myasset <- ifelse(asset$variety=="Unknown", "Unknown", substr(asset$variety, 1, 1))
        myamount <- 1 # not counting more than one here
        # myamount <- ifelse(is.null(asset$amount), 1, asset$amount)
        rep(assetmap[[myasset]], myamount)
        
      })))
    } else if (!is.null(these.names)) {
      retval <- these.names
      # note to self, this is causing the getMatrix functions 
      # to return NA, as the names of the return vector are blank
#    } else if (is.null(these.names)) {
#      retval <- NA
    } else {
      retval <- veris[[tag]]
    }
  } else {
    # else we need to continue to "drill down" into the veris object
    # with the rest of the enum being quieried
    # passing it back to self so it can parse through arrays
    # and use the same logic to continue parsing
    #
    # but before we do, let's check for some unique variables
    # like "industry*" where * is a length to chop
    if (grepl("^industry\\d$", therest, perl=T)) {
      # figure out the length of industry to return
      ind.len <- substr(therest, 9, 9)
      retval <- getenumlist(veris[[tag]], "industry")
      retval <- lapply(retval, function(x) {
        i <- substr(x, 1, ind.len)
        ifelse(nchar(i)==ind.len, i, paste(rep("0", ind.len), collapse=""))
      })
    } else if (tag=="assets" & therest=="variety") {
      retval <- unlist(sapply(veris[[tag]], getVarietyAmount))
    } else if (tag=="data" & therest=="variety") {
      retval <- unlist(getenumlist(veris[[tag]], therest))
    } else if (tag=="loss" & therest=="variety") {
      # TODO: impact could use attention
      retval <- unlist(sapply(veris[[tag]], getVarietyAmount))
    } else {
      retval <- getenumlist(veris[[tag]], therest)      
    }
  }
  retval
}
  

#' Internal: Expand variety
#' 
#' This will expand all of the "variety" fields by the amount
#' specified in the "amount" field of the same level object.
#' 
#' @param x a slice of a veris a verisr object
getVarietyAmount.old <- function(x) {
  variety <- x[['variety']]
  #if ('amount' %in% names(x)) {
  #  amount <- ifelse(x[['amount']]>1, x[['amount']], 1)
  #  variety <- rep(variety, amount)
  #}
  variety
}

#' Internal: Get fields names  
#' 
#' This will grab all the field names from a veris object
#' 
#' @param veris a verisr object
getvnames.old <- function(veris) {
  # all field names
  #vnames <- sort(unique(names(unlist(veris))))
  # remove the replicated numbered ones
  #vnames[grep("\\D\\d$", vnames, perl=T, invert=T)]
  sort(unique(unlist(getvnamelong(veris))))
}

#' Internal: Get fields names using a long method
#' 
#' This will grab all the field names from a veris object.
#' 
#' @param veris a verisr object
#' @param curname used to maintain state internally
getvnamelong.old <- function(veris, curname = NULL) {
  if(is.null(names(veris)) & length(veris)) {
    return(lapply(veris, getvnamelong, curname))
  }
  realname <- function(n) {
    if (is.null(curname)) n else paste0(curname, ".", n)
  }
  allthenames <- lapply(names(veris), function(x) {
    if (mode(veris[[x]]) %in% c("character", "numeric", "logical")) {
      ret <- realname(x)
    } else {
      ret <- getvnamelong(veris[[x]], realname(x))      
    }
    ret
  })
  allthenames
}

#' Displays a useful description of a verisr object
#' 
#' @param object veris object to summarise
#' @param ... other arguments ignored (for compatibility with generic)
#' @keywords internal
#' @method summary verisr
#' @export
summary.verisr.old <- function(object, ...) {
  lifecycle::deprecate_soft("2.3.1.006", "summary.verisr.old()")
  
  veris <- object
  cat(paste(nrow(veris), "incidents in this object.\n"))
  actor <- getenum(veris, "actor", add.freq=T)
  action <- getenum(veris, "action", add.freq=T)
  asset <- getenum(veris, "asset.assets", add.freq=T)
  attribute <- getenum(veris, "attribute", add.freq=T)
  actor.factor <- factor(unlist(apply(actor, 1, function(x) { rep(x[['enum']], x[['x']])})))
  action.factor <- factor(unlist(apply(action, 1, function(x) { rep(x[['enum']], x[['x']])})))
  asset.factor <- factor(unlist(apply(asset, 1, function(x) { rep(x[['enum']], x[['x']])})))
  attr.factor <- factor(unlist(apply(attribute, 1, function(x) { rep(x[['enum']], x[['x']])})))
  cat("\nActor:\n")
  print(summary(actor.factor))
  cat("\nAction:\n")
  print(summary(action.factor))
  cat("\nAsset:\n")
  print(summary(asset.factor))
  cat("\nAttribute:\n")
  print(summary(attr.factor))
}

#' Displays a four panel barplot of a verisr object
#' 
#' @param object veris object to summarise
#' @param ... other arguments ignored (for compatibility with generic)
#' @keywords internal
#' @method plot verisr
#' @export
plot.verisr.old <- function(x, y, ...) {
  lifecycle::deprecate_soft("2.3.1.006", "plot.verisr.old()")
  
#  x <- object
  actor <- getenum(x, "actor", add.freq=T)
  action <- getenum(x, "action", add.freq=T)
  asset <- getenum(x, "asset.assets", add.freq=T)
  attribute <- getenum(x, "attribute", add.freq=T)
  # save off paramteres before messing with them
  savemfrow <- par()$mfrow
  savelas <- par()$las
  savemar <- par()$mar
  # and mess with them 
  par(las=2) # make label text perpendicular to axis
  par(mar=c(2.5,5.6,2.1,1.1)) # increase y-axis margin.
  par(mfrow=c(2,2))
  # four bar plots
  barplot(actor$freq*100, names.arg=actor$enum, horiz=T, main="Actor")
  barplot(action$freq*100, names.arg=action$enum, horiz=T, main="Action")
  barplot(asset$freq*100, names.arg=asset$enum, horiz=T, main="Asset")
  barplot(attribute$freq*100, names.arg=attribute$enum, horiz=T, main="Attribute")
  par(las=savelas)
  par(mfrow=savemfrow)
  par(mar=savemar)
}

# #' Metadata for 2-digit NAICS industry classification
# #'
# #' This data allows a mapping between two digit NAICS code and the 
# #' full definition provided by the NAICS specification and a shorter
# #' version of the title for compact visuals.
# #' @name industry2
# #' @docType data
# #' @references \url{www.census.gov/naics/}
# #' @keywords data
# NULL
# 
# #' Metadata for 3-digit NAICS industry classification
# #'
# #' This data allows a mapping between three digit NAICS code and the 
# #' full definition provided by the NAICS specification.
# #' @name industry3
# #' @docType data
# #' @references \url{www.census.gov/naics/}
# #' @keywords data
# NULL
# 
# #' Metadata for victim orgsize
# #'
# #' This data allows a mapping between the enumeration of victim 
# #' employee_count and the designation of "large" and "small"  
# #' @name orgsize
# #' @docType data
# #' @references \url{www.veriscommunity.net}
# #' @keywords data
# NULL