#' Read in all the JSON files in directory
#'
#' This function will iterate through all the JSON files (regex pattern of "json$") in
#' the given directory and parse it as a VERIS object.
#' This will return a verisr object.
#'
#' @param dir the directory to list through
#' @keywords json
#' @export
#' @import rjson
#' @examples
#' \dontrun{
#' veris <- json2veris(dir="~/vcdb")
#' }
json2veris <- function(dir=".") {
  # create listing of files
  jfiles <- unlist(sapply(dir, list.files, pattern = "json$", full.names=T))
  jread <- function(jfile) {
    doc <- fromJSON(file=jfile, method='C')
  }
  # now read them all
  veris <- lapply(jfiles, jread)    
  # set my class
  class(veris) <- "verisr"
  veris
}

#' Count the existance of a field in all records
#'
#' This function will count where the field is not null
#'
#' @param veris a verisr object
#' @param field the field to count
#' @export
#' @examples
#' \dontrun{
#' hacking <- count(veris, "action.hacking")
#' external <- count(veris, "actor.external")
#' }
count <- function(veris, field) {
  sapply(field, function(f) {
    vars <- unlist(strsplit(f, ".", fixed=T))
    sum(unlist(lapply(veris, function(x) ifelse(is.null(x[[vars]]), 0, 1))))
  })
}

#' Get a count of enumerations values
#'
#' This will collect the values for an enumation and count them up.
#'
#' @param veris a verisr object
#' @param enum the field to count
#' @param add.n include a total count of variables found (denominator)
#' @param add.freq include a percentage (x/n)
#' @export
#' @examples
#' \dontrun{
#' hacking <- getenum(veris, "action.hacking.variety")
#' external <- getenum(veris, "actor.external.motive")
#' }
getenum <- function(veris, enum, add.n=F, add.freq=F) {
  # get the internal list for the enumeration
  int.enum <- getintenum(veris, enum)
  # count and aggreagte it
  count.enum <- table(unlist(int.enum))
  # convert to data.frame
  enum.df <- data.frame(enum=names(count.enum), x=as.vector(count.enum))
  # order it
  enum.df <- enum.df[with(enum.df, order(x)), ]
  # make the enum a factor
  enum.df$enum <- factor(enum.df$enum, levels=rev(enum.df$enum), ordered=T)
  if (add.n | add.freq) {
    n <- sum(sapply(int.enum, function(x) {
      ifelse(length(x)==1,
             ifelse(is.na(x), 0, 1)
             ,1) })
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

#' Get a count of enumerations values by some other enumeration
#'
#' This will collect an enumation and count it.
#'
#' @param veris a verisr object
#' @param enum the field to count
#' @param by the second enumeration to sort by
#' @export
#' @examples
#' \dontrun{
#' hacking <- getenum(veris, "action.hacking.variety")
#' external <- getenum(veris, "actor.external.motive")
#' }
getenumby <- function(veris, enum, by) {
  # get the internal list for the enumeration
  primary.enum <- getintenum(veris, enum)
  #secondary.df <- getenum(veris, by)
  if(by=="industry2") {
    secondary.enum <- getindustry(veris, 2)
  } else {
    secondary.enum <- getintenum(veris, by)
  }
  by.list <- unique(unlist(secondary.enum))
  by.list <- by.list[!is.na(by.list)]
  rez <- do.call(rbind, lapply(by.list, function(x) {
    local.filter <- getsimfilter(secondary.enum, x)
    pri.count <- table(unlist(primary.enum[local.filter]))
    if (length(pri.count)) {
      int.df <- data.frame(enum=names(pri.count), 
                           x=as.vector(pri.count), 
                           primary=x)      
    } else {
      int.df <- NULL
    }
    int.df
  }) )
  if(by=="industry2") {
    rez$primary <- merge(rez, industry2, by.x="primary", by.y="code")$title
  }
  rez
}  
#foo <-getenumby(vcdb, "action.hacking.variety", "actor.external.variety")
#foo <-getenumby(vcdb, "action.hacking.variety", "industry2")
#ggplot(foo, aes(enum, x)) + geom_bar(stat="identity") + facet_wrap( ~ primary, ncol=2) + coord_flip() + theme_bw()

getindustry <- function(veris, len=2) {
  int.enum <- getintenum(veris, "victim.industry")
  sapply(int.enum, function(x) {
    sapply(x, function(y) {
      ret.val <- NA
      if (nchar(y) > len) {
        ret.val <- substr(y, 1, len)
        if (ret.val==rep("0", len)) {
          ret.val <- NA
        }
      } 
      ret.val      
    })
  })  
}

#' Internal Function.
#' This will iterate through the veris object and return
#' a list of matches.  This is intented to maintain the orginal
#' indexes of the veris object so further manipulation can be done.
#'
#' @param veris a verisr object
#' @param enum the field to count
getintenum <- function(veris, enum) {
  # if the veris object has null names and yet length
  # it is an array, and we simply want to step into
  # and through it.  The top level veris object
  # is an array, as is things like victim and assets
  # and data variety
  if(is.null(names(veris)) & length(veris)) {
    return(lapply(veris, getintenum, enum))
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
    # else if we are at the end of our enum, return the value
    retval <- veris[[tag]]
  } else {
    # else we need to continue to "drill down" into the veris object
    # with the rest of the enum being quieried
    # passing it back to self so it can parse through arrays
    # and use the same logic to continue parsing
    retval <- getintenum(veris[[tag]], therest)
  }
  retval
}

#' Internal Function.
#' This will create a mask of indexes based on filter
#' a list of matches.  This is intented to maintain the orginal
#' indexes of the veris object so further manipulation can be done.
#'
#' @param veris a verisr object
#' @param enum the field to count
#' @param value 
getfilter <- function(veris, enum, value) {
  # test if value is regex!
  int.enum <- getintenum(veris, enum)
  sapply(int.enum, function(x) { ifelse(value %in% x, TRUE, FALSE)})
}
  
getsimfilter <- function(int.enum, value) {
  sapply(int.enum, function(x) { ifelse(value %in% x, TRUE, FALSE)})
}  



#dir <- "~/Documents/github/VCDB/incidents"
#dir <- "~/Documents/json/newfinal/uscert"
# dir <- c("~/Documents/github/VCDB/incidents", "~/Documents/json/newfinal/vzint")
#vcdb <- json2veris(dir)

#' Displays a useful description of a ggplot object
#' 
#' @param object veris object to summarise
#' @param ... other arguments ignored (for compatibility with generic)
#' @keywords internal
#' @method summary verisr
#' @export
summary.verisr <- function(object, ...) {
  x <- object
  cat(paste(length(x), "incidents in this object.\n"))
  actor <- c("ext"="actor.external", 
            "int"="actor.internal",
            "prt"="actor.partner")
  cat("\n")
  print(count(x, actor))
  action <- c("mal"="action.malware",
              "hak"="action.hacking",
              "soc"="action.social",
              "mis"="action.misuse",
              "err"="action.error",
              "phy"="action.physical",
              "env"="action.environmental")
  cat("\n")
  print(count(x, action))
}

