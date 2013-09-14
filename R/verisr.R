#setClass("verisr", representation(javaMongo = "jobjRef"))

#' Read in all the JSON files in directory
#'
#' This function will iterate through all the JSON files ("json$") in
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
  jfiles <- unlist(sapply(dir, list.files, pattern = "json$", full.names=T))
  jread <- function(jfile) {
    doc <- fromJSON(file=jfile, method='C')
  }
  veris <- lapply(jfiles, jread)    
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
#' This will collect an enumation and count it.
#'
#' @param veris a verisr object
#' @param enum the field to count
#' @export
#' @examples
#' \dontrun{
#' hacking <- getenum(veris, "action.hacking.variety")
#' external <- getenum(veris, "actor.external.motive")
#' }
getenum <- function(veris, field) {
  vars <- unlist(strsplit(field, ".", fixed=T))
  field.levels <- sapply(seq_along(vars), function(x) vars[1:x])
  all.enums <- sapply(veris, function(x) {
    for(y in field.levels) {
      ret.val <- NULL
      if (! is.null(x[[y]])) { 
        ret.val <- x[[y]] 
      } else {
        break
      }
    }
    ret.val
  })
  count.enum <- table(unlist(all.enums))
  enum.df <- data.frame(enum=names(count.enum), x=as.vector(count.enum))
  enum.df <- enum.df[with(enum.df, order(x)), ]
  enum.df$enum <- factor(enum.df$enum, levels=rev(enum.df$enum), ordered=T)
  enum.df
}

#dir <- "~/Documents/github/VCDB/incidents"
#dir <- "~/Documents/json/newfinal/uscert"
# dir <- c("~/Documents/github/VCDB/incidents", "~/Documents/json/newfinal/vzint")
#vcdb <- json2veris(dir)


summary.verisr <- function(x, ...) {
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
