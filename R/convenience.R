#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#'
#' @importFrom magrittr %>%
NULL


#' Convenience function to pull the logical columns from verisr object
#' 
#' Given a verisr object this will return a vector of column names which are logical values.
#' 
#' @param veris the verisr object
#' @export
getlogical <- function(veris) {
  our.cols <- sapply(veris, mode)
  names(our.cols[grep('^logical$', our.cols)])
}

#' Metadata for 2-digit NAICS industry classification
#'
#' This data allows a mapping between two digit NAICS code and the 
#' full definition provided by the NAICS specification and a shorter
#' version of the title for compact visuals.
#' @name industry2
#' @docType data
#' @references \url{www.census.gov/naics/}
#' @keywords data
NULL

#' Metadata for 3-digit NAICS industry classification
#'
#' This data allows a mapping between three digit NAICS code and the 
#' full definition provided by the NAICS specification.
#' @name industry3
#' @docType data
#' @references \url{www.census.gov/naics/}
#' @keywords data
NULL


#' sample data file to test/explore verisr
#'
#' This is a collection of the 1000 most complete incidents within the VCDB database.
#' See the example below on how the data was generated.
#' 
#' @name veris.sample
#' @docType data
#' @keywords data
#' @examples
#' \dontrun{
#' # set vcdbdir and schema file
#' 
#' vcdb <- json2veris(vcdbdir, schemafile, progressbar = T)
#' mycols <- getlogical(vcdb)
#' testdata <- vcdb[order(-rowSums(vcdb[, mycols, with=F]))]
#' got.action <- as.vector(testdata[ ,'action.Unknown', with=F] == F)
#' got.actor <- as.vector(testdata[ ,'actor.Unknown', with=F] == F)
#' got.asset <- as.vector(testdata[ ,'asset.assets.variety.Unknown', with=F] == F)
#' veris.sample <- head(testdata[got.actor & got.action & got.asset, ], 1000)
#' # cast this into a verisr object for future use.
#' class(veris.sample) <- c("verisr", "data.table", "data.frame")
#' save(veris.sample, file="data/veris.sample.rda", compress="xz")
#' }
NULL
