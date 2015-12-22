#' convert industry codes to short labels
#' 
#' given a vector of 2 or 3 digit industry codes, returns
#' a character vector of short labels as an ordered factor.
#' 
#' @param industry a vector of 2 or 3 digit  verisr object
#' @export
industry2label <- function(industry) {
  data(industry2, envir = environment())
  data(industry3, envir = environment())
  ind <- rbind(industry2, industry3)
  sapply(industry, function(x) {
    ifelse(x %in% ind$code, as.character(ind$short[which(ind$code==x)]), NA)
  })
}

#' convert veris industry field names to short labels
#' 
#' given a vector of veris names with industries, 
#' (victim.industry.44 for example), returns
#' a character vector of short labels as an ordered factor.
#' 
#' @param industry a vector of 2 or 3 digit veris industry names
#' @export
indname2label <- function(industry) {
  codes <- sapply(industry, function(x) {
    unlist(strsplit(x, '[.]'))[3]
  })
  industry2label(codes)
}

#' return a vector for ordered factors within VERIS.
#' 
#' given a VERIS enumeration field, return a vector
#' that can be used to sort the factors of the 
#' enumerations.
#' 
#' @param enum a verisr field name
#' @param unknown logical; include unknown in the enumeration
#' @export
getlevel <- function(enum, unknown=T) {
  switch(enum,
         actor=c("external", "internal", "partner", "unknown"),
         action=c("malware", "hacking", "social", "misuse", "physical", "error", "environmental", "unknown"),
         asset.assets=c("Server", "Network", "User Dev", "Media", "Person", "Kiosk/Term", "Unknown"),
         attribute=c("confidentiality", "integrity", "availability"),
         attribute.confidentiality.data.variety=c(
           "Payment", "Bank", "Credentials", "Personal", "Medical", 
           "Classified", "Copyrighted", "System", "Internal", "Secrets", 
           "Other", "Unknown"),
         timeline.unit=c( "Seconds", "Minutes", "Hours", "Days",
            "Weeks", "Months", "Years", "Never", "Unknown", "NA")
         )
}
