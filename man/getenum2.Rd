% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/verisr.R
\name{getenum2}
\alias{getenum2}
\title{Extract counts from one or more enumerations}
\usage{
getenum2(
  veris,
  enum,
  primary = NULL,
  secondary = NULL,
  filter = NULL,
  add.n = T,
  add.freq = T,
  fillzero = T,
  exclusive = F
)
}
\arguments{
\item{veris}{a verisr object}

\item{enum}{the main enumeration field}

\item{primary}{the primary enumeration to filter on}

\item{secondary}{the (optional) secondary enumeration to filter on}

\item{filter}{limit what records are searched (optional)}

\item{add.n}{include a total count of variables found (denominator)}

\item{add.freq}{include a percentage (x/n)}

\item{fillzero}{fill in missing matches with zeros}

\item{exclusive}{logical value, If true, will count the unknown value only if it exclusive and it will not count the Unknown if it is selected with other attributes in the enumeration.}
}
\description{
When exploring VERIS data, you may want to get a simple count of the values within a value or enumeration.  
Given one or more enumerations, this will return the subsequent underlying logical values in an ordered data frame.  
The data frame should be formatted for use in \code{ggplot2} graphics.
}
\details{
As of version 1.1: the \code{enum} variable may be a vector of one or more enumerations.  
This enables any number of dimensions to be specified.  This makes the \code{primary} and \code{secondary}
obsolete but are still supported for the time being.

Note there are some special values that can be set as the enumeration, 
that may not be obvious. :
* actor, action, attribute: will all return the next level down.  For example, just querying for "action" will return "malware", "hacking", and so on.
* action.variety: will return the variety enumerations across all actions (e.g. top N actions) (not in getenumby() yet)
* asset.variety: will return the type of assets, "Server", "Network, "User Dev" and so on
* victim.industry2: will return a short label of industries based on 2 values of NAICS code.
* victim.industry3: will return a short label of industries based on 3 values of NAICS code.
* pattern: will return the pattern the incidents are in.

Change in 1.1: the "add.n" and "add.freq" options are now TRUE by default.
#' @aliases getenumby
}
\examples{
\dontrun{
# old method:
a2 <- getenum(veris, "action", primary="asset.variety")
# new method:
a4 <- getenum(veris, c("action", "asset.variety", "actor", "attribute"))
}
}
