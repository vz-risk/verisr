\name{getenum}
\alias{getenum}
\title{Get a count of enumerations values}
\usage{
  getenum(veris, enum, add.n = F, add.freq = F)
}
\arguments{
  \item{veris}{a verisr object}

  \item{enum}{the field to count}

  \item{add.n}{include a total count of variables found
  (denominator)}

  \item{add.freq}{include a percentage (x/n)}
}
\description{
  This will collect the values for an enumation and count
  them up.
}
\examples{
\dontrun{
hacking <- getenum(veris, "action.hacking.variety")
external <- getenum(veris, "actor.external.motive")
}
}

