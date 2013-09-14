\name{getenum}
\alias{getenum}
\title{Get a count of enumerations values}
\usage{
  getenum(veris, field)
}
\arguments{
  \item{veris}{a verisr object}

  \item{enum}{the field to count}
}
\description{
  This will collect an enumation and count it.
}
\examples{
\dontrun{
hacking <- getenum(veris, "action.hacking.variety")
external <- getenum(veris, "actor.external.motive")
}
}

