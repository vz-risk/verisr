\name{getenumby}
\alias{getenumby}
\title{Get a count of enumerations values by some other enumeration}
\usage{
  getenumby(veris, enum, by)
}
\arguments{
  \item{veris}{a verisr object}

  \item{enum}{the field to count}

  \item{by}{the second enumeration to sort by}
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

