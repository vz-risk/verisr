% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ci.R
\name{test_veris_consistency}
\alias{test_veris_consistency}
\title{Test whether two enumerations are basically equal}
\usage{
test_veris_consistency(
  chunk,
  Ea,
  Eb,
  ci.level = 0.05,
  reps = 1000,
  quietly = FALSE,
  visualize = FALSE
)
}
\arguments{
\item{chunk}{getenumCI() object}

\item{Ea}{Enumeration A.  e.g. "action.Error"}

\item{Eb}{Enumeration B.  e.g. "action.Misuse"}

\item{ci.level}{the confidence level to test against}

\item{reps}{number of simulations to conduct}

\item{quietly}{do not produce textual output}

\item{visualize}{produce visual output}
}
\value{
a logical TRUE/FALSE to the hypothesis
}
\description{
This is most useful for testing an enumerations whether an 
enumeration has stayed the same since last year.  It 
functions by creating an null distribution around the 
observed proportion, and then checking the probability of 
the proportion being 0 (i.e. no change).
}
\details{
Technically instead of 'true/false', the language should really 
be along the lines of "we have evidence for the alternative  
hypothesis ..." or "we do not have evidence to go against our 
original null hypothesis ...", but for simplicity we have left
it the way it is.
}
\examples{
\dontrun{
tmp <- tempfile(fileext = ".dat")
download.file("https://github.com/vz-risk/VCDB/raw/master/data/verisr/vcdb.dat", tmp, quiet=TRUE)
load(tmp, verbose=TRUE)
vcdb \%>\%
  filter(timeline.incident.year \%in\% 2015:2020) \%>\%
  filter(attribute.confidentiality.data_disclosure.Yes) \%>\%
  verisr::getenumCI2020(
    "attribute.confidentiality.data.variety", 
    by="plus.dbir_year") \%>\%
  filter(!is.na(by)) \%>\%
  filter(enum == "Bank")  \%>\%
  select(-enum) \%>\%
  rename(enum = by)  \%>\%
  verisr::test_veris_consistency(Ea="2019", Eb="2020")
}
}
