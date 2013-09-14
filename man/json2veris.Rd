\name{json2veris}
\alias{json2veris}
\title{Read in all the JSON files in directory}
\usage{
  json2veris(dir = ".")
}
\arguments{
  \item{dir}{the directory to list through}
}
\description{
  This function will iterate through all the JSON files
  (regex pattern of "json$") in the given directory and
  parse it as a VERIS object. This will return a verisr
  object.
}
\examples{
\dontrun{
veris <- json2veris(dir="~/vcdb")
}
}
\keyword{json}

