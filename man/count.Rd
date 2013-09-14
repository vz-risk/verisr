\name{count}
\alias{count}
\title{Count the existance of a field in all records}
\usage{
  count(veris, field)
}
\arguments{
  \item{veris}{a verisr object}

  \item{field}{the field to count}
}
\description{
  This function will count where the field is not null
}
\examples{
\dontrun{
hacking <- count("action.hacking")
external <- count("actor.external")
}
}

