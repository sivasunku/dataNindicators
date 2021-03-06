% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bullbearstrength.R
\name{bullbearstrength}
\alias{bullbearstrength}
\title{Calculate the bull/bear strength of a given xts OHLC}
\usage{
bullbearstrength(m, pct = TRUE)
}
\arguments{
\item{m}{- OHLC}

\item{pct}{- If 'm' should be normalized before finding the strength}
}
\value{
returns bullStrength,bearStrength
}
\description{
The function returns the bull/bear strength (zero if negative)
}
\author{
Siva Sunku
}
\keyword{bullbearstrength}
