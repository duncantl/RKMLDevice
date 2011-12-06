\name{SFTemperatures}
\alias{SFTemperatures}
\alias{PortlandTemperatures}
\alias{SeattleTemperatures}
\docType{data}
\title{Daily temperatures for 3 US cities from 1995 to Feb 2010}
\description{
  These 3 data sets are daily temperatures from 1995 to 2010
  for 3 US west coast cities - Portland, Oregon, San Francisco, CA and
   Seattle, Washington.
 This data comes from \url{http://www.engr.udayton.edu/weather/citylistUS.htm}.
}
\usage{data(SFTemperatures)}
\format{
  A data frame with 5534 observations on the following 5 variables.
  \describe{
    \item{\code{yearCount}}{a numeric vector}
    \item{\code{dayOfMonth}}{a numeric vector}
    \item{\code{year}}{a numeric vector}
    \item{\code{temp}}{a numeric vector}
    \item{\code{dates}}{a Date}
  }
}
\source{
\url{http://www.engr.udayton.edu/weather/citylistUS.htm}
}
\examples{
data(SFTemperatures)
}
\keyword{datasets}
