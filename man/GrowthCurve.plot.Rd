% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GrowthCurve.plot.R
\name{GrowthCurve.plot}
\alias{GrowthCurve.plot}
\title{GrowthCurve.plot}
\usage{
GrowthCurve.plot(Sn, ph0 = seq(1, 4), Unique = FALSE, debug = FALSE, ...)
}
\arguments{
\item{Sn}{[list] (**required**) efficiencies S table (see Sn function)}

\item{ph0}{[numeric] (**with default**): selected preheat}

\item{Unique}{[logical]  (**with default**)  TRUE a single De, independent from preheat temperature}

\item{debug}{if \code{FALSE} (default), \pkg{WinBUGS} is closed automatically
    when the script has finished running, otherwise \pkg{WinBUGS} remains open
    for further investigation}

\item{...}{further arguments that will be passed to the function BayesCal}
}
\value{
a plot

WinBUg results object
}
\description{
growthcurve plot at a single preheat temperature
}
\examples{
data(Anatolian2, envir = environment())
file<-Anatolian2$FILE

\dontrun{
GrowthCurve.plot(Sn(file,ech=1,OSL=2,Dose=c(0,200,250,300,0,200)),
                ph0=c(1,2,3,4),
                  Unique=FALSE)
}

}
