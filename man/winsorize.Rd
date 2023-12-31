% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme.R
\name{winsorize}
\alias{winsorize}
\title{Winsorize a vector of data}
\usage{
winsorize(vec, limits)
}
\arguments{
\item{vec}{vector to process}

\item{limits}{vector of length 2; values beyond which outliers will be 
winsorized}
}
\description{
Winsorize a vector by clipping extreme values. If either limit is \code{NA},
the data will not be winsorized at that end.
}
