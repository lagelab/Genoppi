% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{is_cols}
\alias{is_cols}
\title{is columns}
\usage{
is_cols(df, col, test)
}
\arguments{
\item{df}{the data.frame}

\item{col}{string. Column name regex.}

\item{test}{a function. E.g. is.numeric}
}
\description{
conducts a test on a specific
subset of columns in the data.frame
}
