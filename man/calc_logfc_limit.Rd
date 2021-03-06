% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_logfc_limit.R
\name{calc_logfc_limit}
\alias{calc_logfc_limit}
\title{calculate logfc limits}
\usage{
calc_logfc_limit(df, logfc_direction = "both")
}
\arguments{
\item{df}{a data.frame with column logFC}

\item{logfc_direction}{the direction of logfc. A string of either
'negative', 'positive' or 'both'.}
}
\description{
returns the maximum absoluate values plus 0.5 of the logFC.
}
\seealso{
Other shiny: 
\code{\link{add_plotly_markers_search_pathway}()},
\code{\link{add_plotly_markers_search}()},
\code{\link{get_replicate_summary_text}()},
\code{\link{parse_uploaded_file}()}
}
\concept{shiny}
