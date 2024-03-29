% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abcddata-utilities.R
\name{abcddata_rows_with_missing}
\alias{abcddata_rows_with_missing}
\title{Identify Rows with Missing Data}
\usage{
abcddata_rows_with_missing(dtf_data, vec_missing = c(NA, ""), lgc_any = TRUE)
}
\arguments{
\item{dtf_data}{A data frame.}

\item{vec_missing}{A vector with codes
for missing values (can include \code{NA}).}

\item{lgc_any}{A logical value; if \code{TRUE}
flags rows with any missing values, otherwise
flags rows with all missing values.}
}
\value{
A logical vector.
}
\description{
Function to identify rows with any/all
missing data.
}
\examples{
dtf_data <- data.frame(
  C1 = c( NA, '', 'A', '' ),
  C2 = c( 'B', NA, 'C', '' )
)

abcddata_rows_with_missing( dtf_data )
abcddata_rows_with_missing( dtf_data, vec_missing = NA )
abcddata_rows_with_missing( dtf_data, lgc_any = FALSE )

}
