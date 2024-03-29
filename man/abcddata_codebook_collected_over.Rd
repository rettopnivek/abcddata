% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abcddata-codebook.R
\name{abcddata_codebook_collected_over}
\alias{abcddata_codebook_collected_over}
\title{Determine What Entries a Variable was Collected Over}
\usage{
abcddata_codebook_collected_over(
  dtf_base,
  chr_variable,
  chr_factors,
  obj_missing = NA
)
}
\arguments{
\item{dtf_base}{A data frame.}

\item{chr_variable}{A character string, the
column name for the variable of interest.}

\item{chr_factors}{A character vector, the
column names for the variables whose
entries should be considered.}

\item{obj_missing}{An optional vector with
codes for missing data.}
}
\value{
A list of two vectors of matching length
labeled 'content' and 'additional_content'
respectively.
}
\description{
Function to determine the unique entries over which
a variable was collected over. Useful, for example,
to determine the time points over which a variable
was collected.
}
\examples{
# Example data set
dtf_base <- data.frame(
  IDS.CHR.G.G.Site = c( '1', '2', '1', '2' ),
  SSS.CHR.G.G.Time_point = c( 'A', 'A', 'B', 'B' ),
  QST.INT.G.Y.Example = c( 1, 2, NA, NA )
)
abcddata_codebook_collected_over(
  dtf_base, 'QST.INT.G.Y.Example',
  c( 'IDS.CHR.G.G.Site', 'SSS.CHR.G.G.Time_point' )
)

}
