% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_correspondence_tables.r
\name{get_correspondence_tables}
\alias{get_correspondence_tables}
\title{Get correspondence tables from UN stat}
\usage{
get_correspondence_tables(digit_level = 6)
}
\arguments{
\item{digit_level}{number of digits to create the correspondence table for, e.g., '6' for 6-digit, etc.}
}
\value{
A \code{dataframe} containing the following columns, also writes the dataframe to \code{data/full_correspondence_tables.csv}:

\item{first}{the 'to' product code}
\item{second}{the 'from' product code}
\item{first_name}{the hs version of the 'to' product code}
\item{second_name}{the hs version of the 'from' product code}
\item{relationship}{the correspondence type of this product pair, e.g. 1:n, n:n, etc.}
}
\description{
Gets all correspondence tables from the UN and generates a CSV with all the mappings
}
