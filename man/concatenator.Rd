% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concatenator.R
\name{concatenator}
\alias{concatenator}
\title{Concatenate texts by group}
\usage{
concatenator(df, text_field = NULL, group_field = NULL)
}
\arguments{
\item{df}{a dataframe with at least textual data and a grouping variable}

\item{text_field}{name of the column (in quotation marks) containing the textual data}

\item{group_field}{name of the column (in quotation marks) of the grouping variable}
}
\value{
a data frame with groups and the corresponding textual data
}
\description{
Given a data frame with texts and groups, it concatenates texts by group
}
\examples{
\dontrun{
grouped_df <- concatenator(df, text_field = "texts", group_field = "groups")}

}
