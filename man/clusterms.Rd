% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clusterms.R
\name{clusterms}
\alias{clusterms}
\title{Get terms by cluster}
\usage{
clusterms(
  df,
  cluster_field = NULL,
  docid_field = NULL,
  text_field = NULL,
  clean = FALSE,
  lang = NULL,
  n = 10
)
}
\arguments{
\item{df}{a dataframe with at least a column with textual data, cluster's and documents' IDs}

\item{cluster_field}{name of the column (in quotation marks) containing the clusters' IDs (default NULL)}

\item{docid_field}{name of the column (in quotation marks) containing the documents' ID (default NULL)}

\item{text_field}{name of the column (in quotation marks) containing textual data}

\item{clean}{clean the text from stopwords, punctuation, symbols etc. (default FALSE)}

\item{lang}{if clean=TRUE, langauge of the stopword should be specified. It supports the following languages:
danish, dutch, english, finnish, french, german, hungarian, italian, norwegian, portuguese, russian, spanish, and swedish}

\item{n}{number of top words to return}
}
\value{
a data frame with the most frequent and specific words of each cluster
}
\description{
Given a data frame with clusters, top words by cluster are returned
}
\details{
the most specific words of each clusters are computed through the chi-squared statistics as implemented in \link[quanteda]{textstat_keyness}
}
\examples{
\dontrun{
top_terms <- clusterm(df, cluster_field = "cluster", text_field = "texts")}

}
