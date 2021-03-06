% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lemmatizer.r
\name{lemmatizer}
\alias{lemmatizer}
\title{Lemmatize texts}
\usage{
lemmatizer(
  rawtext,
  lang = "it",
  TreeTaggerPath = "C:/TreeTagger",
  parallel = TRUE
)
}
\arguments{
\item{rawtext}{the raw texts to lemmatize}

\item{lang}{language of the texts. Default to "it" (Italian). It support the following languages:
\itemize{
 \item{"it"}: {Italian}
 \item{"en"}: {English}
 \item{"de"}: {German}
 \item{"es"}: {Spanish}
 \item{"fr"}: {French}
 \item{"nl"}: {Dutch}
 \item{"pt"}: {Portuguese}
 \item{"ru"}: {Russian}
}}

\item{TreeTaggerPath}{the file path of the local installation of Tree Tagger (default "C:/TreeTagger")}

\item{parallel}{enables parallel processing to speed up the lemmatization process taking advantage of multiple cores (default TRUE).
The number of cores is automatically set to all the available cores minus one}
}
\value{
a text vector with lemmata (nouns, names, adjectives, verbs, adverbs and unrecognized words)
}
\description{
Given a text vector, words' lemmata are returned
}
\details{
the function is based on \href{https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/}{TreeTagger} and the related R package \href{https://cran.r-project.org/web/packages/koRpus/index.html}{koRpus}.
To install TreeTagger please refer to \href{https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/}{online documentation}.
Language specific files available in the \href{https://undocumeantit.github.io/repos/l10n/}{following repository} are also needed.
The function returns the lemmata of "significant" words (nouns, names, adjectives, verbs, and adverbs) most commonly used in social science works.
Also unrecognized words are returned.
}
\examples{
\dontrun{
dataframe$lemma <- lemmatizer(rawtext=dataframe$text, lang="it",
TreeTaggerPath = "C:/TreeTagger", parallel=TRUE)}

}
