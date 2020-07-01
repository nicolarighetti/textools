#' Lemmatize texts
#'
#' @description Given a text vector, words' lemmata are returned
#'
#' @param rawtext the raw texts to lemmatize
#' @param lang language of the texts. Default to "it" (Italian). It support the following languages:
#' \itemize{
#'  \item{"it"}: {Italian}
#'  \item{"en"}: {English}
#'  \item{"de"}: {German}
#'  \item{"es"}: {Spanish}
#'  \item{"fr"}: {French}
#'  \item{"nl"}: {Dutch}
#'  \item{"pt"}: {Portuguese}
#'  \item{"ru"}: {Russian}
#' }
#'
#' @param TreeTaggerPath the file path of the local installation of Tree Tagger (default "C:/TreeTagger")
#' @param parallel enables parallel processing to speed up the lemmatization process taking advantage of multiple cores (default TRUE).
#' The number of cores is automatically set to all the available cores minus one
#'
#' @return a text vector with lemmata (nouns, names, adjectives, verbs, adverbs and unrecognized words)
#'
#' @details the function is based on \href{https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/}{TreeTagger} and the related R package \href{https://cran.r-project.org/web/packages/koRpus/index.html}{koRpus}.
#' To install TreeTagger please refer to \href{https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/}{online documentation}.
#' Language specific files available in the \href{https://undocumeantit.github.io/repos/l10n/}{following repository} are also needed.
#' The function returns the lemmata of "significant" words (nouns, names, adjectives, verbs, and adverbs) most commonly used in social science works.
#' Also unrecognized words are returned.
#'
#' @examples
#' \dontrun{
#' dataframe$lemma <- lemmatizer(rawtext=dataframe$text, lang="it",
#' TreeTaggerPath = "C:/TreeTagger", parallel=TRUE)}
#'
#' @importFrom koRpus treetag
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @import dplyr
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar%
#' @importFrom doSNOW registerDoSNOW
#'
#'
#' @export

lemmatizer <- function(rawtext, lang = "it", TreeTaggerPath = "C:/TreeTagger", parallel = TRUE) {

  if (lang == "it") {
    base::requireNamespace("koRpus.lang.it")
      lang_preset <- "koRpus.lang.it"
    } else if (lang == "en") {
      base::requireNamespace("koRpus.lang.en")
      lang_preset <- "koRpus.lang.en"
    } else if (lang == "de") {
      base::requireNamespace("koRpus.lang.de")
      lang_preset <- "koRpus.lang.de"
    } else if (lang == "es") {
      base::requireNamespace("koRpus.lang.es")
      lang_preset <- "koRpus.lang.es"
    } else if (lang == "fr") {
      base::requireNamespace("koRpus.lang.fr")
      lang_preset <- "koRpus.lang.fr"
    } else if (lang == "nl") {
      base::requireNamespace("koRpus.lang.nl")
      lang_preset <- "koRpus.lang.nl"
    } else if (lang == "pt") {
      base::requireNamespace("koRpus.lang.pt")
      lang_preset <- "koRpus.lang.pt"
    } else if (lang == "ru") {
      base::requireNamespace("koRpus.lang.ru")
      lang_preset <- "koRpus.lang.ru"
    }
  
  rawtext <- subset(rawtext, nchar(rawtext)>0) # remove empty documents

    if (parallel == TRUE) {
      # setup parallel backend
      cores <- parallel::detectCores() - 1
      cl <- parallel::makeCluster(cores)
      doSNOW::registerDoSNOW(cl)

      # progress bar
      pb <- utils::txtProgressBar(max = length(rawtext), style = 3)
      progress <- function(n)
        utils::setTxtProgressBar(pb, n)
      progress_bar <- list(progress = progress)

      lemmatized_texts <-
        foreach::foreach(i = seq(1:length(rawtext)), .combine = rbind, .packages = c("dplyr", "koRpus", "utils", lang_preset),
                         .options.snow = progress_bar) %dopar% {

          # show progress...
          utils::setTxtProgressBar(pb, pb$getVal() + 1)

          text <- rawtext[i]

          tab <- koRpus::treetag(as.vector(text),
                                 format = "obj",
                                 treetagger = "manual",
                                 lang = lang,
                                 TT.options = list(path = TreeTaggerPath, preset = lang))

                    TT.table <- data.frame(tab@TT.res)

                    lemma.df <- subset(TT.table,
                                       wclass == "noun" | wclass == "name" | wclass == "adjective" | wclass == "verb" | tag == "adverb")

          for (i in 1:nrow(lemma.df)) {
            try(if (lemma.df$lemma[i] == "<unknown>")
              lemma.df$lemma[i] <- lemma.df$token[i])
          }
          str <- paste(lemma.df$lemma, collapse = " ")
          return(str)
        }

      parallel::stopCluster(cl)

      return(lemmatized_texts)
    }

    else{
      tab <-
        koRpus::treetag(
          as.vector(str),
          format = "obj",
          treetagger = "manual",
          lang = lang,
          TT.options = list(path = "C:/TreeTagger", preset = lang)
        )

      TT.table <-
        data.frame(tab@TT.res)
      lemma.df <-
        subset(
          TT.table,
          wclass == "noun" |
            wclass == "name" |
            wclass == "adjective" |
            wclass == "verb" | tag == "adverb"
        )

      for (i in 1:nrow(lemma.df)) {
        try(if (lemma.df$lemma[i] == "<unknown>")
          lemma.df$lemma[i] <- lemma.df$token[i])
      }

      str <- paste(lemma.df$lemma, collapse = " ")
      return(str)

      # stem each text block in turn
      lemmatized_texts <- lapply(X = text, FUN = tag.text)
      lemmatized_texts <- as.character(lemmatized_texts)
      # return stemed text blocks
      return(lemmatized_texts)
    }
  }
