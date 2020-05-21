#' Lexical Correspondence Analysis
#'
#' @description correspondence analysis of textual data
#'
#' @param df a dataframe with at least a column with textual data and a column with documents' ID
#' @param docid_field name of the column (in quotation marks) containing the IDs of the documents (default NULL)
#' @param text_field  name of the column (in quotation marks) containing textual data
#' @param min_docfreq minimum values of a feature's document frequency, below which features will be removed (default 0.5 percentile)
#' @param max_docfreq maximum values of a feature's document frequency, above which features will be removed (default 99 percentile)
#'
#' @return a correspondence analysis model
#'
#' @details the function is substantially a wrapper of functions available in \href{https://quanteda.io/}{quanteda} and \href{https://rpkgs.datanovia.com/factoextra/index.html}{factoextra}.
#' More specifically it leverages the correspondence analysis function \link[FactoMineR]{CA}.
#' The fitted ca_model can be used to further explore the model and create plots with \link[factoextra]{fviz_ca_biplot}
#'
#' @examples
#' \dontrun{
#' dataframe$cluster <- lca(df, docid_field = "documents", text_field = "texts")}
#'
#' @importFrom quanteda dfm corpus dfm_trim dfm_tfidf convert rowSums
#' @importFrom FactoMineR CA
#'
#' @export


lca <- function(df, docid_field = NULL, text_field = NULL, min_docfreq = 0.5, max_docfreq = 99) {

  mycorpus <-
    quanteda::corpus(df,
                     docid_field = docid_field,
                     text_field = text_field)
    mydfm <-
      quanteda::dfm(mycorpus)

    mydfm <-
      quanteda::dfm_trim(mydfm, min_docfreq = min_docfreq, max_docfreq = max_docfreq, termfreq_type = "quantile")

    mydfm <- mydfm[quanteda::rowSums(mydfm) != 0,]

    mydf <- quanteda::convert(mydfm, to = "data.frame")

    rownames(mydf) <- mydf$document
    mydf <- mydf[,-1]

    ca_model <- FactoMineR::CA(mydf, graph = F)

    return(ca_model)
  }
