#' Get terms by cluster
#'
#' @description Given a data frame with clusters, top words by cluster are returned
#'
#' @param df a dataframe with at least a column with textual data, cluster's and documents' IDs
#' @param cluster_field name of the column (in quotation marks) containing the clusters' IDs (default NULL)
#' @param docid_field name of the column (in quotation marks) containing the documents' ID (default NULL)
#' @param text_field  name of the column (in quotation marks) containing textual data
#' @param clean clean the text from stopwords, punctuation, symbols etc. (default FALSE)
#' @param lang if clean=TRUE, langauge of the stopword should be specified. It supports the following languages:
#' danish, dutch, english, finnish, french, german, hungarian, italian, norwegian, portuguese, russian, spanish, and swedish
#' @param n number of top words to return
#'
#' @return a data frame with the most frequent and specific words of each cluster
#'
#' @details the most specific words of each clusters are computed through the chi-squared statistics as implemented in \link[quanteda]{textstat_keyness}
#'
#' @examples
#' \dontrun{
#' top_terms <- clusterm(df, cluster_field = "cluster", text_field = "texts")}
#'
#' @importFrom quanteda dfm corpus topfeatures textstat_keyness
#'
#' @export


clusterms <-
  function(df, cluster_field = NULL, docid_field = NULL, text_field = NULL, clean = FALSE, lang = NULL, n = 10) {
    df <- df[!is.na(df[[cluster_field]]), ]

    df[[cluster_field]] <- as.factor(df[[cluster_field]])

    if (clean == TRUE) {
      df[[text_field]] <- cleaner(df[[text_field]])
    }

    mydfm <-
      quanteda::dfm(
        quanteda::corpus(df,
                         docid_field = docid_field,
                         text_field = text_field)
      )

    # top words
    top_words <-
      quanteda::topfeatures(mydfm, groups = cluster_field, n = n)

    # specificity
    specificity_list <- list()
    l <- length(levels(df[[cluster_field]]))

      for (i in 1:l) {
        specificity_list[i]  <- quanteda::textstat_keyness(mydfm,
                                                    measure = "chi2",
                                                    target = i)[1:n, 1]
      }

    # top words and specificity table
    top_terms <- data.frame(matrix(data = NA, nrow = l*2, ncol = n))
    counter <- 0

    for (i in 1:l*2-1) {
        counter <- counter + 1
        top_terms[i,] <- rownames(data.frame(top_words[[counter]][1:n]))
      }

    for (i in 1:l*2) {
        top_terms[i,] <- specificity_list[[i/2]]
      }

    rownames(top_terms) <- paste(paste("Cluster", rep(1:l, each=2), rep(c("Top Words", "Specificity"), l)))

    return(top_terms)
  }
