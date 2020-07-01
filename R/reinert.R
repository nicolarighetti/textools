#' Reinert clustering (Rainette)
#'
#' @description Given a data frame with texts, this function perform Reinert's clustering
#'
#' @param df a data frame with at least a column with textual data and a column with documents' ID
#' @param docid_field name of the column (in quotation marks) containing the IDs of the documents (default NULL)
#' @param text_field  name of the column (in quotation marks) containing textual data
#' @param min_docfreq minimum values of a feature's document frequency, below which features will be removed (default 0.5 percentile)
#' @param max_docfreq maximum values of a feature's document frequency, above which features will be removed (default 99 percentile)
#' @param k.max desired maximum number of clusters (default NULL).
#' @param min_uc_size minimum number of forms by document (default 10).
#' @param min_split_members don't try to split groups with fewer members (default 5).
#' @param cc_test contingency coefficient value for feature selection (default 0.3).
#' @param tsj minimum frequency value for feature selection (default 3).

#' @return a list containing the result of clustering and the original document term matrix, for subsequent analysis
#' through, for instance \link[rainette]{rainette_plot} or \link[rainette]{rainette_explor}
#'
#' @details the function is simply a wrapper of functions available in the package \href{https://juba.github.io/rainette/}{rainette}.
#' Please refer to the available documentations of \link[rainette]{rainette}
#'
#' @examples
#' \dontrun{
#' reinert_clustering <- reinert(df, docid_field = "URL", text_field = "Message", k = 10)
#' rainette::rainette_plot(reinert_clustering[[1]], reinert_clustering[[2]])}
#'
#' @importFrom quanteda dfm corpus dfm_trim dfm_tfidf dfm_trim
#' @importFrom rainette rainette
#'
#' @export

reinert <-
  function(df,
           docid_field = NULL,
           text_field = NULL,
           min_docfreq = 0.5, 
           max_docfreq = 99,
           segment_size = 40,
           k = NULL,
           min_uc_size = 10,
           min_split_members = 5,
           cc_test = 0.3,
           tsj = 3) {
    
    mycorpus <- quanteda::corpus(df,
                                 docid_field = docid_field,
                                 text_field = text_field)
    
    mycorpus <-
      rainette::split_segments(mycorpus, segment_size = segment_size)
    
    mydfm <-
      quanteda::dfm(mycorpus)
    
    mydfm <- quanteda::dfm_trim(mydfm, min_docfreq = min_docfreq, max_docfreq = max_docfreq, termfreq_type = "quantile")
    
    res <-
      rainette::rainette(
        mydfm,
        k = 10,
        min_uc_size = min_uc_size,
        min_split_members = min_split_members,
        cc_test = cc_test,
        tsj = tsj
      )
    
    output <- list(res, mydfm)
    
    return(output)
  }
