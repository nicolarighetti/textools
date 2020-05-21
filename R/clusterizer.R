#' Text clustering
#'
#' @description Given a data frame with texts, documents (or features) clustering is returned
#'
#' @param df a data frame with at least a column with textual data and a column with documents' ID
#' @param docid_field name of the column (in quotation marks) containing the IDs of the documents (default NULL)
#' @param text_field  name of the column (in quotation marks) containing textual data
#' @param min_docfreq minimum values of a feature's document frequency, below which features will be removed (default 0.5 percentile)
#' @param max_docfreq maximum values of a feature's document frequency, above which features will be removed (default 99 percentile)
#' @param tfidf term frequency inverse document frequency weighting (default TRUE)
#' @param element elements to cluster. Available options are "documents" and "features" (default "documents")
#' @param k desired number of clusters (default NULL).
#' If NULL, the silhouette method is used to estimate the appropriate number of clusters
#' @param k.max max number of cluster if k is not specified (default NULL)
#' @param nstart number of initial configurations (default 25)
#' @param method clustering method. Available options are "kmeans" and "hclust" (default "kmeans)
#' @param hc_method the agglomeration method to be used in case of "hclust" method.
#' This should be one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA),
#' "median" (= WPGMC) or "centroid" (= UPGMC). See \link[stats]{hclust}
#' @param return_fit return the fitted cluster_model in the main environment
#'
#' @return an vector of cluster IDs. Silhouette information with clusters' size and average silhouette width are printed in console
#'
#' @details the function is substantially a wrapper of functions available in \href{https://quanteda.io/}{quanteda} and  \href{https://rpkgs.datanovia.com/factoextra/index.html}{factoextra}.
#' Please refer to the available documentations of \link[quanteda]{textstat_simil} and \link[factoextra]{eclust}.
#' The fitted cluster_model can be used to create kmeans plots with \link[factoextra]{fviz_cluster} or dendrogram (hclust) wiht \link[factoextra]{fviz_dend}
#'
#' @examples
#' \dontrun{
#' df$cluster <- clusterizer(df, docid_field = "documents", text_field = "texts", k = 10)}
#'
#' @importFrom quanteda dfm corpus dfm_trim dfm_tfidf textstat_simil rowSums
#' @importFrom factoextra eclust fviz_nbclust
#' @importFrom stats kmeans
#'
#' @export

clusterizer <-
  function(df, docid_field = NULL, text_field = NULL, min_docfreq = 0.5, max_docfreq = 99, tfidf = TRUE, element = "documents",
           k = NULL, k.max = NULL, nstart = 25, method = "kmeans", hc_method = NULL, return_fit = FALSE) {

    mycorpus <- quanteda::corpus(df,
                                 docid_field = docid_field,
                                 text_field = text_field)
    mydfm <-
      quanteda::dfm(mycorpus)

    mydfm <- quanteda::dfm_trim(mydfm, min_docfreq = min_docfreq, max_docfreq = max_docfreq, termfreq_type = "quantile")

    mydfm <- mydfm[quanteda::rowSums(mydfm) !=0, ]

    if (isTRUE(tfidf)) {
      mydfm <- quanteda::dfm_tfidf(mydfm)
    }

    # (dis)similarity matrix
    sim <- quanteda::textstat_simil(mydfm,
                                    margin = element,
                                    method = "cosine")
    d <- 1 - sim
    d <- as.data.frame(as.matrix(d))

    # remove zero variance columns (it make impossibile to do kmeans)
    # d <- d[, apply(d, 2, var) != 0]

    if (is.null(k) & !is.null(k.max)){
    c <- factoextra::fviz_nbclust(d, FUNcluster = method, "silhouette", k.max = k.max)
    k <- c$data$clusters[c$data$y == max(c$data$y)]
    k <- as.numeric(as.character(k))
    }

    fit <-
      factoextra::eclust(
        d,
        FUNcluster = method,
        hc_method = hc_method,
        k = k,
        nstart = nstart,
        graph = FALSE
      )

    cat("\nSilhouette")
    sil <- factoextra::fviz_silhouette(fit)
    cat("\nAverage silhouette width: ", round(mean(sil$data$sil_width), 2))

    if (return_fit == TRUE) {
      cluster_model <<- fit
    }

    dd <- data.frame(docs_id = df[[docid_field]])
    d <- data.frame(docs_id = rownames(d), clusters = as.vector(fit$cluster))
    dd <- merge(dd, d, by = "docs_id", all.x = T)

    return(dd$clusters)
  }
