# Textools

Textools is a collection of tools to perform common text analysis operations in R. It is essentialy a convenience wrapper for consolidate functions included in great and well-known packages like [quanteda](https://quanteda.io/), [FactoMineR](http://factominer.free.fr/), [factoextra](https://github.com/kassambara/factoextra), [koRpus](https://cran.r-project.org/web/packages/koRpus/vignettes/koRpus_vignette.html), and [rainette](https://juba.github.io/rainette/) among others. 

It also includes a lemmatizer based on [Tree Tagger](https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/) and [koRpus](https://cran.r-project.org/web/packages/koRpus/vignettes/koRpus_vignette.html) that works with many languages.

Textools is not a replacement for the above-mentioned packages, which include an incredible number of great functions. Its main aim is simply to shorten my code and save time when performing some common text analyses such as clustering, correspondence analysis, and text cleaning operations, for instance by including in the same function the preprocessing steps (e.g. the creation of the document-term-matrix) and the analysis itself. In other case, textools levarages the above-mentioned packages to add new useful functions, such as in the case of the "lemmatizer".

The package is a work in progress.

## Features

Currently, textools includes:
* a "cleaner" to clean textual data
* a "concatenator" to merge textual data by group
* a "lemmatizer" to perform text lemmatization in many languages
* a "clusterizer" to perform text clustering (kmeans and hierarchical clusterning based on cosine metric) and a function "clusterms" to extract cluster-related terms 
* a "reinert" function to perform text clustering by using the Reinert's method
* a "lca" function to perform lexical correspondence analysis
