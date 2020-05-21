# textools

Textools is a collection of tools to perform common text analysis operations in R. It is essentialy a convenience wrapper for consolidate functions included in great and well-known packages like [quanteda](https://quanteda.io/), [FactoMineR](http://factominer.free.fr/), [factoextra](https://github.com/kassambara/factoextra), and [koRpus](https://cran.r-project.org/web/packages/koRpus/vignettes/koRpus_vignette.html), among others. 

It also includes a lemmatizer based on [Tree Tagger](https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/) and [koRpus](https://cran.r-project.org/web/packages/koRpus/vignettes/koRpus_vignette.html) that works on multiple language including Italian.

Textools is not a replacement for the above-mentioned packages, which include an incredible number of great functions. Its main aim is simply to shorten the code and save time when performing some common text analyses such as clustering, correspondence analysis, and text cleaning operations, for instance by including in the same function the preprocessing steps (e.g. the creation of a document-term-matrix) and the analysis itself. In other case it levarages these packages to add new useful function, such as in the case of the "lemmatizer".

The package is a work in progress.

## Features

Currently textools includes:
* a "cleaner" to clean textual data
* a "concatenator" to merge textual data by group
* a "lemmatizer" to perform text lemmatization in many languages
* a "clusterizer" to perform text clustering (kmeans and hierarchical clusterning based on cosine metrics) and a function "clusterms" to extract cluster-related terms 
* a "lca" function to perform lexical correspondence analysis

## Installation
You can install textools from GitHub.

``` r
# install.packages("devtools")

library("devtools")
devtools::install_github("nicolarighetti/textools")
```
