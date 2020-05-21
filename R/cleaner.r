#' Clean textual data
#'
#' @description Given a text vector, clean text is returned
#'
#' @param rawtext texts to clean
#' @param stopwords stopwords language. Default to "italian". It supports the following languages:
#' danish, dutch, english, finnish, french, german, hungarian, italian, norwegian, portuguese, russian, spanish, and swedish
#' @param delete_url delete URLs in the texts (default TRUE)
#' @param lowercase convert words to lower (default TRUE)
#' @param simbols keep just alphanumeric characters (default TRUE).
#' @param numbers remove numbers (default TRUE).
#' @param smallwords remove words composed of up to 2 characters (default TRUE).
#' @param spaces remove extra white spaces (default TRUE).

#' @return a text vector with cleaned textual data
#'
#' @examples
#' \dontrun{
#' clean_text <- cleaner(rawtext)}
#'
#' @importFrom tm removeWords
#'
#' @export
#'

cleaner <-
  function(rawtext, stopwords = "italian", delete_url = TRUE, lowercase = TRUE, simbols = TRUE, numbers = TRUE, smallwords = TRUE, spaces = TRUE) {

    if (delete_url == TRUE) {
      url_pattern <-
        "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
      rawtext <- gsub(url_pattern, "", rawtext)
    }

    # lower case
    if (lowercase == TRUE) {
      rawtext <- tolower(rawtext)
    }

    # remove simbols (punctuation)
    if (simbols == TRUE) {
      rawtext <- gsub("[^[:alnum:]]", " ", rawtext)
    }

    # remove numbers
    if (numbers == TRUE) {
      rawtext <- gsub("\\d", " ", rawtext)
    }

    # remove stopwords
    if (stopwords == TRUE) {
      rawtext <- tm::removeWords(rawtext, stopwords(stopwords))
    }

    # remove 1-2 letter words
    if (smallwords == TRUE) {
      rawtext <-
        gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", rawtext) # Remove 1-2 letter words
    }

    # remove extra whitespaces and leading/trailing whitespace
    if (spaces == TRUE) {
      rawtext = tm::stripWhitespace(rawtext)
      rawtext = trimws(rawtext, which = "both")
    }

    return(rawtext)
  }
