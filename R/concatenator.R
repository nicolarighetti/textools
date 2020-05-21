#' Concatenate texts by group
#'
#' @description Given a data frame with texts and groups, it concatenates texts by group
#'
#' @param df a dataframe with at least textual data and a grouping variable
#' @param text_field  name of the column (in quotation marks) containing the textual data
#' @param group_field name of the column (in quotation marks) of the grouping variable
#'
#' @return a data frame with groups and the corresponding textual data
#'
#' @examples
#' \dontrun{
#' grouped_df <- concatenator(df, text_field = "texts", group_field = "groups")}
#'
#' @importFrom dplyr %>% group_by sym summarize
#'
#' @export

concatenator <- function(df, text_field = NULL, group_field = NULL) {

  df_grouped <- df %>%
    dplyr::group_by(!!dplyr::sym(group_field)) %>%
    dplyr::summarize(grouped_text = paste0(!!dplyr::sym(text_field), collapse = " "))
}

