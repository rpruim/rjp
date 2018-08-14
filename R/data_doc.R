#'
#' Create stub documentation for a data frame
#'
#' Create stub documentation for a data frame
#' @import magrittr
#' @param data a data frame
#' @return a character string containing a documentation stub
#' @export
#' @examples
#' data_doc(iris) %>% cat()
#'
data_doc <- function(data) {
  data_name <- lazyeval::expr_text(data)
  template <-
"#' @docType data
#' @name DATASET
#' @usage data(DATASET)
#' @format  A CLASS with NROW observations on the following NCOL variables.
#' \\itemize{
ITEMS
#' }
#' @source
#'
#' @details
"
  itemsStr <- paste("#'    \\item{\\code{", names(data), " }}{[",
                    sapply(names(data), function(x) class(data[[x]])[1]),
                    "]}", sep = "", collapse = "\n")
  template %>%
    gsub("DATASET", data_name, .) %>%
    gsub("CLASS", class(data)[1], .) %>%
    gsub("NROW", nrow(data), .) %>%
    gsub("NCOL", ncol(data), .) %>%
    gsub("ITEMS", itemsStr, .)
}
