utils::globalVariables(".")
#'
#' Create stub documentation for a data frame
#'
#' Create stub documentation for a data frame
#'
#' @param data a data frame
#' @return a character string containing a documentation stub
#' @importFrom magrittr %>%
#' @export
#' @examples
#'
#' cat(data_doc(iris))

data_doc <- function(data) {
  data_name <- lazyeval::expr_text(data)
  template <-
"#' @docType data
@#' @name DATASET
@#' @usage data(DATASET)
@#' @format  A CLASS with NROW observations on the following NCOL variables.
@#' \\itemize{
ITEMS
@#' }
@#' @source
@#'
@#' @details
"
  # work-around for roxygen checks
  template <- gsub("@#", "#", template)

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
