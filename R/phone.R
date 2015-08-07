#' Extract phone numbers
#'
#' Extract phone numbers
#'
#' @importFrom tidyr extract_numeric
#' @export
#' @examples
#' p <- c("1234567", "1234567890", "(123) 456-7890", "123.456.7890")
#' extract_phone(p)
#' extract_phone(p, area=555)
#'
extract_phone <- function(x, area = NULL) {
  cx <- gsub("[^0123456789 ]", "", as.character(x))
  nx <- tidyr::extract_numeric(cx) %% 1e10
  if (!is.null(area)) {
    tidyr::extract_numeric(area)
  } else {
    area <- 0
  }
  nx <-
    ifelse(
      nx < 1e7 & !is.null(area),
      tidyr::extract_numeric(area) * 1e7 + nx,
      nx)

  area <- trunc(nx / 1e7)
  area <- ifelse(area < 100, "", paste0("(", area, ") ") )
  exchange <- trunc(nx / 1e4) %% 1e3
  number <- nx %% 1e4
  paste0(area, exchange, "-", number)
}
