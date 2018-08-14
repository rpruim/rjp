#' Extract phone numbers
#'
#' Extract phone numbers
#'
#' @importFrom tidyr extract_numeric
#' @param x A string or number to extract phone number from.
#' @param area An area code
#' @param na.return Value to return when no telephone number is found.
#' @export
#' @examples
#' p <- c(NA, "1234567", "1234567890", "(123) 456-7890", "123.456.7890", 12345678900)
#' extract_phone(p)
#' extract_phone(p, area=555)
#' extract_phone(p, area=555, na.return = "no phone number supplied")
#'
extract_phone <- function(x, area = NULL, na.return = NA) {
  cx <- gsub("[^0123456789]", "", as.character(x))
  nx <- readr::parse_number(cx)
  if (!is.null(area)) {
    readr::parse_number(area)
  } else {
    area <- 0
  }
  nx <-
    ifelse(
      nx < 1e7 & !is.null(area),
      readr::parse_number(area) * 1e7 + nx,
      nx)

  nx <- ifelse( nx >= 1e10 | nx <= 99999, NA, nx)

  area <- trunc(nx / 1e7)
  area <- ifelse(area < 100, "", paste0("(", area, ") ") )
  exchange <- trunc(nx / 1e4) %% 1e3
  number <- nx %% 1e4
  ifelse( is.na(nx), na.return, paste0(area, exchange, "-", number))
}
