
#' Reproducible Examples
#'
#' Create a reprex with default figure size set and easily adjusted.
#'
#' @inheritParams reprex::reprex
#' @param ... Additional arguments passed on to [`reprex::reprex()`].
#' @param fig.width Width of figures.
#' @param fig.height Height of figures.
#' @importFrom reprex reprex
#' @importFrom utils modifyList
#' @export
#' @seealso [`reprex::reprex()`]

reprex <-
  function(
    x, ...,
    fig.width = 6, fig.height = 3,
    opts_chunk = list()) {

    reprex::reprex(
      x, ...,
      opts_chunk =
        utils::modifyList(
          list(fig.width = fig.width, fig.height = fig.height),
          opts_chunk
        )
    )
}
