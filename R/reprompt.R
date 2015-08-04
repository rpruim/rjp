#' Re-prompt R code
#'
#' Remove prompts from R code and output and comment the output, much like knitr does by default
#'
#' @param comment a character string to precede each line of output
#' @param file a file containing mixed R code and output.  If \code{NULL}, the edit buffer will
#' be used.
#' @param prompt,continue the prompt and continue prompt for R code.
reprompt <-
  function(
    comment = "##",
    file=NULL,
    prompt = getOption("prompt"),
    continue = getOption("continue")
  ) {
    if (is.null(file)) {
      # to_edit <- suppressWarnings(readLines("clipboard"))
      to_edit <- readLines(pipe("pbpaste|awk 1"))  # awk 1 handles last line without EOL
    } else {
      to_edit <- readLines(file)
    }

    cmdPrompts <- paste("^", prompt, "|^", continue, sep="")
    cmdPrompts <- gsub("\\+", "\\\\+", cmdPrompts)
    cmdPrompts <- gsub("\\.", "\\\\.", cmdPrompts)

    id_commands <- grep(cmdPrompts, to_edit) # which are command or continuation lines
    to_edit[id_commands] <- sub(cmdPrompts, "", to_edit[id_commands]) # remove prompts
    to_edit[-id_commands] <- paste(comment, to_edit[-id_commands]) # comment output
    writeLines(to_edit)
  }
