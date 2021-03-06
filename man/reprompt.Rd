% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reprompt.R
\name{reprompt}
\alias{reprompt}
\title{Re-prompt R code}
\usage{
reprompt(comment = "##", file = NULL, prompt = getOption("prompt"),
  continue = getOption("continue"))
}
\arguments{
\item{comment}{a character string to precede each line of output}

\item{file}{a file containing mixed R code and output.  If \code{NULL}, the edit buffer will
be used.}

\item{prompt, continue}{the prompt and continue prompt for R code.}
}
\description{
Remove prompts from R code and output and comment the output, much like knitr does by default
}
\details{
The value of this function is in its side effects.  The modified code
will be printed and if \code{file} is \code{NULL}, it will also be pasted into the
pasteboard buffer.
}
