% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compiler-TransformRecipe.R
\name{prepare_transformation_recipe}
\alias{prepare_transformation_recipe}
\title{Create the Transformation Recipe for an einops call}
\usage{
prepare_transformation_recipe(
  expr,
  func,
  axes_names,
  ndim,
  reverse_groups = FALSE
)
}
\arguments{
\item{expr}{The input einops expression string}

\item{func}{The string/function indicating the reduction operation}

\item{axes_names}{user defined axis names as a \code{\link[=character]{character()}} vector.}

\item{ndim}{count for the number of dimensions of the input tensor}

\item{reverse_groups}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} logical: whether
to reverse the order of the axes in each group.}
}
\value{
a populated \code{\link[=TransformRecipe]{TransformRecipe()}} object
}
\description{
This function does the following parts of the einops 'compilation' pipeline:
\enumerate{
\item Lexing: tokenizing the input expression string
\item Parsing: converting the tokens into an Abstract Syntax Tree (AST)
\item Syntactic Analysis:
\itemize{
\item operation-based AST validation pass
\item Compile syntactic info for intermediate representation (IR).
}
\item IR generation: return the \code{\link[=TransformRecipe]{TransformRecipe()}} object.
}
}
\keyword{internal}
