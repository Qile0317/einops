% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compiler-backends.R
\name{get_backend}
\alias{get_backend}
\title{Main function to detect and return backend}
\usage{
get_backend(tensor)
}
\arguments{
\item{tensor}{any supported tensor-like object}
}
\value{
An instance of a \code{\link[=EinopsBackend]{EinopsBackend()}} class. Every returned object
is a singleton, so the same object will be returned for the same tensor type.
}
\description{
Main function to detect and return backend
}
\keyword{internal}
