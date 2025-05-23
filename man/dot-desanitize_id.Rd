% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.desanitize_id}
\alias{.desanitize_id}
\title{Desanitize a Filesystem Identifier}
\usage{
.desanitize_id(sanitized_id)
}
\arguments{
\item{sanitized_id}{Character string identifier sanitized by \code{.sanitize_id()}.}
}
\value{
The original identifier string.
}
\description{
Reverses the replacements made by \code{.sanitize_id()}.
}
\keyword{internal}
