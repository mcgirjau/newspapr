% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/authentication.R
\name{register_key}
\alias{register_key}
\title{Register API key}
\usage{
register_key(key, write = FALSE)
}
\arguments{
\item{key}{A string. Your NewsAPI key.}

\item{write}{(Optional) A boolean. Specifies whether you want your API key to
be written to your .Renviron file for use in all future R sessions. Defaults to FALSE.}
}
\description{
Returns the subset of news publishers that top headlines are available from.
It's mainly a convenience function that can be used to keep track of the
publishers available on the API.
}
