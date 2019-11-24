.build_query_url <- function(endpoint, ...) {
  query <- list(apiKey = .get_key(), ...)
  query <- Filter(Negate(is.null), query)
  raw_url <- structure(list(scheme = "https",
                            hostname = "newsapi.org/v2",
                            path = endpoint,
                            query = query),
                       class = "url")
  query_url <- httr::build_url(raw_url)
  return(query_url)
}
