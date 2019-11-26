#' Get top headlines articles
#'
#' Returns a list of current top headlines articles based on some query. At least
#' one parameter from among keyword, country_iso, category, and sources must be
#' specified, with the limitation that the sources parameter cannot be mixed with
#' country_iso or category.
#'
#' @param keyword (Optional) A string. Keyword(s) based on which to search for articles,
#' such as "Hong Kong protests" or "Amazon forest".
#' @param country (Optional) A string. The 2-letter ISO 3166-1 code of the country you
#' want to get headlines for, such as "US" or "FR". This parameter cannot be mixed with
#' the sources parameter.
#' @param category (Optional) A string. The category you want to get top headlines for, such
#' as "business", "entertainment", or "sports". This parameter cannot be mixed with
#' the sources parameter.
#' @param sources (Optional) A character vector. The news sources or blogs you want top
#' headlines from. See the get_sources() function for how to obtain a full list
#' of the available sources. This parameter cannot be mixed with the country_iso
#' or category parameters.
#' @param page_size (Optional) An integer. The number of results to return per request.
#' The API's default is 20, and the maximum is 100.
#' @param page (Optional) An integer. Use this to page through the results if the total
#' results found is greater than the page size.
#'
#' @return A tibble of article information, with source, author, title,
#' description, URL, publication date, and a short preview of the content.
#'
#' @examples
#' get_top_headlines(keyword = "Panama Canal", page_size = 10, page = 2)
#' get_top_headlines(country = "br", category = "sports")
#' get_top_headlines(keyword = "iPhone 10", source = "techcrunch")
#'
#' @export
get_top_headlines <- function(keyword = NULL, country = NULL, category = NULL,
                              sources = NULL, page_size = NULL, page = NULL) {

  .has_required_arguments(keyword, country, category, sources)

  .has_compatible_parameters(country, category, sources)

  if (!is.null(country)) {
    .check_country(country)
    # change country to NewsAPI-compatible ISO code if not already so
    if (!grepl("^.{2}$", country)) {
      country <- .country_to_iso(country)
    }
  }

  if (!is.null(category)) {
    .check_category(category)
  }

  if (!is.null(sources)) {
    .check_sources(sources)
    sources <- .sources_to_csv(sources)
  }

  if (!is.null(page_size)) {
    .check_page_size(page_size)
  }

  if (!is.null(page)) {
    .check_page(page)
  }

  query_url <- .build_query_url(endpoint = "top-headlines", q = keyword, country = country,
                                category = category, sources = sources,
                                pageSize = page_size, page = page)

  .check_internet()
  request <- httr::GET(query_url)
  .check_request(request)

  articles <- .extract_articles(request)

  return(articles)
}
