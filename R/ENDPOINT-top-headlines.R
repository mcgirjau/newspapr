#' Get top headlines articles
#'
#' Returns a list of current top headlines articles based on some query. At least
#' one parameter from among keyword, country_iso, category, and sources must be
#' specified, with the limitation that the sources parameter cannot be mixed with
#' country_iso or category.
#'
#' @param keyword (Optional) A string. Keyword(s) based on which to search for articles,
#' such as "Hong Kong protests" or "Amazon forest".
#' @param country_iso (Optional) A string. The 2-letter ISO 3166-1 code of the country you
#' want to get headlines for, such as "US" or "FR". This parameter cannot be mixed with
#' the sources parameter.
#' @param category (Optional) A string. The category you want to get top headlines for, such
#' as "business", "entertainment", or "sports". This parameter cannot be mixed with
#' the sources parameter.
#' @param sources (Optional) A character vector. The news sources or blogs you want top
#' headlines from. The get_sources() function can be used to obtain a full
#' list of the available sources. This parameter cannot be mixed with the country_iso
#' or category parameters.
#' @param page_size (Optional) An integer. The number of results to return per request.
#' The API's default is 20, and the maximum is 100.
#' @param page (Optional) An integer. Use this to page through the results if the total
#' results found is greater than the page size.
#'
#' @return A nested list of articles, including source, author, title, description,
#' URL, publication date, and a short preview of the content.
#'
#' @examples
#' get_top_headlines(keyword = "Panama Canal", page_size = 10, page = 2)
#' get_top_headlines(country_iso = "br", category = "sport")
#' get_top_headlines(keyword = "iPhone 10", source = "techcrunch")
#'
#' @export
get_top_headlines <- function(keyword = NULL, country_iso = NULL, category = NULL, sources = NULL, page_size = NULL, page = NULL) {
  if (all(is.null(keyword), is.null(country_iso), is.null(category), is.null(sources))) {
    stop("Missing required parameters. Please set any of the following parameters and try again: keyword, sources, country, category", call. = FALSE)
  }
  if (!is.null(sources) && (!is.null(country_iso) || !is.null(category))) {
    stop("Incompatible parameters. The sources parameter cannot be mixed with parameters country_iso or category.", call. = FALSE)
  }
  if (!is.null(country_iso)) {
    country_iso <- tolower(country_iso)
    .iso_codes <- c("ae", "ar", "at", "au", "be", "bg", "br", "ca", "ch", "cn",
                    "co", "cu", "cz", "de", "eg", "fr", "gb", "gr", "hk", "hu",
                    "id", "ie", "il", "in", "it", "jp", "kr", "lt", "lv", "ma",
                    "mx", "my", "ng", "nl", "no", "nz", "ph", "pl", "pt", "ro",
                    "rs", "ru", "sa", "se", "sg", "si", "sk", "th", "tr", "tw",
                    "ua", "us", "ve", "za")
    if (length(country_iso) != 2) {
      stop("Invalid country parameter. Parameter country_iso must be a 2-letter ISO 3166-1 code, e.g. \"US\" or \"FR\"", call. = FALSE)
    } else if (!(country_iso %in% .iso_codes)) {
      stop(paste(country_iso, "is not a valid ISO 3166-1 code. Please try again"), call. = FALSE)
    }
  }
  if (!is.null(page_size)) {
    if (is.logical(page_size) || is.na(as.numeric(page_size)) || as.numeric(page_size) < 0 ||
        !is.integer(as.numeric(page_size))) {
      stop("Invalid page size parameter. Parameter page_size must be a whole, positive number, e.g. 5. Please try again", call. = FALSE)
    }
    if (page_size > 100) {
      stop("Invalid page size parameter. Maximum page size is 100.", call. = FALSE)
    }
  }
  if (!is.null(page)) {
    if (is.logical(page) || is.na(as.numeric(page)) || as.numeric(page) < 0 ||
        !is.integer(as.numeric(page))) {
      stop("Invalid page parameter. Parameter page must be a whole, positive number, e.g. 5. Please try again", call. = FALSE)
    }
  }
  if (!is.null(sources)){
    # check that source is valid using list of sources from API endpoint
    # modify character vector so that its valid csv
  }
  query_url <- .build_query_url(endpoint = "top-headlines", q = keyword, country = country_iso,
                                category = category, sources = sources, pageSize = page_size, page = page)
  # add print function that specifies how many articles were found e.g. foudn 458 articles, returning 1-10
  articles <- httr::GET(query_url) %>%
    httr::content() %>%
    .[["articles"]]
  return(articles)
}


