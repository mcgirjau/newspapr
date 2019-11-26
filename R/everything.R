get_everything <- function(keyword = NULL, keyword_in_title = NULL, exact_match = NULL, must_appear = NULL, must_not_appear = NULL, sources = NULL, domains = NULL, exclude_domains = NULL, from = NULL, to = NULL, language = NULL, sort_by = NULL, page_size = NULL, page = NULL) {
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
      stop("Invalid country parameter. Parameter country_iso must be a 2-letter ISO 3166-1 code, e.g. \"us\" or \"fr\"", call. = FALSE)
    } else if (!(country_iso %in% .iso_codes)) {
      stop(paste(country_iso, "is not a valid ISO 3166-1 code. Please try again"), call. = FALSE)
    }
  }
  if (!is.null(page_size)) {
    if (is.logical(page_size) || is.na(as.numeric(page_size)) || as.numeric(page_size) < 0 ||
        !is.integer(as.numeric(page_size))) {
      stop("Invalid page size parameter. Parameter page_size must be a whole, positive number, e.g. 5. Please try again", call. = FALSE)
    }
  }
  if (!is.null(page)) {
    if (is.logical(page) || is.na(as.numeric(page)) || as.numeric(page) < 0 ||
        !is.integer(as.numeric(page))) {
      stop("Invalid page parameter. Parameter page must be a whole, positive number, e.g. 5. Please try again", call. = FALSE)
    }
  }
  if (!is.null(sources)) {
    # check that source is valid using list of sources from API endpoint
  }
  query_url <- .build_query_url(endpoint = "top-headlines", q = keyword, country = country_iso,
                                category = category, sources = sources, pageSize = page_size, page = page)
  articles <- httr::GET(query_url) %>%
    httr::content() %>%
    .[["articles"]]
  return(articles)
}


