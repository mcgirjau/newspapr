# -----------------------------------------------------------------------------
# QUERY BUILDER - builds query URL for the GET request
# -----------------------------------------------------------------------------

.build_query_url <- function(endpoint, ...) {

  query <- list(apiKey = .get_key(), ...)
  query <- Filter(Negate(is.null), query)

  raw_url <- structure(list(scheme = "https",
                            hostname = "newsapi.org/v2",
                            path = endpoint,
                            query = query),
                       class = "url")

  # performs URL encoding automatically
  query_url <- httr::build_url(raw_url)
  return(query_url)
}

# -----------------------------------------------------------------------------
# INTERNET CHECKER - checks that there's an internet connection
# -----------------------------------------------------------------------------

.check_internet <- function() {
  if (!curl::has_internet()) {
    stop("Please check your internet connection and try again.")
  }
}

# -----------------------------------------------------------------------------
# REQUEST CHECKER - checks that the GET request has been completed successfully
# -----------------------------------------------------------------------------

.check_request <- function(request) {

  status_code <- httr::status_code(request)

  if (status_code == 200) {
    return()
  } else {
    message <- httr::content(request)[["message"]]
    error <- paste("NewsAPI says:", message)
    stop(error, call. = FALSE)
  }
}

# -----------------------------------------------------------------------------
# ARGUMENT CHECKERS - check that the arguments provided by the user are
#                     compatible with NewsAPI
# -----------------------------------------------------------------------------

.has_required_parameters <- function(keyword = NULL, title_keyword = NULL,
                                     country = NULL, category = NULL,
                                     sources = NULL, domains = NULL) {

  args <- list(keyword, title_keyword, country, category, sources, domains)

  if (all(sapply(args, is.null))) {
    error <- paste("Missing required parameters. Please set any of the following",
                   "parameters and try again: keyword, sources, country, category",
                   "(for top headlines) or: keyword, title_keyword, sources,",
                   "domains (for everything).")
    stop(error, call. = FALSE)
  }
}

# -----------------------------------------------------------------------------

.has_compatible_parameters <- function(country, category, sources) {
  if (!is.null(sources) && (!is.null(country) || !is.null(category))) {
    error <- paste("Incompatible parameters. The sources parameter cannot be mixed",
                   "with country or category.")
    stop(error, call. = FALSE)
  }
}

# -----------------------------------------------------------------------------

.check_category <- function(category) {

  categories <- c("business", "entertainment", "general", "health", "science",
                   "sports", "technology")

  if (!(tolower(category) %in% categories)) {
    error <- paste("Invalid category. Must be one of: business, entertainment,",
                   "general, health, science, sports, technology.")
    stop(error, call. = FALSE)
  }
}

# -----------------------------------------------------------------------------

.check_language <- function(language) {

  languages <- c("arabic", "ar", "german", "de", "english", "en", "spanish", "es",
                 "french", "fr", "hebrew", "he", "italian", "it", "dutch", "nl",
                 "norwegian", "no", "portuguese", "pt", "russian", "ru", "swedish",
                 "se", "urdu", "ud", "chinese", "zh")

  if (!(tolower(language) %in% languages)) {
    error <- paste("Invalid language. Available languages are: Arabic, German,",
                   "English, Spanish, French, Hebrew, Italian, Dutch, Norwegian,",
                   "Portuguese, Russian, Swedish, Urdu, and Chinese.")
    stop(error, call. = FALSE)
  }
}

# -----------------------------------------------------------------------------

.check_country <- function(country) {

  countries <- c("ae", "ar", "at", "au", "be", "bg", "br", "ca", "ch", "cn",
                 "co", "cu", "cz", "de", "eg", "fr", "gb", "gr", "hk", "hu",
                 "id", "ie", "il", "in", "it", "jp", "kr", "lt", "lv", "ma",
                 "mx", "my", "ng", "nl", "no", "nz", "ph", "pl", "pt", "ro",
                 "rs", "ru", "sa", "se", "sg", "si", "sk", "th", "tr", "tw",
                 "ua", "us", "ve", "za")

  if (!grepl("^.{2}$", country)) {

    error <- paste("Invalid country parameter. Must be a 2-letter ISO 3166-1 code",
                   "e.g. US or FR. For a full list of the countries available",
                   "through the NewsAPI, see https://newsapi.org/docs/endpoints/sources.")
    stop(error, call. = FALSE)

  } else if (!(tolower(country) %in% countries)) {

    error <- paste("Invalid ISO code.", country, "is either not a valid ISO 3166-1",
                   "code, or it is not currently supported. For a full list of the",
                   "countries available through NewsAPI, see https://newsapi.org/docs/endpoints/sources.")
    stop(error, call. = FALSE)
  }
}

# -----------------------------------------------------------------------------

.check_sources <- function(sources) {

  sources_name <- get_sources()$name
  sources_id <- get_sources()$id
  sources_url <- get_sources()$url

  for (source in sources) {
    if (!(source %in% sources_name || source %in% sources_id || source %in% sources_url)) {
      error <- paste("Invalid source,", source, "- please use get_sources() to",
                     "see available sources for NewsAPI queries. You can enter",
                     "name, ID, or URL. Please note that the set of sources",
                     "that can be used in a query is only a small subset of all",
                     "available sources that return articles.")
      stop(error, call. = FALSE)
    }
  }
}

# -----------------------------------------------------------------------------

#' @importFrom parsedate parse_date format_iso_8601
.check_date <- function(date) {
  date <- parsedate::parse_date(date) %>%
    parsedate::format_iso_8601()
  if (is.na(date)) {
    error <- paste("Invalid date(s) for parameters to and/or from. Please enter a",
                  "valid date, preferably in ISO 8601 format, e.g. 2019-01-01T17:15:00.")
    stop(error, call. = FALSE)
  }
}

# -----------------------------------------------------------------------------

.check_sort <- function(string) {
  options <- c("relevancy", "popularity", "published at")

}

# -----------------------------------------------------------------------------

.check_page_size <- function(page_size) {
  if (is.logical(page_size) || is.na(as.numeric(page_size)) ||
      as.numeric(page_size) < 0 || page_size %% 1 != 0 || page_size > 100) {
    error <- paste("Invalid page size parameter. Must be a whole number",
                   "between 0 and 100, e.g. 25.")
    stop(error, call. = FALSE)
  }
}

# -----------------------------------------------------------------------------

.check_page <- function(page) {
  if (is.logical(page) || is.na(as.numeric(page)) || as.numeric(page) < 0 ||
      page %% 1 != 0) {
    error <- paste("Invalid page parameter. Must be a whole, positive number",
                   "e.g. 2.")
    stop(error, call. = FALSE)
  }
}

# -----------------------------------------------------------------------------
# ARGUMENT CONVERTERS - convert the more flexible R format into one ready for
#                       NewsAPI queries
# -----------------------------------------------------------------------------

.language_to_iso <- function(language) {

  language <- tolower(language)

  language <- case_when(
    language == "arabic" ~ "ar",
    language == "german" ~ "de",
    language == "english" ~ "en",
    language == "spanish" ~ "es",
    language == "french" ~ "fr",
    language == "hebrew" ~ "he",
    language == "italian" ~ "it",
    language == "dutch" ~ "nl",
    language == "norwegian" ~ "no",
    language == "portuguese" ~ "pt",
    language == "russian" ~ "ru",
    language == "swedish" ~ "se",
    language == "urdu" ~ "ud",
    language == "chinese" ~ "zh"
  )

  return(language)
}

# -----------------------------------------------------------------------------

.country_to_iso <- function(country) {
  # add functionality here
}

# -----------------------------------------------------------------------------

.sources_to_csv <- function(sources) {

  targets <- character(0)

  for (source in sources) {
    id <- subset(get_sources(), id == source | name == source | url == source)$id
    targets <- append(targets, id)
  }

  # removing leading comma
  csv <- paste(targets, collapse = ",")
  return(csv)
}

# -----------------------------------------------------------------------------

.domains_to_csv <- function(domains) {
  csv <- paste(domains, collapse = ",")
  return(csv)
}

# -----------------------------------------------------------------------------

#' @importFrom parsedate parse_date format_iso_8601
.date_to_iso <- function(date) {
  date <- parsedate::parse_date(date) %>%
    parsedate::format_iso_8601()
  return(date)
}

# -----------------------------------------------------------------------------
# ARTICLE EXTRACTOR - gets articles from HTTP request, removes NULL values, and
#                     converts to data frame
# -----------------------------------------------------------------------------

#' @importFrom dplyr "%>%"
#' @importFrom tibble as_tibble
#' @importFrom janitor clean_names
.extract_articles <- function(request) {

  article_list <- request %>%
    httr::content() %>%
    .[["articles"]]

  # remove nested sublists with repetitive source information
  article_list <- lapply(article_list, function(x) {
    x[[1]] <- x$source$name
    return(x)
  })

  # remove NULL values from fields
  article_list <- lapply(article_list, function(x) {
    x <- lapply(x, function(element) {
      if (is.null(element)) {
        element <- NA
      }
      return(element)
    })
    return(x)
  })

  # convert to tibble
  articles <- do.call(rbind.data.frame, c(article_list, stringsAsFactors = FALSE)) %>%
    tibble::as_tibble() %>%
    janitor::clean_names()
  return(articles)
}
