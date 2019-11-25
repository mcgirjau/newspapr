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
    code <- httr::content(request)[["code"]]

    error <- dplyr::case_when(

      code == "apiKeyInvalid" ~ paste("NewsAPI.org says: Your API key hasn't been",
                                       "entered correctly. Double check it and try again."),

      code == "apiKeyDisabled" ~ "NewsAPI.org says: Your API key has been disabled.",

      code == "apiKeyExhausted" ~ "NewsAPI.org says: Your API key has no more requests available.",

      code == "rateLimited" ~ paste("NewsAPI.org says: You have been rate limited.",
                                     "Back off for a while before trying the request again."),

      code == "unexpectedError" ~ paste("NewsAPI.org says: This shouldn't happen,",
                                         "and if it does then it's our fault, not yours.",
                                         "Try the request again shortly.")
    )

    stop(error, call. = FALSE)
  }
}

# -----------------------------------------------------------------------------
# ARGUMENT CHECKERS - check that the arguments provided by the user are
#                     compatible with NewsAPI
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

  } else if (!(country %in% countries)) {

    error <- paste("Invalid ISO code.", country, "is either not a valid ISO 3166-1",
                   "code, or it is not currently supported. For a full list of the",
                   "countries available through NewsAPI, see https://newsapi.org/docs/endpoints/sources.")
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

