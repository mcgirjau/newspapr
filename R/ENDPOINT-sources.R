#' Get index of available sources
#'
#' Returns the subset of news publishers that top headlines are available from.
#' It's mainly a convenience function that can be used to keep track of the
#' publishers available on the API.
#'
#' @param category (Optional) A string. Find sources that display news of a specific category,
#' such as "business", "entertainment", or "sports".
#' @param language (Optional) A string. Find sources that display news in a specific language.
#' Possible options are: Arabic, German, English, Spanish, French, Hebrew, Italian
#' Dutch, Norwegian, Portuguese, Russian, Swedish, Urdu, and Chinese.
#' By default, the API returns sources in all languages.
#' @param country_iso (Optional) A string. The 2-letter ISO 3166-1 code of the country you
#' want to get headlines for, such as "US" or "FR". This parameter cannot be mixed with
#' the sources parameter.
#'
#' @return A list of news publishers.
#'
#' @examples
#' get_sources(language = "de")
get_sources <- function(category = NULL, language = NULL, country_iso = NULL) {
  if (!is.null(language)) {
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
      language == "chinese" ~ "zh",
      TRUE ~ stop("Invalid language. Available languages are: Arabic, German, English, Spanish, French, Hebrew, Italian, Dutch, Norwegian, Portuguese, Russian, Swedish, Urdu, and Chinese.", call. = FALSE)
    )
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
  query_url <- .build_query_url(endpoint = "sources", category = category, language = language,
                                country = country_iso)
  sources <- httr::GET(query_url) %>%
    httr::content() %>%
    .[["sources"]]
  return(sources)
}
