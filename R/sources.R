#' Get index of available sources
#'
#' Returns the subset of news publishers that top headlines are available from.
#' It's mainly a convenience function that can be used to keep track of the
#' publishers available on the API.
#'
#' @param category (Optional) A string. Find sources that display news of a
#' specific category. Possible options are: "business", "entertainment",
#' "general", "health", "science", "sports", and "technology". By default, the
#' API returns sources of all categories.
#' @param language (Optional) A string. Find sources that display news in a specific language.
#' Possible options are: Arabic, German, English, Spanish, French, Hebrew, Italian
#' Dutch, Norwegian, Portuguese, Russian, Swedish, Urdu, and Chinese. The language
#' cam also be specified using ISO codes, e.g. "DE" for German or "ES" for Spanish.
#' By default, the API returns sources in all languages.
#' @param country (Optional) A string. The 2-letter ISO 3166-1 code of the country you
#' want to get headlines for, such as "US" or "FR". This parameter cannot be mixed with
#' the sources parameter.
#'
#' @return A data frame of news publishers, with their ID as it should be queried
#' in a NewsAPI request, a short description, their URL, their category, their
#' language, and their country.
#'
#' @examples
#' get_sources(language = "German")
#'
#' @importFrom dplyr "%>%"
#'
#' @export
get_sources <- function(category = NULL, language = NULL, country = NULL) {

  if (!is.null(category)) {
    .check_category(category)
  }

  if (!is.null(language)) {
    .check_language(language)
    # change language to NewsAPI-compatible ISO code if not already so
    if (!grepl("^.{2}$", language)) {
      language <- .language_to_iso(language)
    }
  }

  if (!is.null(country)) {
    .check_country_iso(country)
    # change country to NewsAPI-compatible ISO code if not already so
    if (!grepl("^.{2}$", country)) {
      country <- .country_to_iso(country)
    }
  }

  query_url <- .build_query_url(endpoint = "sources", category = category,
                                language = language, country = country)

  .check_internet()
  request <- httr::GET(query_url)
  .check_request(request)

  sources <- request %>%
    httr::content() %>%
    .[["sources"]] %>%
    {do.call(rbind.data.frame, .)}

  return(sources)

}
