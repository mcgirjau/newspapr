#' Get all articles
#'
#' Search through millions of articles from over 30,000 large and small news
#' sources and blogs. This includes breaking news as well as lesser articles.
#' At least one parameter from among keyword, title_keyword, sources, and
#' domains must be specified, otherwise the search is too broad.
#'
#' @param keyword (Optional) A string. Keyword(s) based on
#'     which to search for articles, such as "Hong Kong protests" or "Amazon forest".
#'     Keywords may appear either in the title or the body of the article. Advanced
#'     search is supported here - surround phrases with quotes (") for exact match,
#'     prepend words or phrases that must appear with a + symbol (e.g. +bitcoin),
#'     prepend words that must not appear with a - symbol (e.g. -bitcoin).
#'     Alternatively, you can use the AND / OR / NOT keywords, and optionally
#'     group these with parenthesis. Eg: "crypto AND (ethereum OR litecoin) NOT bitcoin".
#' @param title_keyword (Optional) A string or character vector. Keywords or phrases
#'     to search for in the article title only. Advanced search is supported here
#'     (see parameter "keyword") for details.
#' @param sources (Optional) A character vector. The news sources or blogs you want top
#'     headlines from. See the get_sources() function for how to obtain a full list
#'     of the available sources.
#' @param domains (Optional) A character vector. The set of domains (eg bbc.co.uk,
#'     techcrunch.com, engadget.com) to restrict the search to.
#' @param exclude_domains (Optional) A character vector. The set of domains
#'     (eg bbc.co.uk, techcrunch.com, engadget.com) to remove from the results.
#' @param from (Optional) A string. A date and optional time for the oldest
#'     article allowed. This should be in ISO 8601 format (e.g. "2019-11-25" or
#'     "2019-11-25T23:45:05"). Default: the oldest according to your plan.
#' @param to (Optional) A string. A date and optional time for the newest
#'     article allowed. This should be in ISO 8601 format (e.g. "2019-11-25" or
#'     "2019-11-25T23:45:05"). Default: the newest according to your plan.
#' @param language (Optional) A string. Find sources that display news in a s
#'     pecific language. Possible options are: Arabic, German, English, Spanish,
#'     French, Hebrew, Italian, Dutch, Norwegian, Portuguese, Russian, Swedish,
#'     Urdu, and Chinese. The language can also be specified using ISO codes,
#'     e.g. "DE" for German or "ES" for Spanish. By default, the API returns
#'     sources in all languages.
#' @param sort_by (Optional) A string. The order to sort the articles in.
#'     Possible options: "relevancy", "popularity", "published at". Defaults to
#'     "published at".
#' @param page_size (Optional) An integer. The number of results to return per request.
#'     The API's default is 20, and the maximum is 100.
#' @param page (Optional) An integer. Use this to page through the results if the total
#'     results found is greater than the page size.
#'
#' @return A tibble of article information, with source, author, title,
#' description, URL, publication date, and a short preview of the content.
#'
#' @examples
#' get_everything(keyword = "Panama Canal", page_size = 10, page = 2)
#' get_everything(country = "br", category = "sports")
#' get_everything(keyword = "iPhone 10", source = "techcrunch")
#'
#' @export
get_everything <- function(keyword = NULL, title_keyword = NULL, sources = NULL,
                              domains = NULL, exclude_domains = NULL, from = NULL,
                              to = NULL, language = NULL, sort_by = "published at",
                              page_size = NULL, page = NULL) {

  .has_required_parameters(keyword = keyword, title_keyword = title_keyword,
                           sources = sources, domains = domains)

  if (!is.null(sources)) {
    .check_sources(sources)
    sources <- .sources_to_csv(sources)
  }

  if (!is.null(domains)) {
    domains <- .domains_to_csv(domains)
  }

  if (!is.null(exclude_domains)) {
    exclude_domains <- .domains_to_csv(exclude_domains)
  }

  if (!is.null(from)) {
    .check_date(from)
    from <- .date_to_iso(from)
  }

  if (!is.null(to)) {
    .check_date(to)
    to <- .date_to_iso(to)
  }

  if (!is.null(language)) {
    .check_language(language)
    # change language to NewsAPI-compatible ISO code if not already so
    if (!grepl("^.{2}$", language)) {
      language <- .language_to_iso(language)
    }
  }

  if (!is.null(sort_by)) {
    .check_sort(sort_by)
    if (sort_by == "published at") {
      # published at is the default so it's redundant in the query
      sort_by = NULL
    }
  }

  if (!is.null(page_size)) {
    .check_page_size(page_size)
  }

  if (!is.null(page)) {
    .check_page(page)
  }

  # keywords must be URL-encoded
  if (!is.null(keyword)) {
    keyword <- URLencode(keyword)
  }
  if (!is.null(title_keyword)) {
    title_keyword <- URLencode(title_keyword)
  }

  query_url <- .build_query_url(endpoint = "everything", q = keyword,
                                qInTitle = title_keyword, sources = sources,
                                domains = domains, excludeDomains = exclude_domains,
                                from = from, to = to, language = language,
                                sortBy = sort_by, pageSize = page_size, page = page)

  .check_internet()
  request <- httr::GET(query_url)
  .check_request(request)

  articles <- .extract_articles(request)

  return(articles)
}
