.has_key <- function() {
  key <- Sys.getenv("NEWS_API_KEY")
  if (any(is.null(key), identical(key, ""))) {
    return(FALSE)
  }
  return(TRUE)
}

.get_key <- function() {
  if (!.has_key()) {
    stop("Could not find NewsAPI key. Please register it using register_key().", call. = FALSE)
  } else {
    return(Sys.getenv("NEWS_API_KEY"))
  }
}

#' Register API key
#'
#' Returns the subset of news publishers that top headlines are available from.
#' It's mainly a convenience function that can be used to keep track of the
#' publishers available on the API.
#'
#' @param key A string. Your NewsAPI key.
#' @param write (Optional) A boolean. Specifies whether you want your API key to
#' be written to your .Renviron file for use in all future R sessions. Defaults to FALSE.
#'
#' @importFrom stringr str_detect str_extract
register_key <- function(key, write = FALSE) {
  if (missing(key)) {
    stop("Please enter your API key.", call. = FALSE)
  } else {
    key <- deparse(substitute(key))
  }
  if (write) {
    environ_file <- file.path(Sys.getenv("HOME"), ".Renviron")
    if (!file.exists(file.path(Sys.getenv("HOME"), ".Renviron"))) {
      message(paste("Creating file", environ_file))
      file.create(environ_file)
    }
    environ_lines <- readLines(environ_file)
    if (!any(str_detect(environ_lines, "NEWS_API_KEY="))) {
      message(paste("Adding key to ", environ_file))
      environ_lines <- c(environ_lines, paste0("NEWS_API_KEY=", key))
      writeLines(environ_lines, environ_file)
    } else {
      key_line_index <- which(str_detect(environ_lines, "NEWS_API_KEY="))
      old_key <- str_extract(environ_lines[key_line_index], "(?<=NEWS_API_KEY=)\\w+")
      message(paste("Replacing old key", old_key, "with new key in", environ_file))
      environ_lines[key_line_index] <- paste0("NEWS_API_KEY=", key)
      writeLines(environ_lines, environ_file)
    }
    Sys.setenv(NEWS_API_KEY = key)
  } else {
    Sys.setenv(NEWS_API_KEY = key)
  }
}

#' Display API key
#'
#' Displays the API key that has been registered.
show_key <- function() {
  paste("Your NewsAPI key is:", .get_key())
}

#' Check whether an API key has been registered
#'
#' Checks whether an API key has been registered within the current R session,
#' either manually by using register_key(), or automatically from the .Renviron
#' file.
#'
#' @return A boolean indicating whether an API key has been registered within the
#' current R session.
has_key <- function() {
  return(.has_key())
}

#' Check whether API key is valid
#'
#' Checks whether an API key is valid. Prints a message to let the user know whether
#' their key is usable with NewsAPI (case in which the function prints "All OK"),
#' or whether an error occurred (case in which the function prints a descriptive
#' message of said error).
#'
#' @param api_key (Optional) A string. Your NewsAPI key.
#'
#' @importFrom httr content status_code
#' @importFrom dplyr case_when
check_key <- function(key = NULL) {
  if (missing(key) && !.has_key()) {
    stop("No API key found. Please register it using register_key() or pass it as an argument to check_key().", call. = FALSE)
  } else if (missing(key)) {
    key <- .get_key()
  }
  query <- paste0("https://newsapi.org/v2/top-headlines?country=us&apiKey=", key)
  status_code <- httr::status_code(httr::GET(query))
  if (status_code == 200) {
    message("All OK.")
  } else {
    error <- httr::content(httr::GET(query))[["code"]]
    message <- dplyr::case_when(
      error == "apiKeyInvalid" ~ "NewsAPI.org says: Your API key hasn't been entered correctly. Double check it and try again.",
      error == "apiKeyDisabled" ~ "NewsAPI.org says: Your API key has been disabled.",
      error == "apiKeyExhausted" ~ "NewsAPI.org says: Your API key has no more requests available.",
      error == "rateLimited" ~ "NewsAPI.org says: You have been rate limited. Back off for a while before trying the request again.",
      error == "unexpectedError" ~ "NewsAPI.org says: This shouldn't happen, and if it does then it's our fault, not yours. Try the request again shortly."
      )
    message(message)
  }
}