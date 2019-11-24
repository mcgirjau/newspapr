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

show_key <- function() {
  paste("Your NewsAPI key is:", .get_key())
}

has_key <- function() {
  return(.has_key())
}

check_key <- function(key = NULL) {
  if (missing(key) && !.has_key()) {
    stop("No API key found. Please register it using register_key() or pass it as an argument to check_key().", call. = FALSE)
  } else if (missing(key)) {
    key <- .get_key()
  }
  query <- paste0("https://newsapi.org/v2/top-headlines?country=us&apiKey=", key)
  status_code <- httr::status_code(httr::GET(query))
  case_when(
    status_code == 200 ~ "OK",
    status_code == 401 ~ "Unauthorized",
    status_code == 429 ~ "Too Many Requests",
    status_code == 429 ~ "Server Error",
  )
}
