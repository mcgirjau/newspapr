
<!-- README.md is generated from README.Rmd. -->

# newspapr <img src="man/figures/logo.png" title="logo created with hexSticker" width="160px" align="right"/>

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

newspapr is an R wrapper package for [NewsAPI](https://newsapi.org/).

NewsAPI provides access to live breaking news headlines and articles
from over 30,000 worldwide news sources and blogs.

## Installation

``` r
install.packages("devtools")
devtools::install_github("mcgirjau/newspapr")
library(newspapr)
```

## Authentication

To use NewsAPI and newspapr, you will need a free API key that can be
issued [here](https://newsapi.org/account). You must then register your
key within newspapr for the current session. You can do this using:

``` r
register_key(key = api_key)
```

To avoid having to run `register_key()` in every new session, you can
write the API key to your `.Renviron` file, which will then be used in
all future sessions:

``` r
register_key(key = api_key, write = TRUE)
```

You can check whether you have a registered key for the current session
using:

``` r
has_key()
```

In case of unexpected errors while using the API, check to make sure the
issue isn’t with your API key:

``` r
check_key()
```

## Endpoints

`newspapr` supports all 3 of the NewsAPI
[endpoints](https://newsapi.org/docs/endpoints):

  - `/v2/top-headlines` - “This endpoint provides live top and breaking
    headlines for a country, specific category in a country, single
    source, or multiple sources. You can also search with keywords.
    Articles are sorted by the earliest date published first.”
  - `/v2/everything` - “Search through millions of articles from over
    30,000 large and small news sources and blogs. This includes
    breaking news as well as lesser articles. This endpoint suits
    article discovery and analysis, but can be used to retrieve articles
    for display, too.”
  - `/v2/sources` - “This endpoint returns the subset of news publishers
    that top headlines (`/v2/top-headlines`) are available from. It’s
    mainly a convenience endpoint that you can use to keep track of the
    publishers available on the API.”

## Usage

For how to use newspapr, please read the
[documentation](https://github.com/mcgirjau/newspapr/blob/master/newspapr-manual.pdf).
