
<!-- README.md is generated from README.Rmd. -->

# newspapr <img src="man/figures/logo.png" width="160px" align="right"/>

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

newspapr is an R wrapper package for [NewsAPI](https://newsapi.org/).

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

  - `/v2/top-headlines`
  - `/v2/everything`
  - `/v2/sources`

<!-- # TO DO -->

<!-- - write package to mirror exactly all capabilities of the NewsAPI -->

<!-- - create Shiny app to implement all of the functions written in GUI form -->

<!-- - implement special machine learning algorithm to detect if news headline is -->

<!-- sensationalist or not -->

<!-- - word cloud generator -->

<!-- - sentiment analysis (i.e. put in given term, be told if news are good or bad) -->

<!-- - trend plots of terms over time -->

<!-- - daily update feature (today's top news based on user preference) -->

<!-- For the Shiny client, things to include: -->

<!-- - set preferred languages -->

<!-- - choose publication -->

<!-- ## Usage -->

<!-- For how to use `newspapr`, please see the [vignette](https://#) or the [documentation](https://#). -->

<!-- ## Showcase -->

<!-- Check out a news recommendation system built using newspapr -->
