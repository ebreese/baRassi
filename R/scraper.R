usethis::use_package("httr")
usethis::use_package("jsonlite")
usethis::use_package("dplyr")
usethis::use_package("lubridate")
usethis::use_package("rlang")




get_token <- function() {
  response <- httr::POST("https://api.afl.com.au/cfs/afl/WMCTok")
  httr::stop_for_status(response)
  httr::content(response)$token
}

access_api <- function(url, token = NULL) {

  if (is.null(token)) token <- get_token()

  make_request <- function(tok) {
    httr::GET(url, httr::add_headers("x-media-mis-token" = tok))
  }

  response <- make_request(token)

  # Token expired/invalid -> get a new one and retry once
  if (httr::status_code(response) %in% c(401, 403)) {
    token <- get_token()
    response <- make_request(token)
  }

  httr::stop_for_status(response)

  content <- response |>
    httr::content(as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON(flatten = TRUE)

  attr(content, "token") <- token
  content
}
