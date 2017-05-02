`%+%` <- function(x, y) paste0(x, y)

#' Get Google Pagespeed Results
#'
#' @param url URL to get score for
#' @param api_key API key from Google
#' @param strategy The analysis strategy to use, either 'desktop' or 'mobile' (optional)
#' @return An object of class 'pagespeed_api'
#' @export
#'
#' @examples\dontrun{
#' get_pagespeed_api("http://finovate.com/blog/", "1234")
#' }
get_pagespeed_api <- function(url, api_key, strategy = NULL){

  # If the protocol is missing add http as protocol
  if (!grepl('(?i)http(s)?://.*', url)){
    url <- paste0('http://', url)
  }

  get_url <-
    'https://www.googleapis.com/pagespeedonline/v2/runPagespeed'

  if (!is.null(strategy)){
    if (!(strategy %in% c('mobile', 'desktop'))) stop("Stategy must be 'desktop', 'mobile' or NULL")
  }

  parameters <- list(
    'key' = api_key,
    'url' = url,
    'strategy' = strategy
  )

  parameters <- purrr::discard(parameters, is.null)

  response <- httr::GET(get_url, query = parameters)

  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  httr::stop_for_status(response)

  content <- httr::content(response, type = 'text', encoding = 'utf-8')
  parsed  <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  structure(
    list(
      content     = parsed,
      response    = response,
      raw_content = content
    ),
    class = "pagespeed_api"
  )
}

#' Convert a Google Pagespeed Object to a dataframe
#'
#' @param pagespeed_api A object of class 'pagespeed_api';
#' normally returned from `get_pagespeed_api`.
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' @examples \dontrun{
#' pagespeed_as_dataframe(get_pagespeed_api("http://finovate.com/blog/", "1234"))
#' }
pagespeed_as_dataframe <- function(pagespeed_api){

  df <- as.data.frame(pagespeed_api$content$pageStats)

  df$speed_score <- pagespeed_api$content$ruleGroups$SPEED$score

  return(df)
}

#' Get Font Size Impact
#'
#' @param pagespeed_api A object of class 'pagespeed_api';
#' normally returned from `get_pagespeed_api`.
#'
#' @return A single number, or NA if no data for this rule
#' @export
#'
#' @examples \dontrun{
#' pagespeed_font_size_impact(get_pagespeed_api("http://finovate.com/blog/", "1234", "mobile"))
#' }
pagespeed_font_size_impact <- function(pagespeed_api){
  font_size <- pagespeed_api$content$formattedResults$ruleResults$UseLegibleFontSizes

  if (is.null(font_size)) return(NA)

  return(font_size$ruleImpact)
}

