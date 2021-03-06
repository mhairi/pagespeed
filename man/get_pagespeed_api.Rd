% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{get_pagespeed_api}
\alias{get_pagespeed_api}
\title{Get Google Pagespeed Results}
\usage{
get_pagespeed_api(url, api_key, strategy = NULL)
}
\arguments{
\item{url}{URL to get score for}

\item{api_key}{API key from Google}

\item{strategy}{The analysis strategy to use, either 'desktop' or 'mobile' (optional)}
}
\value{
An object of class 'pagespeed_api'
}
\description{
Get Google Pagespeed Results
}
\examples{
\dontrun{
get_pagespeed_api("http://finovate.com/blog/", "1234")
}
}

