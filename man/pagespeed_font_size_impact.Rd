% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{pagespeed_font_size_impact}
\alias{pagespeed_font_size_impact}
\title{Get Font Size Impact}
\usage{
pagespeed_font_size_impact(pagespeed_api)
}
\arguments{
\item{pagespeed_api}{A object of class 'pagespeed_api';
normally returned from `get_pagespeed_api`.}
}
\value{
A single number, or NA if no data for this rule
}
\description{
Get Font Size Impact
}
\examples{
\dontrun{
pagespeed_font_size_impact(get_pagespeed_api("http://finovate.com/blog/", "1234", "mobile"))
}
}

