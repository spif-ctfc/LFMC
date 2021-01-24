#' ARIMA parameters
#'
#' The dataset contains parameters required for ARIMA definition in the outlier search routine.
#' These parameters were obtained by fitting an ARIMA for each site-species series,
#' using auto.arima function from forecast package.
#' Data includes parameters for Auto Regressive (AR) and Moving Average (MA) model terms, and seasonal component.
#'
#' @format A data frame with 42 rows and 10 columns
#' \describe{
#' \item{SamplingSiteCode}{Unique code of sampling locality and site}
#' \item{SpeciesCode}{Unique species code}
#' \item{SiteSpCode}{Combination of site and species code}
#' \item{p}{non-seasonal AR order}
#' \item{d}{non-seasonal differencing, defined as d = 0}
#' \item{q}{non-seasonal MA order}
#' \item{sP}{ seasonal AR order}
#' \item{sD}{seasonal differencing}
#' \item{sQ}{seasonal MA order}
#' \item{Method}{fitting method used in auto.arima function}
#' }
#'
#'
"arimaParameters"
