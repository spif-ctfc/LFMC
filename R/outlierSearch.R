#' Search outliers in LFMC data
#'
#' Routine for search and flagg outliers in LFMC data
#'
#' @param lfmc_db LFMC database connection
#' @param plotImpute Whether or not plot imputations by site_species series
#' @param plotOutlier Whether or not plot outliers by site_species series
#'
#' @details Outlier search is carried out only for LFMC series with more than 15 years of data.
#' The parameters for the ARIMA model fitted to detect outliers by LFMC series are read from 'arimaParameters.rda'.
#' Delta parameter for TC was specified as 0.5 for Q. coccifera and 0.7 for the other species.
#'
#' @examples
#' \dontrun{
#' LFMC::setDBpath("lfmc.sqlite")
#' lfmc_db <- DBI::dbConnect(RSQLite::SQLite(), get("lfmcdbfile"))
#' outlierSearch(lfmc_db)
#' dbDisconnect(lfmc_db)
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
#'

outlierSearch <- function(lfmc_db, plotImpute = F, plotOutlier = F) {

  lfmc_df <- DBI::dbGetQuery(
  lfmc_db,
  "SELECT lfmc.SiteSpCode, lfmc.Date, lfmc.LFMC, lfmc.AdditiveOutlier,
  ssp.SpeciesCode, ssp.SamplingSiteCode
  FROM lfmc lfmc JOIN sites_species ssp
  ON lfmc.SiteSpCode = ssp.SiteSpCode"
  )

  lfmc_df$Date = lubridate::as_date(lfmc_df$Date)
  lfmc_df$Year = lubridate::year(lfmc_df$Date)
  lfmc_df$Fortnight = ((lubridate::yday(lfmc_df$Date) - 1) %/% 14) + 1

  lfmc_df <- lfmc_df %>%
    dplyr::mutate(SamplingSiteCode = as.double(SamplingSiteCode)) %>%
    dplyr::mutate(SamplingSiteCode = dplyr::case_when(SamplingSiteCode == 31 ~ 3,
                                                      SamplingSiteCode == 32 ~ 3,
                                                      SamplingSiteCode == 33 ~ 3,
                                                      SamplingSiteCode == 34 ~ 3,
                                                      TRUE ~ SamplingSiteCode)) %>%
    dplyr::mutate(SamplingSiteCode = as.integer(SamplingSiteCode))

  # lfmc series with more than 15 years of data
  lfmc_df <- lfmc_df %>%
    dplyr::group_by(SiteSpCode, SpeciesCode, SamplingSiteCode) %>%
    dplyr::mutate(Ayears = dplyr::n_distinct(Year)) %>%
    dplyr::filter(Ayears > 14)

  lfmc_df = lfmc_df %>% tidyr::complete(Fortnight, tidyr::nesting(SiteSpCode))

  for(ssp in unique(lfmc_df$SiteSpCode)) {
    series <- lfmc_df[lfmc_df$SiteSpCode == ssp, ]
    series <- series[order(series$Year), ]

    tsNA <- stats::ts(data = series$LFMC,
                   start = c(min(series$Year, na.rm = T), 1),
                   end = c(max(series$Year, na.rm = T), 26),
                   frequency = 26)
    ts <- imputeTS::na_ma(tsNA , k = 4, weighting = "linear")

    if(plotImpute)
      imputeTS::plotNA.imputations(tsNA, ts, main = paste0("Imputations for ", ssp))

    AP <- arimaParameters
    # Arima no seasonal component
    p = AP[AP[["SiteSpCode"]] == ssp, ]$p
    d = AP[AP[["SiteSpCode"]] == ssp, ]$d
    q = AP[AP[["SiteSpCode"]] == ssp, ]$q
    # Arima seasonal component
    sP = AP[AP[["SiteSpCode"]] == ssp, ]$sP
    sD = AP[AP[["SiteSpCode"]] == ssp, ]$sD
    sQ = AP[AP[["SiteSpCode"]] == ssp, ]$sQ

    listS = list(order = c(sP, sD, sQ))
    if(sum(is.na(listS$order)) > 0) listS = list(order = c(0, 1, 1)) # default values in tso function

    if(series$SpeciesCode == 2) {
      outliers = tsoutliers::tso(ts, types = c("AO", "TC"), tsmethod = "arima",
                     args.tsmethod = list(order = c(p, d, q), seasonal = listS), delta = 0.5)
    } else {
      outliers = tsoutliers::tso(ts, types = c("AO", "TC"), tsmethod = "arima",
                     args.tsmethod = list(order = c(p, d, q), seasonal = listS), delta = 0.7)
    }

    if(plotOutlier) plot(outliers)

    Outliers = outliers[1]
    for(i in Outliers){
      for(j in i$ind) {
        index = match(j, i[ , "ind"])
        if(i[ ,"type"][[index]] == "AO") {
          lfmc_df$AdditiveOutlier[[j]] <- 1 # flag for AO
        }
      }
    }
  }

  lfmc_df = subset(lfmc_df, select = -c(SpeciesCode, SamplingSiteCode, Year, Fortnight, Ayears))
  # Replace lfmc table
  dbRemoveTable(lfmc_db, "lfmc")
  dbWriteTable(lfmc_db, "lfmc", lfmc_df)
}

