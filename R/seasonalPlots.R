#' Seasonal plots with quantiles for LFMC data
#'
#' @param speciesCode Integer indicating the species code.
#' @param siteCode Integer or a list indicating site codes.
#' @param period String indicating if LFMC values are shown by month or fortnight.
#' @param quantiles Vector indicating the quantiles to be displayed.
#' @param LFMCflag Logical indicating if manually identified outliers are excluded.
#' @param AOflag Logical indicating if additive outliers are excluded.
#' @param plotCurrentYear Logical indicating if data from the last two years are shown.
#'
#' @examples
#' \dontrun{
#'    seasonalPlot(speciescode = 1, sitecode = 5, period = "Month",
#'    quantiles = c(0.1, 0.25, 0.50, 0.75, 0.97),
#'    LFMCflag = T, AOflag = T, plotCurrentYear = T)
#' }
#'
#' @return A seasonal plot by site
#'
#' @importFrom dplyr %>%
#' @importFrom ggplot2 aes


seasonalPlot <- function(speciesCode = 1, siteCode = 1, period = "Fortnight",
                         quantiles = c(0.10, 0.50, 0.95), LFMCflag = T, AOflag = F,
                         plotCurrentYear = F) {

  lfmc_db <- DBI::dbConnect(RSQLite::SQLite(), get("lfmcdbfile"))
  rs <- DBI::dbSendQuery(lfmc_db,
                         'SELECT ssp.SamplingSiteCode, s.SamplingSiteName, sp.SpeciesName, lfmc.Date, lfmc.LFMC
                         FROM sites_species ssp
                         INNER JOIN sites s
                         ON ssp.SamplingSiteCode = s.SamplingSiteCode
                         INNER JOIN species sp
                         ON sp.SpeciesCode = ssp.SpeciesCode
                         INNER JOIN lfmc
                         ON lfmc.SiteSpCode = ssp.SiteSpCode
                         WHERE sp.SpeciesCode = ?')

  dbBind(rs, list(speciesCode))
  dfSp = dbFetch(rs)
  dbClearResult(rs)
  dbDisconnect(lfmc_db)

  dfSp$Date = lubridate::as_date(dfSp$Date)

  df = dfSp[dfSp[["SamplingSiteCode"]] == siteCode, ] # filter species dataframe by site

  # Exclude outliers
  if(LFMCflag == T) {
    dfSp <- dfSp[dfSp[["ManualOutlier"]] != 1, ]
  }
  else if(AOflag == T) {
    dfSp <- dfSp[dfSp[["AdditiveOutlier"]] != 1, ]
  }

  # Monthly data
  if(period == "Month") {
    df$Period <- as.numeric(lubridate::month(df$Date))
    df$PeriodLabel <- factor(df$Period, levels = as.character(1:12),
                             labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                        "Jul","Aug","Sep","Oct","Nov","Dec"))
  }
  # Fortnightly data
  else if(period == "Fortnight") {
    df$Period <- ((lubridate::yday(df$Date) - 1) %/% 14) + 1
    seq = seq(1:26)
    df$PeriodLabel <- factor(df$Period, levels = seq,
                             labels = as.character(seq))
  } else {
    stop("argument period must be 'Month' or 'Fortnight'")
  }
  # Group data by period
  byperiod <- df %>% dplyr::group_by(SamplingSiteName, Period)

  # Define plot
  g <- ggplot2::ggplot(byperiod, aes(Period, LFMC))
  # Draw quantiles
  quantiles <- sort(quantiles, decreasing = T)
  nquantiles = length(quantiles)
  v_colors = viridis::viridis(nquantiles) # set of 'nquantiles' number of colors from viridis palette
  for(qtl in quantiles) {
    index = match(qtl, quantiles)
    g <- g + ggplotFL::stat_flquantiles(probs = c(0, qtl),
                              geom = "ribbon",
                              fill = v_colors[[index]],
                              alpha = 0.80)
  }
  # Plot data from last two years
  if(plotCurrentYear) {
    df$Year <- lubridate::year(df$Date)
    DataYear <- df %>% dplyr::group_by(SamplingSiteName) %>%
      dplyr::mutate(maxYear = max(Year))

    # Filter data from last two years
    last_years <- DataYear %>% dplyr::filter(Year == maxYear | Year == (maxYear - 1))
    last_years$Year <- as.factor(last_years$Year)
    last_years <- last_years %>%
      dplyr::group_by(SamplingSiteName, PeriodLabel, Year) %>%
      dplyr::mutate(meanLFMC = mean(LFMC, na.rm = T))

    lastVal <- tail(last_years, 1)

    # Add points for penultimate and last year
    g <- g + ggplot2::geom_point(data = last_years, aes(Period, meanLFMC, shape = Year), size = 1)
    g <- g + ggplot2::scale_shape_manual(values = c(1, 15))
    g <- g + ggplot2::xlim(min(last_years$Period), max(last_years$Period))
    g <- g + ggplot2::theme(legend.position = "bottom")

    # Show last sample
    g <- g + ggplot2::geom_point(data = lastVal, aes(Period, meanLFMC, shape = as.factor(Period)),
                      size = 5, stroke = 1, shape = 1, colour = "red", show.legend = F)
  }
  return(g)
}
