#' Seasonal plots with quantiles for LFMC data
#'
#' @param speciescode Integer indicating the species code.
#' @param sitecode Integer or a list indicating site codes.
#' @param period String indicating if LFMC values are shown by month or fortnight.
#' @param quantiles Vector indicating the quantiles to be displayed.
#' @param LFMCflag Logical indicating if manually identified outliers are excluded.
#' @param AOflag Logical indicating if additive outliers are excluded.
#' @param plotCurrentYear Logical indicating if data from the last two years are shown.
#'
#' @examples
#' \dontrun{
#'    seasonalPlot(speciescode = 1, sitecode = c(1, 5),
#'    period = "Month", quantiles = c(0.1, 0.25, 0.50, 0.75, 0.97), data = T)
#' }
#'
#' @return A seasonal plot by site


seasonalPlot <- function(speciescode = 1, sitecode = 1, period = "Fortnight",
                         quantiles = c(0.10, 0.50, 0.95), LFMCflag = T, AOflag = F,
                         plotCurrentYear = F) {

  lfmc_db <- DBI::dbConnect(RSQLite::SQLite(), get("lfmcdbfile"))
  rs <- DBI::dbSendQuery(lfmc_db,
                         'SELECT ZoneName, SiteCode, SiteName, SpeciesName, Date, LFMC
                         FROM sites_species ssp
                         INNER JOIN sites s
                         ON ssp.SitePlotCode = s.SitePlotCode
                         INNER JOIN species sp
                         ON sp.SpeciesCode = ssp.SpeciesCode
                         INNER JOIN lfmc
                         ON lfmc.SiteXSpecies = ssp.SiteXSpecies
                         WHERE SpeciesCode = ?')
  dbBind(rs, list(SpeciesCode))
  dfSp = dbFetch(rs)
  dbClearResult(rs)
  dbDisconnect(lfmc_db)

  # Exclude outliers
  if(LFMCflag == T) {
    dfSp <- dfSp[is.na(dfSp$LFMC_flag), ]
  }
  else if(AOflag == T) {
    dfSp <- dfSp[is.na(dfSp$AO_flag), ]
  }

  # Filter species dataframe by requested sites
  df = data.frame()
  for(site in sitecode) {
    dfSite = dfSp[dfSp[["SiteCode"]] == site, ]
    df = rbind(df, dfSite)
  }

  df <- select(df, SiteCode, SpeciesCode, ZoneName, SiteName, Species, Date, LFMC)
  df <- na.omit(df)

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
  byperiod <- df %>% group_by(ZoneName, SiteName, Period)

  # Define plot
  g <- ggplot(byperiod, aes(Period, LFMC))
  # Draw quantiles
  quantiles <- sort(quantiles, decreasing = T)
  nquantiles = length(quantiles)
  v_colors =  viridis(nquantiles) # set of 'nquantiles' number of colors from viridis palette
  for(qtl in quantiles) {
    index = match(qtl, quantiles)
    g <- g + stat_flquantiles(probs = c(0, qtl),
                              geom = "ribbon",
                              fill = v_colors[[index]],
                              alpha = 0.80)
  }
  # Plot data from last two years
  if(plotCurrentYear == T) {
    df$Year <- lubridate::year(df$Date)
    DataYear <- df %>% group_by(ZoneName, SiteName) %>%
      mutate(maxYear = max(Year))

    # Filter data from last two years
    last_years <- DataYear %>% dplyr::filter(Year == maxYear | Year == (maxYear - 1))
    last_years <- last_years  %>%
      group_by(ZoneName, SiteName, PeriodLabel, Year) %>%
      mutate(meanLFMC = mean(LFMC, na.rm = T))

    last_years$Year <- as.factor(last_years$Year)

    # Add points for penultimate and last year
    g <- g + geom_point(data = last_years, aes(Period, meanLFMC, shape = Year))
    g <- g + scale_shape_manual(values = c(1, 16, 0, 15, 2, 17, 5, 18))
    g <- g + xlim(min(last_years$Period), max(last_years$Period))
    g <- g + theme(legend.position = "bottom")
  }
  # Subgraphics: when more than one site per species is requested
  if(length(sitecode > 1)) {
    g <- g + facet_grid(.~ZoneName + SiteName, scales = "free", space = "free")
  }
  return(g)
}
