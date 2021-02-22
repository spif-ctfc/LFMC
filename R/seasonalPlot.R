#' Seasonal plots with quantiles for LFMC data
#'
#' @param speciesCode Integer indicating the species code.
#' @param siteCode Integer or a list indicating site codes.
#' @param period String indicating if LFMC values are shown by month or fortnight.
#' @param MOutliers Logical indicating if manually identified outliers are excluded.
#' @param AOutliers Logical indicating if additive outliers are excluded.
#' @param plotCurrentYear Logical indicating if data from the last two years are shown.
#'
#' @import tidyverse ggplotFL viridis
#'
#' @examples
#' \dontrun{
#'    setDBpath("../data/lfmc.sqlite")
#'    seasonalPlot(speciesCode = 1, siteCode = 5, period = "Month")
#' }
#'
#' @return A seasonal plot of an specific species-site request.
#'
#' @export
#'


seasonalPlot <- function(speciesCode = 1, siteCode = 1, period = "Fortnight",
                         MOutliers = T, AOutliers = F, plotCurrentYear = F) {

  lfmc_db <- DBI::dbConnect(RSQLite::SQLite(), get("lfmcdbfile"))
  rs <- DBI::dbSendQuery(
    lfmc_db,
    "SELECT ssp.SamplingSiteCode, s.LocalityName, s.SamplingSiteName,
    sp.SpeciesName, lfmc.Date, lfmc.LFMC, lfmc.ManualOutlier, lfmc.AdditiveOutlier
    FROM sites_species ssp
    INNER JOIN sites s
    ON ssp.SamplingSiteCode = s.SamplingSiteCode
    INNER JOIN species sp
    ON sp.SpeciesCode = ssp.SpeciesCode
    INNER JOIN lfmc
    ON lfmc.SiteSpCode = ssp.SiteSpCode
    WHERE sp.SpeciesCode = ?")

  dbBind(rs, list(speciesCode))
  dfSp = dbFetch(rs)
  dbClearResult(rs)
  dbDisconnect(lfmc_db)

  dfSp$Date = lubridate::as_date(dfSp$Date)

  # Exclude outliers
  if(MOutliers) {
    dfSp <- dfSp[is.na(dfSp$ManualOutlier), ]
  }
  else if(AOutliers) {
    dfSp <- dfSp[is.na(dfSp$AdditiveOutlier), ]
  }

  # Filter species dataframe by requested sites
  df = dfSp[dfSp[["SamplingSiteCode"]] == siteCode, ]

  # Monthly data
  if(period == "Month") {
    df$Period <- as.numeric(lubridate::month(df$Date))
    df$PeriodLabel <- factor(
      df$Period, levels = as.character(1:12),
      labels = c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")
      )
  }
  # Fortnightly data
  else if(period == "Fortnight") {
    df$Period <- ((lubridate::yday(df$Date) - 1) %/% 14) + 1
    seq = seq(1:26)
    df$PeriodLabel <- factor(
      df$Period, levels = seq,
      labels = as.character(seq)
      )
    } else {
      stop("argument period must be 'Month' or 'Fortnight'")
      }
  # Group data by period
  byperiod <- df %>% group_by(LocalityName, SamplingSiteName, Period)

  # Define plot
  g <- ggplot(byperiod, aes(Period, LFMC))

  g <- g + stat_flquantiles(probs=c(0.25, 0.75), geom = "ribbon",
                            fill = "greenyellow", alpha = 0.40)
  g <- g + stat_flquantiles(probs=c(0.50), geom = "line",
                            colour = "darkgreen", alpha = 0.5)
  g <- g + stat_flquantiles(probs = c(0.05), geom = "line",
                            colour = "firebrick2", linetype = 1, alpha = 0.5)
  g <- g + stat_flquantiles(probs = c(0.95), geom = "line",
                            colour = "navyblue", linetype = 1, alpha = 0.5)

  # Plot data from last two years
  if(plotCurrentYear == T) {
    df$Year <- lubridate::year(df$Date)
    DataYear <- df %>%
      group_by(LocalityName, SamplingSiteName) %>%
      mutate(maxYear = max(Year))

    # Filter data from last two years
    last_years <- DataYear %>% dplyr::filter(Year == maxYear | Year == (maxYear - 1))
    last_years <- last_years  %>%
      group_by(LocalityName, SamplingSiteName, PeriodLabel, Year) %>%
      mutate(meanLFMC = mean(LFMC, na.rm = T))

    last_years$Year <- as.factor(last_years$Year)
    # Last recorded LFMC value
    lastVal <- tail(last_years, 1)

    # Add points for penultimate and last year
    g <- g + geom_point(data = last_years, aes(Period, meanLFMC, shape = Year), size = 1)
    # Define a vector of shapes: cases with multiple sites and different last two years by sites
    g <- g + scale_shape_manual(values = c(1, 15))
    g <- g + xlim(min(last_years$Period), max(last_years$Period))
    g <- g + theme(legend.position = "bottom")
    # Show last recorded LFMC value
    g <- g + geom_point(data = lastVal, aes(Period, meanLFMC, shape = as.factor(Period)),
                        size = 5, stroke = 1, shape = 1, colour = "red", show.legend = F)
  }
  return(g)
  return(dfSp)
}
