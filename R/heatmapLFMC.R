#' Heatmap plots for LFMC data
#'
#' @param speciesCode Integer indicating the species code.
#' @param siteCode Integer indicating the site code.
#' @param period String indicating if LFMC values are shown on a monthly or a fortnightly basis.
#'
#' @examples
#' \dontrun{
#'    heatmapLFMC(speciesCode = 5, siteCode = 50, period = "Month")
#' }
#'
#' @return A heatmap of an specific species-site request.

heatmapLFMC <- function(speciesCode = 1, siteCode = 1, period = "Fortnight") {

  lfmc_db <- DBI::dbConnect(RSQLite::SQLite(), get("lfmcdbfile"))
  rs <- DBI::dbSendQuery(
    lfmc_db,
    'SELECT ssp.SamplingSiteCode, s.SamplingSiteName, sp.SpeciesName, lfmc.Date, lfmc.LFMC
    FROM sites_species ssp
    INNER JOIN sites s
    ON ssp.SamplingSiteCode = s.SamplingSiteCode
    INNER JOIN species sp
    ON sp.SpeciesCode = ssp.SpeciesCode
    INNER JOIN lfmc
    ON lfmc.SiteSpCode = ssp.SiteSpCode
    WHERE sp.SpeciesCode = ?'
    )

  dbBind(rs, list(speciesCode))
  dfSp = dbFetch(rs)
  dbClearResult(rs)
  dbDisconnect(lfmc_db)

  dfSp$Date = lubridate::as_date(dfSp$Date)

  df = dfSp[dfSp[["SamplingSiteCode"]] == siteCode, ] # filter species dataframe by site

  # Monthly data
  if(period == "Month") {
    df$Period <- lubridate::month(df$Date)
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
      df$Period, levels = seq, labels = as.character(seq)
      )
    } else {
    stop("argument period must be 'Month' or 'Fortnight'")
  }

  # Define variable 'Year' and transform to class factor
  df$Year = lubridate::year(df$Date)
  df$Year = as.factor(df$Year)

  # heatmap
  p <- ggplot2::ggplot(df, ggplot2::aes(PeriodLabel, Year, fill = LFMC))
  # Axis titles
  p <- p + ggplot2::labs(x = "", y = "")
  # Plot title
  p <- p + ggplot2::geom_tile(color = "white")
  p <- p + viridis::scale_fill_viridis(name = "% LFMC", option = "viridis")
  p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40))
  p <- p + ggplot2::ggtitle(paste(df$SpeciesName, df$SamplingSiteName, sep = " - "))
  return(p)
}



