#' Heatmap plots for LFMC data
#'
#' @param variable String indicating the variable to be graphed.
#' @param speciescode Integer indicating the species code.
#' @param sitecode Integer or list of values indicating site codes.
#' @param period String indicating if LFMC values are shown on a monthly or a fortnightly basis.
#'
#' @examples
#' \dontrun{
#'    heatmapLFMC(variable = "LFMC", speciescode = 5, sitecode = c(5, 50), period = "Month")
#' }
#'
#' @return A heatmap by site

heatmapLFMC <- function(variable = "LFMC", speciescode = 1, sitecode = 1, period = "Fortnight") {

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

  df = data.frame()  # empty dataframe
  for(site in sitecode) {
    dfSite = dfSp[dfSp[[SiteCode]] == site, ] # filter species dataframe by site in sitecode
    df = rbind(df, dfSite)
    }

  # Monthly data
  if(period == "Month") {
    df$Period <- lubridate::month(df$Date)
    df$PeriodLabel <- factor(df$Period, levels = as.character(1:12),
                             labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                        "Jul","Aug","Sep","Oct","Nov","Dec"))
    }
  # Fortnightly data
  else if(period == "Fortnight") {
    df$Period <- fortnight(df$Date)
    df$PeriodLabel <- factor(df$Period, levels = as.character(seq(1:26)),
                             labels = as.character(seq(1:26)))
  } else {
    stop("argument period must be 'Month' or 'Fortnight'")
  }

  # Define variable 'Year' and transform to class factor
  df$Year = lubridate::year(df$Date)
  df$Year = as.factor(df$Year)

  # Define 'y' labels
  ylabels = levels(df$Year) # using y axis as factor

  # Define 'x' labels
  if(period == "Fortnight") {
    xlab = "Fortnight"
  } else {
    xlab = ""
  }

  # Year as integer when more than one site is graphed
  if(length(sitecode) > 1) {
    df$Year = as.integer(df$Year) # y axis as integer is necessary for sec_axis function
  }

  # heat map
  #------------
  p <- ggplot(df, aes(PeriodLabel, Year, fill = df[[variable]]), group = SiteName)
  p <- p + geom_tile(color = "white")
  p <- p + scale_fill_viridis(name = "LFMC (%)", option = "viridis")

  # generate right 'y' axis in graph for sitecode length > 1
  if(length(sitecode) > 1) {
    p <- p + scale_y_continuous(expand = c(0, 0),
                                breaks = min(df$Year):max(dft$Year),
                                labels = ylabels,
                                sec.axis = sec_axis(~., breaks = min(df$Year):max(df$Year),
                                                    labels = ylabels))
    }
  p <- p + theme_light()
  # Axis text
  p <- p + theme(axis.text.x = element_text(size = 9, angle = 60, vjust = 0.7))
  p <- p + theme(axis.text.y = element_text(size = 10))
  # Axis titles
  p <- p + labs(x = xlab)
  p <- p + theme(axis.title.x = element_text(size = 16))
  p <- p + theme(axis.title.y = element_blank())
  # Plot title
  p <- p + theme(plot.title = element_text(size = 18, face = "italic", hjust = 0.5))
  # Subgraphic titles
  p <- p + theme(strip.text.x = element_text(size = 11, margin = margin(0, 0, 0, 0)))
  # Subgraphics
  p <- p + facet_grid(.~ZoneName + SiteName, scales = "free", space = "free")
  p <- p + ggtitle(SpeciesName)
  # Legend
  p <- p + theme(legend.title = element_text(size = 14))
  p <- p + theme(legend.text = element_text(size = 15))
  p <- p + theme(legend.key.size = unit(0.9, "cm"))
  p <- p + theme(legend.key.width = unit(0.5, "cm"))

  return(p)

  if(length(sitecode) == 1) {
    w = 12
    h = 5
  } else {
    w = 24
    h = 5.5
  }
  ggsave("heatmapLFMC.png", plot = p, width = w, height = h, units = "cm")
}



