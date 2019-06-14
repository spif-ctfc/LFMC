#' Initializes an empty LFMC database with the appropriate database structure on a new file
#'
#' @param file Database filename
#' @param overwrite Boolean flag to force overwritting an existing file
#'
#' @return NULL
#'
#' @examples
#'
#' init_DB("../lfmc")
#'
init_DB<-function(file, overwrite = FALSE) {
  if(!endsWith(file, ".sqlite")) file = paste0(file, ".sqlite")
  if(file.exists(file)) {
    if(!overwrite) stop(paste0("Database file '", file, "' already exists. Set 'overwrite = TRUE' to force overwriting the database."))
    else cat(paste0("Database file '", file, "' overwritten."))
  } else {
    cat(paste0("New database file '", file, "' created."))
  }
  lfmc_db <- DBI::dbConnect(RSQLite::SQLite(), file, overwrite = overwrite)

  ## Init empty tables
  agents = data.frame(AgentCode = NA, AgentName = NA, County = NA)
  dbWriteTable(lfmc_db, "agents", agents, overwrite = overwrite)

  plots = data.frame(PlotCode = NA, SiteCode = NA, UTM_x = NA, UTM_y = NA, MeanHeight= NA, MeanCover = NA,
                     StartYear = NA, EndYear = NA)
  dbWriteTable(lfmc_db, "plots", plots, overwrite = overwrite)

  sites = data.frame(SiteCode = NA, CurrentPlotCode = NA, County = NA, Town = NA, TownCode = NA, Place = NA, Species1 = NA, Species2 = NA, Species3 = NA)
  dbWriteTable(lfmc_db, "sites", sites, overwrite = overwrite)

  species = data.frame(SpeciesCode = NA, Genus = NA, Species = NA)
  dbWriteTable(lfmc_db, "species", species, overwrite = overwrite)

  lfmc = data.frame(SiteCode = NA, AgentCode = NA, SpeciesCode = NA, Date = NA, DryMass = NA, LFMC = NA)
  dbWriteTable(lfmc_db, "lfmc", lfmc, overwrite = overwrite)

  soilmoisture = data.frame(SiteCode = NA, Date = NA, Time = NA, TopTemp = NA, BottomTemp = NA, TopMoisture = NA, BottomMoisture = NA)
  dbWriteTable(lfmc_db, "soilmoisture", soilmoisture, overwrite = overwrite)

  DBI::dbDisconnect(lfmc_db)
}
