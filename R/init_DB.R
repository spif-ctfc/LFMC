#' Initialize LFMC database
#'
#' Initializes an empty LFMC database with the appropriate database structure on a new file
#'
#' @param file Database filename
#' @param thesaurus_xlsx Excel 'xlsx' file with thesaurus tables for 'plots',
#'                      'sites', 'species' and 'agents'.
#' @param overwrite Boolean flag to force overwritting an existing file
#'
#' @examples
#'
#' \dontrun{
#'   init_DB("../lfmc", thesaurus_xlsx = "../LFMC_tesaures.xlsx", overwrite=TRUE)
#' }
#'
#'
init_DB<-function(file, thesaurus_xlsx =NULL, overwrite = FALSE) {
  if(!endsWith(file, ".sqlite")) file = paste0(file, ".sqlite")
  if(file.exists(file)) {
    if(!overwrite) stop(paste0("Database file '", file, "' already exists. Set 'overwrite = TRUE' to force overwriting the database."))
    unlink(file)
    cat(paste0("Database file '", file, "' overwritten.\n"))
  } else {
    cat(paste0("New database file '", file, "' created.\n"))
  }
  lfmc_db <- DBI::dbConnect(RSQLite::SQLite(), file)

  ## Init empty tables
  int_type = dbDataType(lfmc_db, as.integer(1996))
  dbl_type = dbDataType(lfmc_db, 1.2)
  txt_type = dbDataType(lfmc_db, "abc")
  date_type = dbDataType(lfmc_db, Sys.Date())
  time_type = dbDataType(lfmc_db, Sys.time())

  if(is.null(thesaurus_xlsx)) {
    agent_vars =  c("AgentCode" = int_type,
                    "AgentName" = txt_type)
    dbCreateTable(lfmc_db, "agents",agent_vars)

    plot_vars = c(PlotCode = int_type, SiteCode = int_type,
                  UTM_x = dbl_type, UTM_y = dbl_type,
                  MeanHeight= dbl_type, MeanCover = dbl_type,
                  StartYear = int_type, EndYear = int_type)
    dbCreateTable(lfmc_db, "plots", plot_vars)

    site_vars = c(SiteCode = int_type,
                  CurrentPlotCode = int_type,
                  County = txt_type,
                  Town = txt_type,
                  Place = txt_type,
                  Species1Code = int_type, Species2Code = int_type, Species3Code = int_type)
    dbCreateTable(lfmc_db, "sites", site_vars)

    species_vars = c(SpeciesCode = int_type, Genus = txt_type, Species = txt_type)
    dbCreateTable(lfmc_db, "species", species_vars)
  } else {
    sitesTable = openxlsx::read.xlsx(thesaurus_xlsx, sheet = "sites")
    dbWriteTable(lfmc_db, "sites", sitesTable)

    plotsTable = openxlsx::read.xlsx(thesaurus_xlsx, sheet = "plots")
    dbWriteTable(lfmc_db, "plots", plotsTable)

    speciesTable = openxlsx::read.xlsx(thesaurus_xlsx, sheet = "species")
    dbWriteTable(lfmc_db, "species", speciesTable)

    agentsTable = openxlsx::read.xlsx(thesaurus_xlsx, sheet = "agents")
    dbWriteTable(lfmc_db, "agents", agentsTable)
  }

  lfmc_vars = c(SiteCode = int_type, AgentCode = int_type, SpeciesCode = int_type,
                Date = date_type, SampleCode = txt_type,
                FreshMass = dbl_type,
                DryMass = dbl_type, DryStem = dbl_type, DryLeaf = dbl_type,
                LFMC = dbl_type,
                PhenologyCode = int_type, PhenologySystem = int_type,
                Notes = txt_type)
  dbCreateTable(lfmc_db, "lfmc", lfmc_vars)

  soilmoisture_vars = c(SiteCode = int_type,
                        Date = date_type, Time = time_type,
                        TopTemp = dbl_type, BottomTemp = dbl_type,
                        TopMoisture = dbl_type, BottomMoisture = dbl_type)
  dbCreateTable(lfmc_db, "soilmoisture", soilmoisture_vars)

  DBI::dbDisconnect(lfmc_db)

  set_DBpath(file)
}

