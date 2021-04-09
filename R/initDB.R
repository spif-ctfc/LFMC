#' Initialize LFMC database
#'
#' Initializes an empty LFMC database with the appropriate database structure on a new file
#'
#' @param file Database file name
#' @param overwrite Boolean flag to force overwriting an existing file
#'
#' @examples
#'
#' \dontrun{
#'   initDB("../lfmc", overwrite = TRUE)
#' }
#'
#' @export
#'

initDB <- function(file, overwrite = FALSE) {
  if(!endsWith(file, ".sqlite")) file = paste0(file, ".sqlite")
  if(file.exists(file)) {
    if(!overwrite)
      stop(
        paste0(
          "Database file '", file, "' already exists. Set 'overwrite = TRUE' to force overwriting the database.")
        )
    unlink(file)
    cat(paste0("Database file '", file, "' overwritten.\n"))
  } else {
    cat(paste0("New database file '", file, "' created.\n"))
  }

  lfmc_db <- DBI::dbConnect(RSQLite::SQLite(), file)

  ## Initiate empty tables

  sites <- DBI::dbSendStatement(
  lfmc_db,
  "CREATE TABLE sites(
  SamplingSiteCode INTEGER PRIMARY KEY,
  SamplingSiteName TEXT,
  LocalityCode INTEGER,
  LocalityName TEXT,
  Longitude REAL,
  Latitude REAL,
  StartYear INTEGER,
  EndYear INTEGER,
  SensorCode TEXT,
  FOREIGN KEY(SensorCode)
  REFERENCES tdr_sensor(SensorCode)
  )"
  )
  DBI::dbClearResult(sites)

  species <- DBI::dbSendStatement(
  lfmc_db,
  "CREATE TABLE species(
  SpeciesCode INTEGER PRIMARY KEY,
  SpeciesName TEXT,
  SpeciesCAT TEXT
  )"
  )
  DBI::dbClearResult(species)

  sites_species <- DBI::dbSendStatement(
  lfmc_db,
  "CREATE TABLE sites_species(
  SiteSpCode TEXT PRIMARY KEY,
  SamplingSiteCode INTEGER NOT NULL,
  SpeciesCode INTEGER NOT NULL,
  FOREIGN KEY(SamplingSiteCode)
  REFERENCES sites(SamplingSiteCode),
  FOREIGN KEY(SpeciesCode)
  REFERENCES species(SpeciesCode)
  )"
  )
  DBI::dbClearResult(sites_species)

  # Populate sites, species, and sites_species tables

  load("R/sysData.rda")
  DBI::dbAppendTable(lfmc_db, "sites", sitesTable)
  DBI::dbAppendTable(lfmc_db, "species", speciesTable)
  DBI::dbAppendTable(lfmc_db, "sites_species", sitespeciesTable)

  lfmc <- DBI::dbSendStatement(
  lfmc_db,
  "CREATE TABLE lfmc(
  SampleCode TEXT PRIMARY KEY,
  SiteSpCode TEXT,
  Date NUMERIC,
  FreshMass REAL, DryMass REAL,
  LFMC REAL,
  DryStem REAL, DryLeaf REAL,
  LeafStemRatio REAL,
  ManualOutlier NUMERIC,
  AdditiveOutlier NUMERIC,
  PhenologyCode INTEGER, Notes TEXT,
  FOREIGN KEY(PhenologyCode)
  REFERENCES phenology(PhenologyCode)
  FOREIGN KEY(SiteSpCode)
  REFERENCES sites_species(SiteSpCode)
  )"
  )
  DBI::dbClearResult(lfmc)

  phenology <- DBI::dbSendStatement(
  lfmc_db,
  "CREATE TABLE phenology(
  PhenologyCode INTEGER PRIMARY KEY,
  PhenologySystem INTEGER,
  Phenology INTEGER
  )"
  )
  DBI::dbClearResult(phenology)

  tdr <- DBI::dbSendStatement(
  lfmc_db,
  "CREATE TABLE tdr_sensor
  (SensorCode TEXT PRIMARY KEY,
  SoilLevel INTEGER
  )"
  )
  DBI::dbClearResult(tdr)

  soil_measurements <- DBI::dbSendStatement(
  lfmc_db,
  "CREATE TABLE soil_measurements
  (SMCode TEXT PRIMARY KEY,
  SensorCode TEXT,
  Date NUMERIC, Time NUMERIC,
  Moisture REAL,
  Temperature REAL,
  FOREIGN KEY(SensorCode)
  REFERENCES sites(tdr_sensor)
  )"
  )
  DBI::dbClearResult(soil_measurements)

  DBI::dbDisconnect(lfmc_db)

  setDBpath(file)
}
