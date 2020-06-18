#' Initialize LFMC database
#'
#' Initializes an empty LFMC database with the appropriate database structure on a new file
#'
#' @param file Database filename
#' @param overwrite Boolean flag to force overwritting an existing file
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
    if(!overwrite) stop(paste0("Database file '", file, "' already exists. Set 'overwrite = TRUE' to force overwriting the database."))
    unlink(file)
    cat(paste0("Database file '", file, "' overwritten.\n"))
  } else {
    cat(paste0("New database file '", file, "' created.\n"))
  }

  lfmc_db <- DBI::dbConnect(RSQLite::SQLite(), file)

  ## Init empty tables

  sites <- DBI::dbSendStatement(lfmc_db, "CREATE TABLE sites
                    (
                    SamplingSiteCode INTEGER PRIMARY KEY,
                    SamplingSiteName VARCHAR,
                    LocalityCode INTEGER,
                    LocalityName VARCHAR,
                    Longitude FLOAT,
                    Latitude FLOAT,
                    StartYear INTEGER,
                    EndYear INTEGER,
                    SensorCode VARCHAR,
                    FOREIGN KEY(SensorCode)
                    REFERENCES tdr_sensor(SensorCode)
                    )")
  DBI::dbClearResult(sites)

  species <- DBI::dbSendStatement(lfmc_db, "CREATE TABLE species
                    (
                    SpeciesCode INTEGER PRIMARY KEY,
                    SpeciesName VARCHAR,
                    SpeciesCAT VARCHAR
                    )")
  DBI::dbClearResult(species)

  sites_species <- DBI::dbSendStatement(lfmc_db, "CREATE TABLE sites_species
                    (
                    SiteSpCode VARCHAR PRIMARY KEY,
                    SamplingSiteCode INTEGER NOT NULL,
                    SpeciesCode INTEGER NOT NULL,
                    FOREIGN KEY(SamplingSiteCode)
                    REFERENCES sites(SamplingSiteCode),
                    FOREIGN KEY(SpeciesCode)
                    REFERENCES species(SpeciesCode)
                    )")
  DBI::dbClearResult(sites_species)

  # Populate sites, species, and sites_species tables

  DBI::dbAppendTable(lfmc_db, "sites", LFMC:::sitesTable)
  DBI::dbAppendTable(lfmc_db, "species", LFMC:::speciesTable)
  DBI::dbAppendTable(lfmc_db, "sites_species", LFMC:::sitespeciesTable)


  lfmc <- DBI::dbSendStatement(lfmc_db, "CREATE TABLE lfmc
                    (SampleCode VARCHAR PRIMARY KEY,
                    SiteSpCode VARCHAR,
                    Date DATE,
                    FreshMass FLOAT, DryMass FLOAT,
                    LFMC FLOAT,
                    DryStem FLOAT, DryLeaf FLOAT,
                    LeafStemRatio FLOAT,
                    ManualOutlier LOGICAL,
                    AdditiveOutlier LOGICAL,
                    PhenologyCode INTEGER, Notes TEXT,
                    FOREIGN KEY(PhenologyCode)
                    REFERENCES phenology(PhenologyCode)
                    FOREIGN KEY(SiteSpCode)
                    REFERENCES sites_species(SiteSpCode)
                    )")
  DBI::dbClearResult(lfmc)

  phenology <- DBI::dbSendStatement(lfmc_db, "CREATE TABLE phenology
                    (PhenologyCode INTEGER PRIMARY KEY,
                    PhenologySystem INTEGER,
                    Phenology INTEGER
                    )")
  DBI::dbClearResult(phenology)

  soil_temp <- DBI::dbSendStatement(lfmc_db, "CREATE TABLE soil_temperature
                    (SensorCode VARCHAR PRIMARY KEY,
                    SamplingSiteCode INTEGER,
                    Date DATE, Time TIME,
                    Temperature FLOAT,
                    FOREIGN KEY(SamplingSiteCode)
                    REFERENCES sites(SamplingSiteCode)
                    )")
  DBI::dbClearResult(soil_temp)

  tdr <- DBI::dbSendStatement(lfmc_db, "CREATE TABLE tdr_sensor
                    (SensorCode VARCHAR PRIMARY KEY,
                    SoilLevel INTEGER
                    )")
  DBI::dbClearResult(tdr)

  soil_moist <- DBI::dbSendStatement(lfmc_db, "CREATE TABLE soil_moisture
                    (SMCode VARCHAR PRIMARY KEY,
                    SensorCode VARCHAR,
                    Date DATE, Time TIME,
                    Moisture FLOAT,
                    FOREIGN KEY(SensorCode)
                    REFERENCES sites(tdr_sensor)
                    )")

  DBI::dbClearResult(soil_moist)

  DBI::dbDisconnect(lfmc_db)

  setDBpath(file)
}



