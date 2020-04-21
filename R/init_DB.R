#' Initialize LFMC database
#'
#' Initializes an empty LFMC database with the appropriate database structure on a new file
#'
#' @param file Database filename
#' @param thesaurus_xlsx Excel 'xlsx' file with thesaurus tables for 'sites' and 'species'
#' @param overwrite Boolean flag to force overwritting an existing file
#'
#' @examples
#'
#' \dontrun{
#'   init_DB("../lfmc", thesaurus_xlsx = "../LFMC_tesaures.xlsx", overwrite=TRUE)
#' }
#'
#'
init_DB <- function(file, thesaurus_xlsx = NULL, overwrite = FALSE) {
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

  if(is.null(thesaurus_xlsx)) {

    DBI::dbSendStatement(lfmc_db, "CREATE TABLE sites
                    (
                    SitePloteCode VARCHAR PRIMARY KEY,
                    UTM_x FLOAT,
                    UTM_y FLOAT,
                    County VARCHAR(50),
                    Town VARCHAR(50),
                    Place VARCHAR(50),
                    StartYear INTEGER,
                    EndYear INTEGER
                    )")

    DBI::dbSendStatement(lfmc_db, "CREATE TABLE species
                    (
                    SpeciesCode INTEGER PRIMARY KEY,
                    SpeciesName VARCHAR(50),
                    SpeciesCAT VARCHAR(50)
                    )")

  } else {
    site_vars = c(SitePlotCode = c("INTEGER", "PRIMARY KEY"),
                  UTM_x = "FLOAT",
                  UTM_y = "FLOAT",
                  County = "VARCHAR(50)",
                  Town = "VARCHAR(50)",
                  Place = "VARCHAR(50)",
                  StartYear = "INTEGER",
                  EndYear = "INTEGER")

    sitesTable = openxlsx::read.xlsx(thesaurus_xlsx, sheet = "sites")
    DBI::dbWriteTable(lfmc_db, "sites", sitesTable, field.types = site_vars)

    species_vars = c(SpeciesCode = c("INTEGER", "PRIMARY KEY"),
                     SpeciesName = "VARCHAR(50)",
                     SpeciesCAT = "VARCHAR(50)")

    speciesTable = openxlsx::read.xlsx(thesaurus_xlsx, sheet = "species")
    DBI::dbWriteTable(lfmc_db, "species", speciesTable, field.types = species_vars)
  }


  DBI::dbSendStatement(lfmc_db, "CREATE TABLE sites_species
                    (
                    SitePlotCode VARCHAR NOT NULL,
                    SpeciesCode INTEGER NOT NULL,
                    FOREIGN KEY(SitePlotCode)
                    REFERENCES sites(SitePlotCode),
                    FOREIGN KEY(SpeciesCode)
                    REFERENCES species(SpeciesCode)
                    )")

  DBI::dbSendStatement(lfmc_db, "CREATE TABLE lfmc
                    (SampleCode INTEGER PRIMARY KEY,
                    SiteXSpecies VARCHAR(10),
                    AgentCode INTEGER, Date DATE,
                    FreshMass FLOAT, DryMass FLOAT,
                    LFMC FLOAT, DryStem FLOAT, DryLeaf FLOAT,
                    LeafStemRatio FLOAT,
                    PhenologyCode INTEGER,Notes TEXT,
                    FOREIGN KEY(SiteXSpecies)
                    REFERENCES sites_species(SiteXSpecies)
                    )")

  DBI::dbSendStatement(lfmc_db, "CREATE TABLE phenology
                    (PhenologyCode INTEGER PRIMARY KEY,
                    PhenologySystem INTEGER,
                    Phenology INTEGER
                    )")

  DBI::dbSendStatement(lfmc_db, "CREATE TABLE soil_temperature
                    (SensorCode VARCHAR PRIMARY KEY,
                    SitePlotCode INTEGER,
                    Date DATE, Time TIME,
                    Temperature FLOAT,
                    FOREIGN KEY(SitePlotCode)
                    REFERENCES sites(SitePlotCode)
                    )")

  DBI::dbSendStatement(lfmc_db, "CREATE TABLE tdr_sensor
                    (SensorCode VARCHAR PRIMARY KEY,
                    SoilLevel INTEGER
                    )")

  DBI::dbSendStatement(lfmc_db, "CREATE TABLE soil_moisture
                    (SMCode VARCHAR PRIMARY KEY,
                    SensorCode VARCHAR,
                    Date DATE, Time TIME,
                    Moisture FLOAT,
                    FOREIGN KEY(SensorCode)
                    REFERENCES sites(tdr_sensor)
                    )")

  # Insert values in table sites_species

  DBI::dbSendQuery(lfmc_db, "INSERT INTO sites_species
                    VALUES
                    (z1s1, 1), (z1s1, 3), (z1s1, 4),
                    (z1s10, 10), (z1s10, 3), (z1s10, 4),
                    (z2s2, 1), (z2s2, 2), (z2s2, 4),
                    (z2s20, 1), (z2s2, 2), (z2s2, 4),
                    (z3s30, 1), (z3s30, 2), (z3s30, 4),
                    (z3s31, 1), (z3s31, 2), (z3s31, 4),
                    (z3s32, 1), (z3s32, 2), (z3s32, 4),
                    (z3s33, 1), (z3s33, 2), (z3s33, 4),
                    (z3s34, 1), (z3s34, 2), (z3s34, 4),
                    (z3s35, 1), (z3s35, 2), (z3s35, 4),
                    (z4s4, 1), (z4s4, 2), (z4s4, 4),
                    (z5s5, 1), (z5s5, 2), (z5s5, 4),
                    (z5s50, 1), (z5s50, 3), (z5s50, 4),
                    (z6s6, 3), (z6s6, 5),
                    (z7s7, 1), (z7s7, 2),
                    (z8s8, 1), (z8s8, 2),
                    (z9s9, 2), (z9s9, 3),
                    (z9s90, 1), (z9s90, 2), (z9s90, 3), (z9s90, 4)")

  # Add new column with code SiteXSpecies

  DBI::dbSendStatement(lfmc_db, "ALTER TABLE sites_species
                       ADD COLUMN SiteXSpecies")

  DBI::dbSendStatement(lfmc_db, "UPDATE sites_species
                       SET SiteXSpecies = cast(SitePlotCode || 'sp' || SpeciesCode
                       AS VARCHAR(10))")

  # Define the primary key constraint for table sites_species

  DBI::dbSendStatement(lfmc_db, "ALTER TABLE sites_species
                       RENAME TO ssp")

  DBI::dbSendStatement(lfmc_db, "CREATE TABLE sites_species
                    (
                    SitePlotCode VARCHAR NOT NULL,
                    SpeciesCode INTEGER NOT NULL,
                    SiteXSpecies PRIMARY KEY,
                    FOREIGN KEY(SitePlotCode)
                    REFERENCES sites(SitePlotCode),
                    FOREIGN KEY(SpeciesCode)
                    REFERENCES species(SpeciesCode)
                    )")

  DBI::dbSendStatement(lfmc_db, "INSERT INTO sites_species
                       SELECT * FROM ssp")

  DBI::dbSendStatement(lfmc_db, "DROP TABLE ssp")

  DBI::dbDisconnect(lfmc_db)

  set_DBpath(file)
}



