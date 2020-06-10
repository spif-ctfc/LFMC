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
                    SamplingSiteCode INTEGER PRIMARY KEY,
                    SamplingSiteName VARCHAR,
                    LocalityCode INTEGER,
                    LocalityName VARCHAR,
                    UTM_x FLOAT,
                    UTM_y FLOAT,
                    StartYear INTEGER,
                    EndYear INTEGER,
                    SensorCode VARCHAR,
                    FOREIGN KEY(SensorCode)
                    REFERENCES tdr_sensor(SensorCode)
                    )")

    DBI::dbSendStatement(lfmc_db, "CREATE TABLE species
                    (
                    SpeciesCode INTEGER PRIMARY KEY,
                    SpeciesName VARCHAR(50),
                    SpeciesCAT VARCHAR(50)
                    )")

  } else {

    site_vars = c(SamplingSiteCode = c("INTEGER", "PRIMARY KEY"),
                  SamplingSiteName = "VARCHAR",
                  LocalityCode = "INTEGER",
                  LocalityName = "VARCHAR(50)",
                  UTM_x = "FLOAT",
                  UTM_y = "FLOAT",
                  StartYear = "INTEGER",
                  EndYear = "INTEGER",
                  SensorCode = "VARCHAR")

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
                    SamplingSiteCode INTEGER NOT NULL,
                    SpeciesCode INTEGER NOT NULL,
                    FOREIGN KEY(SamplingSiteCode)
                    REFERENCES sites(SamplingSiteCode),
                    FOREIGN KEY(SpeciesCode)
                    REFERENCES species(SpeciesCode)
                    )")

  DBI::dbSendStatement(lfmc_db, "CREATE TABLE lfmc
                    (SampleCode INTEGER PRIMARY KEY,
                    SiteSpCode VARCHAR(10),
                    Date DATE,
                    FreshMass FLOAT, DryMass FLOAT,
                    LFMC FLOAT, DryStem FLOAT, DryLeaf FLOAT,
                    LeafStemRatio FLOAT,
                    ManualOutlier BOOLEAN,
                    AdditiveOutlier BOOLEAN,
                    ImputedOutlier BOOLEAN,
                    PhenologyCode INTEGER, Notes TEXT,
                    FOREIGN KEY(PhenologyCode)
                    REFERENCES phenology(PhenologyCode)
                    FOREIGN KEY(SiteSpCode)
                    REFERENCES sites_species(SiteSpCode)
                    )")

  DBI::dbSendStatement(lfmc_db, "CREATE TABLE phenology
                    (PhenologyCode INTEGER PRIMARY KEY,
                    PhenologySystem INTEGER,
                    Phenology INTEGER
                    )")

  DBI::dbSendStatement(lfmc_db, "CREATE TABLE soil_temperature
                    (SensorCode VARCHAR PRIMARY KEY,
                    SamplingSiteCode INTEGER,
                    Date DATE, Time TIME,
                    Temperature FLOAT,
                    FOREIGN KEY(SamplingSiteCode)
                    REFERENCES sites(SamplingSiteCode)
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

  # Insert values (SamplingSiteCode and SpeciesCode) in table sites_species

  DBI::dbSendQuery(lfmc_db, "INSERT INTO sites_species
                    VALUES
                    (1, 1), (1, 3), (1, 4),
                    (10, 10), (10, 3), (10, 4),
                    (2, 1), (2, 2), (2, 4),
                    (20, 1), (20, 2), (20, 4),
                    (30, 1), (30, 2), (30, 4),
                    (31, 1), (31, 2), (31, 4),
                    (32, 1), (32, 2), (32, 4),
                    (33, 1), (33, 2), (33, 4),
                    (34, 1), (34, 2), (34, 4),
                    (35, 1), (35, 2), 35, 4),
                    (4, 1), (4, 2), (4, 4),
                    (5, 1), (5, 2), (5, 4),
                    (50, 1), (50, 3), (50, 4),
                    (6, 3), (6, 5),
                    (7, 1), (7, 2),
                    (8, 1), (8, 2),
                    (9, 2), (9, 3),
                    (90, 1), (90, 2), (90, 3), (90, 4)")

  # Add new column with code Site-Species

  DBI::dbSendStatement(lfmc_db, "ALTER TABLE sites_species
                       ADD COLUMN SiteSpCode")

  DBI::dbSendStatement(lfmc_db, "UPDATE sites_species
                       SET SiteSpCode = cast('s' || SamplingSiteCode || 'sp' || SpeciesCode
                       AS VARCHAR(6))")

  # Define the primary key constraint for table sites_species

  DBI::dbSendStatement(lfmc_db, "ALTER TABLE sites_species
                       RENAME TO ssp")

  DBI::dbSendStatement(lfmc_db, "CREATE TABLE sites_species
                    (
                    SamplingSiteCode INTEGER NOT NULL,
                    SpeciesCode INTEGER NOT NULL,
                    SiteSpCode PRIMARY KEY,
                    FOREIGN KEY(SamplingSiteCode)
                    REFERENCES sites(SamplingSiteCode),
                    FOREIGN KEY(SpeciesCode)
                    REFERENCES species(SpeciesCode)
                    )")

  DBI::dbSendStatement(lfmc_db, "INSERT INTO sites_species
                       SELECT * FROM ssp")

  DBI::dbSendStatement(lfmc_db, "DROP TABLE ssp")

  DBI::dbDisconnect(lfmc_db)

  set_DBpath(file)
}



