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
                    SitePloteCode INTEGER PRIMARY KEY,
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
                    SitePlotCode INTEGER NOT NULL,
                    SpeciesCode INTEGER NOT NULL,
                    FOREIGN KEY(SitePlotCode)
                    REFERENCES sites(SitePlotCode),
                    FOREIGN KEY(SpeciesCode)
                    REFERENCES species(SpeciesCode)
                    )")

  DBI::dbSendStatement(lfmc_db, "CREATE TABLE lfmc
                    (SampleCode INTEGER PRIMARY KEY,
                    SiteXSpecies VARCHAR(5),
                    AgentCode INTEGER, Date DATE,
                    FreshMass FLOAT, DryMass FLOAT,
                    LFMC FLOAT, DryStem FLOAT, DryLeaf FLOAT,
                    LeafStemRatio FLOAT, Notes TEXT,
                    FOREIGN KEY(SiteXSpecies)
                    REFERENCES sites_species(SiteXSpecies)
                    )")

  DBI::dbSendStatement(lfmc_db, "CREATE TABLE soil_temperature
                    (SensorCode INTEGER PRIMARY KEY,
                    SitePlotCode INTEGER,
                    Date DATE, Time TIME,
                    Temperature FLOAT,
                    FOREIGN KEY(SitePlotCode)
                    REFERENCES sites(SitePlotCode)
                    )")

  DBI::dbSendStatement(lfmc_db, "CREATE TABLE soil_moisture
                    (SensorCode INTEGER PRIMARY KEY,
                    SitePlotCode INTEGER,
                    Date DATE, Time TIME,
                    Moisture FLOAT,
                    FOREIGN KEY(SitePlotCode)
                    REFERENCES sites(SitePlotCode)
                    )")

  # Insert values in table sites_species

  DBI::dbSendQuery(lfmc_db, "INSERT INTO sites_species
                    VALUES
                    (1, 1), (1, 3), (1, 4),
                    (2, 1), (2, 2), (2, 4),
                    (3, 1), (3, 2), (3, 4),
                    (4, 1), (4, 2), (4, 4),
                    (5, 1), (5, 2), (5, 3), (5, 4),
                    (6, 3), (6, 5),
                    (7, 1), (7, 2),
                    (8, 1), (8, 2),
                    (9, 1), (9, 2), (9, 3)")

  # Add new column with code SiteXSpecies

  DBI::dbSendStatement(lfmc_db, "ALTER TABLE sites_species
                       ADD COLUMN SiteXSpecies")

  DBI::dbSendStatement(lfmc_db, "UPDATE sites_species
                       SET SiteXSpecies = cast(SitePlotCode || '_' || SpeciesCode
                       AS VARCHAR(5))")

  # Define the primary key constraint for table sites_species

  DBI::dbSendStatement(lfmc_db, "ALTER TABLE sites_species
                       RENAME TO ssp")

  DBI::dbSendStatement(lfmc_db, "CREATE TABLE sites_species
                    (
                    SitePlotCode INTEGER NOT NULL,
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



