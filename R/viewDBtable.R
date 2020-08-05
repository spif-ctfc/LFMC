extractDBtable <- function(tablename = "lfmc") {
  lfmc_db <- DBI::dbConnect(RSQLite::SQLite(), get("lfmcdbfile"))
  table = DBI::dbReadTable(lfmc_db, tablename)
  if(tablename == "lfmc") {
    table$Date = as.Date(table$Date, origin = "1970-01-01")
  }
  DBI::dbDisconnect(lfmc_db)
  return(table)
}

viewDBtable <- function(tablename = "lfmc") {
  View(extractDBtable(tablename), tablename)
}


