extract_DBtable<-function(tablename = "lfmc") {
  lfmc_db <- DBI::dbConnect(RSQLite::SQLite(), get("lfmcdbfile"))
  table = dbReadTable(lfmc_db, tablename)
  if(tablename=="lfmc") {
    table$Date = as.Date(table$Date, origin = "1970-01-01")
  }
  dbDisconnect(lfmc_db)
  return(table)
}

view_DBtable<-function(tablename = "lfmc") {
  View(extract_DBtable(tablename), tablename)
}
