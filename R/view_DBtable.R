view_DBtable<-function(tablename = "lfmc") {
  lfmc_db <- DBI::dbConnect(RSQLite::SQLite(), get("lfmcdbfile"))
  View(dbReadTable(lfmc_db, tablename), tablename)
  dbDisconnect(lfmc_db)
}
