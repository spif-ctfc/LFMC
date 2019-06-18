#' Exports LFMC data base
#'
#' Exports whole LFMC data base into an excel (*.xlsx) file
#'
#' @param xlsxfile A string with the path to the destination file (*.xlsx)
#' @param overwrite A boolean flag to indicate that file should be overwritten
#'
#' @examples
#'
#' \dontrun{
#'   export_xlsx("../lfmc_backup.xlsx")
#' }
export_xlsx<-function(xlsxfile, overwrite = FALSE) {
  if(!endsWith(xlsxfile, ".xlsx")) xlsxfile = paste0(xlsxfile, ".xlsx")

  book <- createWorkbook()
  addWorksheet(wb = book, sheetName = "plots", gridLines = FALSE)
  writeDataTable(wb = book, sheet = "plots", x = extract_DBtable("plots"), rowNames = TRUE, tableStyle = "none", withFilter=F)
  addWorksheet(wb = book, sheetName = "sites", gridLines = FALSE)
  writeDataTable(wb = book, sheet = "sites", x = extract_DBtable("sites"), rowNames = TRUE, tableStyle = "none", withFilter=F)
  addWorksheet(wb = book, sheetName = "agents", gridLines = FALSE)
  writeDataTable(wb = book, sheet = "agents", x = extract_DBtable("agents"), rowNames = TRUE, tableStyle = "none", withFilter=F)
  addWorksheet(wb = book, sheetName = "species", gridLines = FALSE)
  writeDataTable(wb = book, sheet = "species", x = extract_DBtable("species"), rowNames = TRUE, tableStyle = "none", withFilter=F)
  addWorksheet(wb = book, sheetName = "lfmc", gridLines = FALSE)
  writeDataTable(wb = book, sheet = "lfmc", x = extract_DBtable("lfmc"), rowNames = TRUE, tableStyle = "none", withFilter=F)
  addWorksheet(wb = book, sheetName = "soilmoisture", gridLines = FALSE)
  writeDataTable(wb = book, sheet = "soilmoisture", x = extract_DBtable("soilmoisture"), rowNames = TRUE, tableStyle = "none", withFilter=F)
  saveWorkbook(book, xlsxfile, overwrite = overwrite)
}
