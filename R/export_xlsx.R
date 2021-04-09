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
#'   export_xlsx("../lfmc_backup.xlsx", overwrite = T)
#' }
#'
export_xlsx <- function(xlsxfile, overwrite = FALSE) {
  if(!endsWith(xlsxfile, ".xlsx")) xlsxfile = paste0(xlsxfile, ".xlsx")

  book <- createWorkbook()

  phenology = extractDBtable("phenology")
  if(nrow(phenology) != 0) {
    addWorksheet(
      wb = book,
      sheetName = "phenology",
      gridLines = FALSE
      )
    writeDataTable(
      wb = book,
      sheet = "phenology",
      x = phenology,
      rowNames = TRUE,
      tableStyle = "none",
      withFilter = F
      )
  }

  lfmc = extractDBtable("lfmc")
  if(nrow(lfmc) != 0) {
    addWorksheet(
      wb = book,
      sheetName = "lfmc",
      gridLines = FALSE
    )
    writeDataTable(
      wb = book,
      sheet = "lfmc",
      x = lfmc,
      rowNames = TRUE,
      tableStyle = "none",
      withFilter = F
    )
  }

  species = extractDBtable("species")
  if(nrow(species) != 0) {
    addWorksheet(
      wb = book,
      sheetName = "species",
      gridLines = FALSE
    )
    writeDataTable(
      wb = book,
      sheet = "species",
      x = species,
      rowNames = TRUE,
      tableStyle = "none",
      withFilter = F
    )
  }

  sites = extractDBtable("sites")
  if(nrow(sites) != 0) {
    addWorksheet(
      wb = book,
      sheetName = "sites",
      gridLines = FALSE
    )
    writeDataTable(
      wb = book,
      sheet = "sites",
      x = sites,
      rowNames = TRUE,
      tableStyle = "none",
      withFilter = F
    )
  }

  tdr = extractDBtable("tdr_sensor")
  if(nrow(tdr) != 0) {
    addWorksheet(
      wb = book,
      sheetName = "tdr",
      gridLines = FALSE
    )
    writeDataTable(
      wb = book,
      sheet = "tdr",
      x = tdr,
      rowNames = TRUE,
      tableStyle = "none",
      withFilter = F
    )
  }

  soil = extractDBtable("soil_measurements")
  if(nrow(soil) != 0) {
    addWorksheet(
      wb = book,
      sheetName = "soil",
      gridLines = FALSE
    )
    writeDataTable(
      wb = book,
      sheet = "soil",
      x = soil,
      rowNames = TRUE,
      tableStyle = "none",
      withFilter = F
    )
  }

  saveWorkbook(book, xlsxfile, overwrite = overwrite)
}
