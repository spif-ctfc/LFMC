#' Populate LFMC data
#'
#' Fills LFMC records from a data frame
#'
#' @param lfmc The data frame from which LFMC records are to be read.
#' @param dateIni String to indicate the earliest date to import (using format in \code{dateFormat}).
#' @param dateFin String to indicate the latest date to import (using format in \code{dateFormat}).
#' @param dateFormat String with date format (see \code{\link{as.Date}}).
#' @param varmapping Named vector of variable mappings (see details).
#' @param overwrite Whether or not to overwrite existing records. LFMC records are uniquely identified
#' with field 'SampleCode'.
#' @param outlierSearch Logical to indicate if outliers search routine is run
#'
#' @details Mapping should be provided at least for 'Date', 'SamplingSiteCode', 'SampleCode' and 'SpeciesCode'.
#' Variables 'LFMC' and 'LeafStemRatio' are by default calculated from imported values. Variable for 'Date' can be of class \code{\link{Date}} or
#' a string. In the latter case date strings are expected to be in \code{dateFormat}.
#'
#' Records with missing 'SampleCode' are discarded. Records already existing in the database (i.e. records
#' corresponding to existing 'SampleCode' values) are also discarded, unless \code{overwrite = TRUE}. This
#' allows adding only new records even if the input data frame contains records already existing in the database.
#'
#' @examples
#' \dontrun{
#' # Initiate data base
#'
#' initDB("../lfmc", overwrite = T)
#'
#' # Parse records from file "2019.xlsx"
#'
#' lfmc = openxlsx::read.xlsx("../LFMC_spif/2019.xlsx")
#' lfmc$DATA = openxlsx::convertToDate(lfmc$DATA)
#' populate_lfmc(lfmc)
#'
#'
#' # Parse records from another file using another (identity) mapping
#' varmapping2 = c("Date" = "DATA", "SamplingSiteCode"  = "CODI_PARCELA",
#'                 "SampleCode" = "NUM_MOSTRA", "SpeciesCode" = "CODI_ESPECIE",
#'                 "FreshMass" = "PES_FRESC", "DryMass" = "PES_SEC",
#'                 "DryStem" = "PES_TIGES", "DryLeaf" = "PES_FULLES",
#'                 "Notes" = "Observacions")
#' lfmc2 = openxlsx::read.xlsx("../LFMC_spif/2019.xlsx")
#' lfmc2$Date = openxlsx::convertToDate(lfmc2$Date)
#' populateLFMC(lfmc2, varmapping = varmapping2)
#' }
#'
#' @export
#'
populateLFMC <- function(lfmc, dateIni = NULL, dateFin = NULL, dateFormat = "%Y-%m-%d",
                         varmapping = c("Date" = "DATA",
                                        "SamplingSiteCode" = "CODI_PARCELA",
                                        "SpeciesCode" = "CODI_ESPECIE",
                                        "SampleCode" = "NUM_MOSTRA",
                                        "FreshMass" = "PES_FRESC",
                                        "DryMass" = "PES_SEC",
                                        "DryStem" = "PES_TIGES",
                                        "DryLeaf" = "PES_FULLES",
                                        "PhenologyCode" = "fenologia",
                                        "Notes" = "Observacions"),
                         overwrite = FALSE, outlierSearch = F) {

  if (!("Date" %in% names(varmapping))) stop ("Please supply mapping for 'Date'")
  if (!("SamplingSiteCode" %in% names(varmapping))) stop ("Please supply mapping for 'SamplingSiteCode'")
  if (!("SampleCode" %in% names(varmapping))) stop ("Please supply mapping for 'SampleCode'")
  if (!("SpeciesCode" %in% names(varmapping))) stop ("Please supply mapping for 'SpeciesCode'")

  if(!varmapping[["Date"]] %in% names(lfmc)) stop (paste0("Date variable '", varmapping[["Date"]], "' not found in input data frame. Check mapping."))

  dates = lfmc[[varmapping[["Date"]]]]
  if (class(dates) != "Date") {
    dates = as.Date(dates, format = dateFormat, origin = "1970-01-01")
  }
  # Date range to be imported
  sel = rep(T, nrow(lfmc))
  if (!is.null(dateIni)) {
    dateIni = as.Date(dateIni, format = dateFormat, origin = "1970-01-01")
    sel = sel & dates >= dateIni
  }
  if (!is.null(dateFin)) {
    dateFin = as.Date(dateFin, format = dateFormat, origin = "1970-01-01")
    sel = sel & dates <= dateFin
  }
  dates = dates[sel]
  lfmc = lfmc[sel,]

  n = nrow(lfmc)
  lfmc_df = data.frame(matrix(nrow = n, ncol = 13))
  names(lfmc_df) <- c("SampleCode", "SiteSpCode", "Date",
                      "FreshMass", "DryMass", "LFMC",
                      "DryStem", "DryLeaf", "LeafStemRatio",
                      "ManualOutlier", "AdditiveOutlier",
                      "PhenologyCode", "Notes")

  lfmc_df[["Date"]] = dates
  # Generate Site-Species Code
  if (varmapping[["SamplingSiteCode"]] %in% names(lfmc) & varmapping[["SpeciesCode"]] %in% names(lfmc)) {
    lfmc_df[["SiteSpCode"]] <- paste0('s', lfmc[[varmapping[['SamplingSiteCode']]]],
                                      'sp', lfmc[[varmapping[['SpeciesCode']]]])
  } else if (!varmapping[["SamplingSiteCode"]] %in% names(lfmc)) {
    stop (paste0("SamplingSiteCode variable '", varmapping[["SamplingSiteCode"]], "' not found in input data frame. Check mapping."))
  } else stop (paste0("SpeciesCode variable '", varmapping[["SpeciesCode"]], "' not found in input data frame. Check mapping."))

  # Mapping the rest of variables
  if(varmapping[["SampleCode"]] %in% names(lfmc)) lfmc_df[["SampleCode"]] = lfmc[[varmapping[["SampleCode"]]]]
  else stop(paste0("SampleCode variable '", varmapping[["SampleCode"]], "' not found in input data frame. Check mapping."))

  if("FreshMass" %in% names(varmapping)) {
    if(varmapping[["FreshMass"]] %in% names(lfmc)) lfmc_df[["FreshMass"]] = as.numeric(lfmc[[varmapping[["FreshMass"]]]])
    else stop(paste0("FreshMass variable '", varmapping[["FreshMass"]], "' not found in input data frame. Check mapping."))
  }

  if("DryMass" %in% names(varmapping)) {
    if(varmapping[["DryMass"]] %in% names(lfmc)) lfmc_df[["DryMass"]] = as.numeric(lfmc[[varmapping[["DryMass"]]]])
    else stop(paste0("DryMass variable '", varmapping[["DryMass"]], "' not found in input data frame. Check mapping."))
  }

  lfmc_df[["LFMC"]] = 100 * (lfmc_df[["FreshMass"]] - lfmc_df[["DryMass"]]) / lfmc_df[["DryMass"]]

  if("DryStem" %in% names(varmapping)) {
    if(varmapping[["DryStem"]] %in% names(lfmc)) lfmc_df[["DryStem"]] = as.numeric(lfmc[[varmapping[["DryStem"]]]])
    else stop(paste0("DryStem variable '", varmapping[["DryStem"]], "' not found in input data frame. Check mapping."))
  }

  lfmc_df[["LeafStemRatio"]] = lfmc_df[["DryLeaf"]] / lfmc_df[["DryStem"]]

  if("PhenologyCode" %in% names(varmapping)) {
    if(varmapping[["PhenologyCode"]] %in% names(lfmc)) lfmc_df[["PhenologyCode"]] = lfmc[[varmapping[["PhenologyCode"]]]]
    else stop(paste0("PhenologyCode variable '", varmapping[["PhenologyCode"]], "' not found in input data frame. Check mapping."))
  }

  if("Notes" %in% names(varmapping)) {
    if(varmapping[["Notes"]] %in% names(lfmc)) lfmc_df[["Notes"]] = lfmc[[varmapping[["Notes"]]]]
    else stop(paste0("Notes variable '", varmapping[["Notes"]], "' not found in input data frame. Check mapping."))
  }

  #remove records with missing SampleCode
  missing_code = is.na(lfmc_df$SampleCode)
  if(sum(missing_code) > 0) {
    cat(paste0(sum(missing_code), " records discarded because of missing 'SampleCode' values.\n"))
    lfmc_df = lfmc_df[!missing_code, ]
  }

  # Connect to database
  if (!exists("lfmcdbfile", envir = globalenv())) stop ("Use setDBpath() to load database")
  lfmc_db <- DBI::dbConnect(RSQLite::SQLite(), get("lfmcdbfile"))
  lfmc_table = DBI::dbReadTable(lfmc_db, "lfmc")
  # Records existing in the database
  existing = lfmc_df$SampleCode %in% lfmc_table$SampleCode
  lfmc_new = lfmc_df[!existing, ]
  lfmc_existing = lfmc_df[existing, ]

  if(!overwrite) {
    ## Discard already existing samples
    if(sum(existing) > 0) {
      cat(paste0(sum(existing), " records discarded because of existing 'SampleCode' values.\n"))
      }
  } else {
    if(sum(existing) > 0) {
      cat(paste0(sum(existing), " existing records will be replaced.\n"))
    }
    ## Replace already existing samples
    scodes = lfmc_existing$SampleCode
    for(i in 1:length(scodes)) {
      sel = lfmc_table$SampleCode == scodes[i]
      sel[is.na(sel)] = FALSE
      lfmc_table[sel, ] = lfmc_existing[i, ]
    }
    ## Replace table
    dbRemoveTable(lfmc_db, "lfmc")
    dbWriteTable(lfmc_db, "lfmc", lfmc_table)
  }
  nnew = dbAppendTable(lfmc_db, "lfmc", lfmc_new)
  cat(paste0(nnew, " new LFMC records added to database.\n"))

  if(outlierSearch) {
    outlierSearch(lfmc_db)
  }

  dbDisconnect(lfmc_db)
}
