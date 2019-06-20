#' Parse LFMC data
#'
#' Parses LFMC records from a data frame
#'
#' @param lfmc The data frame from which LFMC records are to be read
#' @param dateIni String to indicate the earliest date to import (using format in \code{dateFormat})
#' @param dateFin String to indicate the latest date to import (using format in \code{dateFormat})
#' @param dateFormat String with date format (see \code{\link{as.Date}}).
#' @param varmapping Named vector of variable mappings (see details).
#' @param overwrite Whether or not to overwrite existing records. LFMC records are uniquely identified
#' with field 'SampleCode'.
#'
#' @details Mapping should be provided for at least for 'Date', 'SiteCode', 'SampleCode' and either 'SpeciesCode' or 'SpeciesCAT'.
#' If mapping for 'SpeciesCode' is supplied, then 'SpeciesCAT' is not used.
#' Variables 'LFMC' and 'LeafStemRatio' are by default calculated from imported values, but can
#' be also mapped (data integrity is then user's responsibility).
#'
#' @examples
#' \dontrun{
#' # Init DB using excel file with thesaurus tables (agents, species, plots and sites)
#'
#' init_DB("../lfmc", thesaurus_xlsx = "../LFMC_tesaures.xlsx", overwrite = T)
#'
#' # Parse records from file "2019.xlsx"
#'
#' lfmc = openxlsx::read.xlsx("../LFMC_spif/2019.xlsx")
#' lfmc$DATA = openxlsx::convertToDate(lfmc$DATA)
#' parse_LFMC(lfmc)
#'
#'
#' # Parse records from another file using another (identity) mapping
#' varmapping2 = c("Date" = "Date", "SiteCode"  = "SiteCode",
#'                 "SpeciesCode" = "SpeciesCode", "SampleCode" = "SampleCode",
#'                 "FreshMass" = "FreshMass", "DryMass" = "DryMass",
#'                 "DryStem" = "DryStem", "DryLeaf" = "DryLeaf")
#' lfmc2 = openxlsx::read.xlsx("../LFMC_spif/LFMC_raw_table_Miquel.xlsx")
#' lfmc2$Date = openxlsx::convertToDate(lfmc2$Date)
#' parse_LFMC(lfmc2, varmapping = varmapping2)
#' }
#'
parse_LFMC<-function(lfmc, dateIni = NULL, dateFin = NULL, dateFormat = "%Y-%m-%d",
                     varmapping = c("Date" = "DATA",
                                    "SiteCode"  = "CODI_PARCELA",
                                    "SpeciesCAT" = "ESPECIE",
                                    "SampleCode" = "NUM_MOSTRA",
                                    "FreshMass" = "PES_FRESC",
                                    "DryMass" = "PES_SEC",
                                    "DryStem" = "PES_TIGES",
                                    "DryLeaf" = "PES_FULLES",
                                    "Notes" = "Observacions"),
                     overwrite = FALSE) {

  if(!("Date" %in% names(varmapping))) stop("Please supply mapping for 'Date'")

  dates = lfmc[[varmapping[["Date"]]]]
  if(class(dates)!="Date") {
    dates = as.Date(dates, format = dateFormat)
  }
  sel = rep(T, nrow(lfmc))
  if(!is.null(dateIni)) {
    dateIni = as.Date(dateIni, format = dateFormat)
    sel = sel & dates>=dateIni
  }
  if(!is.null(dateFin)) {
    dateFin = as.Date(dateFin, format = dateFormat)
    sel = sel & dates<=dateFin
  }
  dates = dates[sel]
  lfmc = lfmc[sel,]

  n = nrow(lfmc)

  lfmc_df = data.frame(SiteCode = rep(NA,n), AgentCode = rep(NA, n), SpeciesCode = rep(NA, n),
                Date = dates, SampleCode = rep(NA, n),
                FreshMass = rep(NA, n),
                DryMass = rep(NA, n),
                LFMC = rep(NA, n),
                DryStem = rep(NA, n), DryLeaf = rep(NA, n),
                LeafStemRatio = rep(NA, n),
                PhenologyCode = rep(NA, n), PhenologySystem = rep(NA, n))



  if(!("SiteCode" %in% names(varmapping))) stop("Please supply mapping for 'SiteCode'")
  if(!("SampleCode" %in% names(varmapping))) stop("Please supply mapping for 'SampleCode'")

  ## Mapping the rest of variables

  if(varmapping[["SiteCode"]] %in% names(lfmc)) lfmc_df[["SiteCode"]] = lfmc[[varmapping[["SiteCode"]]]]
  else stop(paste0("SiteCode variable '", varmapping[["SiteCode"]], "' not found in input data frame. Check mapping."))

  if(varmapping[["SampleCode"]] %in% names(lfmc)) lfmc_df[["SampleCode"]] = lfmc[[varmapping[["SampleCode"]]]]
  else stop(paste0("SampleCode variable '", varmapping[["SampleCode"]], "' not found in input data frame. Check mapping."))

  if("AgentCode" %in% names(varmapping)) {
    if(varmapping[["AgentCode"]] %in% names(lfmc)) lfmc_df[["AgentCode"]] = lfmc[[varmapping[["AgentCode"]]]]
    else stop(paste0("AgentCode variable '", varmapping[["AgentCode"]], "' not found in input data frame. Check mapping."))

  }
  spCode = rep(NA, n)
  if("SpeciesCode" %in% names(varmapping)) {
    if(varmapping[["SpeciesCode"]] %in% names(lfmc)) {
      spCode = lfmc[[varmapping[["SpeciesCode"]]]]
    } else {
      stop(paste0("SpeciesCode variable '", varmapping[["SpeciesCode"]], "' not found in input data frame. Check mapping."))
    }
  } else if("SpeciesCAT" %in% names(varmapping)) {
    if(varmapping[["SpeciesCAT"]] %in% names(lfmc)) {
      spCAT = lfmc[[varmapping[["SpeciesCAT"]]]]
      spTh = extract_DBtable("species")
      for(i in 1:length(spCAT)) spCode[i] = spTh$SpeciesCode[spTh$SpeciesCAT==spCAT[i]]
    } else {
      stop(paste0("SpeciesCAT variable '", varmapping[["SpeciesCAT"]], "' not found in input data frame. Check mapping."))
    }
  } else {
    stop("Please supply mapping for 'SpeciesCode' or 'SpeciesCAT'")
  }
  lfmc_df[["SpeciesCode"]] =spCode

  if("FreshMass" %in% names(varmapping)) {
    if(varmapping[["FreshMass"]] %in% names(lfmc)) lfmc_df[["FreshMass"]] = as.numeric(lfmc[[varmapping[["FreshMass"]]]])
    else stop(paste0("FreshMass variable '", varmapping[["FreshMass"]], "' not found in input data frame. Check mapping."))
  }

  if("DryMass" %in% names(varmapping)) {
    if(varmapping[["DryMass"]] %in% names(lfmc)) lfmc_df[["DryMass"]] = as.numeric(lfmc[[varmapping[["DryMass"]]]])
    else stop(paste0("DryMass variable '", varmapping[["DryMass"]], "' not found in input data frame. Check mapping."))
  }


  if("LFMC" %in% names(varmapping)) {
    if(varmapping[["LFMC"]] %in% names(lfmc)) lfmc_df[["LFMC"]] = as.numeric(lfmc[[varmapping[["LFMC"]]]])
    else stop(paste0("LFMC variable '", varmapping[["LFMC"]], "' not found in input data frame. Check mapping."))
  } else {
    lfmc_df[["LFMC"]] = 100*(lfmc_df[["FreshMass"]] - lfmc_df[["DryMass"]])/lfmc_df[["DryMass"]]
  }

  if("DryStem" %in% names(varmapping)) {
    if(varmapping[["DryStem"]] %in% names(lfmc)) lfmc_df[["DryStem"]] = as.numeric(lfmc[[varmapping[["DryStem"]]]])
    else stop(paste0("DryStem variable '", varmapping[["DryStem"]], "' not found in input data frame. Check mapping."))
  }

  if("DryLeaf" %in% names(varmapping)) {
    if(varmapping[["DryLeaf"]] %in% names(lfmc)) lfmc_df[["DryLeaf"]] = as.numeric(lfmc[[varmapping[["DryLeaf"]]]])
    else stop(paste0("DryLeaf variable '", varmapping[["DryLeaf"]], "' not found in input data frame. Check mapping."))
  }

  if("LeafStemRatio" %in% names(varmapping)) {
    if(varmapping[["LeafStemRatio"]] %in% names(lfmc)) lfmc_df[["LeafStemRatio"]] = as.numeric(lfmc[[varmapping[["LeafStemRatio"]]]])
    else stop(paste0("LeafStemRatio variable '", varmapping[["LeafStemRatio"]], "' not found in input data frame. Check mapping."))
  } else {
    lfmc_df[["LeafStemRatio"]] = lfmc_df[["DryLeaf"]]/lfmc_df[["DryStem"]]
  }

  if("Notes" %in% names(varmapping)) {
    if(varmapping[["Notes"]] %in% names(lfmc)) lfmc_df[["Notes"]] = lfmc[[varmapping[["Notes"]]]]
    else stop(paste0("Notes variable '", varmapping[["Notes"]], "' not found in input data frame. Check mapping."))
  }

  #remove records with missing SampleCode
  missing_code = is.na(lfmc_df$SampleCode)
  if(sum(missing_code)>0) {
    cat(paste0(sum(missing_code), " records discarded because of missing 'SampleCode' values.\n"))
    lfmc_df = lfmc_df[!missing_code,]
  }


  lfmc_db <- DBI::dbConnect(RSQLite::SQLite(), get("lfmcdbfile"))
  lfmc_table = dbReadTable(lfmc_db, "lfmc")
  existing = lfmc_df$SampleCode %in% lfmc_table$SampleCode
  lfmc_new = lfmc_df[!existing,]
  lfmc_existing = lfmc_df[existing,]
  if(!overwrite) {
    ## Remove already existing samples
    if(sum(existing)>0) {
      cat(paste0(sum(existing), " records discarded because of existing SampleCode values.\n"))
    }
  } else {
    if(sum(existing)>0) {
      cat(paste0(sum(existing), " existing records will be replaced.\n"))
    }
    scodes = lfmc_existing$SampleCode
    for(i in 1:length(scodes)) {
      sel = lfmc_table$SampleCode==scodes[i]
      sel[is.na(sel)]= FALSE
      lfmc_table[sel,] = lfmc_existing[i,]
    }
    ## Replace table
    dbRemoveTable(lfmc_db, "lfmc")
    dbWriteTable(lfmc_db, "lfmc", lfmc_table)
  }
  nnew = dbAppendTable(lfmc_db, "lfmc", lfmc_new)
  cat(paste0(nnew, " new LFMC records added to database.\n"))

  dbDisconnect(lfmc_db)
}
