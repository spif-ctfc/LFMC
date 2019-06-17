#' Reads LFMC data from an excel file
#'
#' @param lfmc_xlsx
#' @param dateIni
#' @param dateFin
#' @param dateFormat
#' @param varmapping
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
parse_LFMC<-function(lfmc_xlsx, dateIni = NULL, dateFin = NULL, dateFormat = "%d/%m/%Y",
                     varmapping = c("Date" = "DATA",
                                    "SiteCode"  = "CODI_PARCELA",
                                    "SpeciesCAT" = "ESPECIE",
                                    "SampleCode" = "NUM_MOSTRA",
                                    "FreshMass" = "PES_FRESC",
                                    "DryMass" = "PES_SEC",
                                    "LFMC" = "HUMITAT",
                                    "DryStem" = "PES_TIGES",
                                    "DryLeaf" = "PES_FULLES",
                                    "Notes" = "Observacions"),
                     overwrite = FALSE) {

  lfmc = readxl::read_xlsx(lfmc_xlsx)
  dates = as.Date(lfmc[[varmapping[["Date"]]]], format = dateFormat)
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
                Date = rep(NA, n), SampleCode = rep(NA, n),
                FreshMass = rep(NA, n),
                DryMass = rep(NA, n), DryStem = rep(NA, n), DryLeaf = rep(NA, n),
                LFMC = rep(NA, n),
                PhenologyCode = rep(NA, n), PhenologySystem = rep(NA, n))


  ## Mapping
  if(varmapping[["Date"]] %in% names(lfmc)) lfmc_df[["Date"]] = dates

  if("SiteCode" %in% names(varmapping)) {
    if(varmapping[["SiteCode"]] %in% names(lfmc)) lfmc_df[["SiteCode"]] = lfmc[[varmapping[["SiteCode"]]]]
  }
  if("AgentCode" %in% names(varmapping)) {
    if(varmapping[["AgentCode"]] %in% names(lfmc)) lfmc_df[["AgentCode"]] = lfmc[[varmapping[["AgentCode"]]]]
  }
  spCode = rep(NA, n)
  if(("SpeciesCode" %in% names(varmapping)) && (varmapping[["SpeciesCode"]] %in% names(lfmc))) {
    spCode = lfmc[[varmapping[["SpeciesCode"]]]]
  } else if(("SpeciesCAT" %in% names(varmapping)) && (varmapping[["SpeciesCAT"]] %in% names(lfmc))) {
    spCAT = lfmc[[varmapping[["SpeciesCAT"]]]]
    spTh = extract_DBtable("species")
    for(i in 1:length(spCAT)) spCode[i] = spTh$SpeciesCode[spTh$SpeciesCAT==spCAT[i]]
  }
  lfmc_df[["SpeciesCode"]] =spCode

  if("SampleCode" %in% names(varmapping)) {
    if(varmapping[["SampleCode"]] %in% names(lfmc)) lfmc_df[["SampleCode"]] = lfmc[[varmapping[["SampleCode"]]]]
  }

  if("FreshMass" %in% names(varmapping)) {
    if(varmapping[["FreshMass"]] %in% names(lfmc)) lfmc_df[["FreshMass"]] = as.numeric(lfmc[[varmapping[["FreshMass"]]]])
  }

  if("DryMass" %in% names(varmapping)) {
    if(varmapping[["DryMass"]] %in% names(lfmc)) lfmc_df[["DryMass"]] = as.numeric(lfmc[[varmapping[["DryMass"]]]])
  }

  if("DryStem" %in% names(varmapping)) {
    if(varmapping[["DryStem"]] %in% names(lfmc)) lfmc_df[["DryStem"]] = as.numeric(lfmc[[varmapping[["DryStem"]]]])
  }

  if("DryLeaf" %in% names(varmapping)) {
    if(varmapping[["DryLeaf"]] %in% names(lfmc)) lfmc_df[["DryLeaf"]] = as.numeric(lfmc[[varmapping[["DryLeaf"]]]])
  }

  if("LFMC" %in% names(varmapping)) {
    if(varmapping[["LFMC"]] %in% names(lfmc)) lfmc_df[["LFMC"]] = as.numeric(lfmc[[varmapping[["LFMC"]]]])
  }

  if("Notes" %in% names(varmapping)) {
    if(varmapping[["Notes"]] %in% names(lfmc)) lfmc_df[["Notes"]] = lfmc[[varmapping[["Notes"]]]]
  }

  #remove records with missing SampleCode
  missing_code = is.na(lfmc_df$SampleCode)
  if(sum(missing_code)>0) {
    cat(paste0(sum(missing_code), " records discarded because of missing SampleCode values.\n"))
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
  cat(paste0(nnew, " new LFMC records added.\n"))

  dbDisconnect(lfmc_db)
}
