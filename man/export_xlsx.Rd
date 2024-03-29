% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_xlsx.R
\name{export_xlsx}
\alias{export_xlsx}
\title{Exports LFMC data base}
\usage{
export_xlsx(xlsxfile, overwrite = FALSE)
}
\arguments{
\item{xlsxfile}{A string with the path to the destination file (*.xlsx)}

\item{overwrite}{A boolean flag to indicate that file should be overwritten}
}
\description{
Exports whole LFMC data base into an excel (*.xlsx) file
}
\examples{

\dontrun{
  export_xlsx("../lfmc_backup.xlsx", overwrite = T)
}

}
