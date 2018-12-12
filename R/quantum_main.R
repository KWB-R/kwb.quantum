# getQuantumVariables ----------------------------------------------------------
getQuantumVariables <- function # Quantum variables with ID and name
### Available Quantum variables with ID and name in a list or data frame
(
  return.as.list = TRUE
  ### if TRUE (default) the Quantum variable IDs and names are returned in a
  ### list, otherwise in a data frame.
)
{
  variables.csv <- system.file(
    "extdata", "Quantum_Messstellen_IDs.csv", package = "kwb.quantum"
  )
  
  variables <- read.csv2(variables.csv, blank.lines.skip = TRUE)
  
  if (return.as.list) {
    
    variableList <- as.list(variables$VariableID)
    
    names(variableList) <- paste0(
      "v", gsub(":", "", hsSubstSpecChars(variables$VariableName))
    )
    
    variables <- variableList
  }
  
  variables
  ### list (if \emph{return.as.list} = TRUE) of Quantum variable IDs with the
  ### list names being R-compatible variants (starting with a letter, no spaces)
  ### of the Quantum variable names or data frame (if \emph{return.as.list} =
  ### FALSE) with the Quantum variable IDs and (original) names being in columns
  ### \emph{VariableID} and \emph{VariableName}, respectively.
}

# exportToDataFrame ------------------------------------------------------------
exportToDataFrame <- function # export Quantum data to data frame
### export Quantum data to data frame
(
  variableIDs,
  ### vector of Quantum variable IDs. For a list of available IDs, see
  ### \code{\link{getQuantumVariables}}
  from,
  ### first day as character of format "yyyy-mm-dd"
  to, 
  ### one day after the last day as character of format "yyyy-mm-dd"
  ...
  ### arguments passed to exportToCsv
)
{
  reserved <- c("sep", "dec", "all.in.one")
  args <- list(...)
  
  if (any(reserved %in% args)) {
    stop("You are not supposed to set any of these arguments:", 
         commaCollapsed(reserved))
  }
  
  csv.file <- exportToCsv(variableIDs = variableIDs, from = from, to = to, ...)
  
  stopifnot(length(csv.file) == 1)
  
  cat("*** Reading downloaded data from", csv.file, "... ")
  quantum.data <- read.csv(csv.file, stringsAsFactors = FALSE)
  cat("ok.\n")
  
  quantum.data[[1]] <- to.GMT.plus.1(quantum.data[[1]])
    
  names(quantum.data) <- hsSubstSpecChars(names(quantum.data))
  
  quantum.data
}

# downloadDataOfQuarter --------------------------------------------------------
downloadDataOfQuarter <- function # download data of one quarter
### download data of one quarter
(
  year, 
  ### year number (numeric)
  quarter,
  ### quarter number (numeric), one out of 1:4
  variableIDs, 
  ### Quantum variable IDs, see \code{\link{getQuantumVariables}}
  target.dir, 
  ### full path to target directory
  last.day.in.file.name = FALSE,
  ### if TRUE and \emph{year} and \emph{quarter} are the current year and 
  ### quarter, the current day is used as the end day and thus will appear in 
  ### the file name. Otherwise the first day of the next quarter will be used as
  ### the end day so that the filename will not change when this function is
  ### called multiple times within the same year and quarter. Default: FALSE
  ...
  ### arguments passed to \code{\link{exportToCsv}}
)
{  
  dateRange <- .dateLimitsQuarter(year, quarter, last.day.in.file.name)
  
  exportToCsv(variableIDs, dateRange[1], dateRange[2],
              target.dir = target.dir, ...)
}

# .dateLimitsQuarter -----------------------------------------------------------
.dateLimitsQuarter <- function(year, quarter, last.day.in.file.name = FALSE)
{
  # Is the current quarter (in the current year) requested?
  current <- (year == currentYear() && quarter == currentQuarter())
  
  quarterLimits <- .quarterLimits(year, quarter)
  
  from <- quarterLimits$from
  
  to <- if (current && last.day.in.file.name) {
    format(Sys.time() + 86400, format = "%Y-%m-%d")
  } else {
    quarterLimits$to
  }
  
  c(from = from, to = to)
  ### named vector with two elements: \emph{from} and \emph{to}
}

# .quarterLimits ---------------------------------------------------------------
.quarterLimits <- function(year, quarter)
{
  format <- "%04d-%02d-01"
  
  nextQuarter <- ifelse(quarter == 4, 1, quarter + 1)  
  
  list(
    from = sprintf(
      format,     
      year, 
      .firstMonthOfQuarter(quarter)
    ),
    to = sprintf(
      format, 
      year + (quarter == 4), 
      .firstMonthOfQuarter(nextQuarter)
    )
  )  
}

# .firstMonthOfQuarter ---------------------------------------------------------
.firstMonthOfQuarter <- function(quarter)
{
  3 * (quarter - 1) + 1
}

# downloadDataOfYear -----------------------------------------------------------
downloadDataOfYear <- function # download data of one year
### # download data of one year
(
  year, 
  variableIDs, 
  target.dir, 
  last.day.in.file.name = FALSE,
  ### if TRUE and \emph{year} is the current year, the current day is used as 
  ### the end day and thus will appear in the file name. Otherwise January 1 of
  ### the next year will be used as the end day so that the filename will not
  ### change when this function is called multiple times within the same year.
  ### Default: FALSE
  ...
  ### arguments passed to \code{\link{exportToCsv}}
)
{
  dateRange <- .dateLimitsYear(year, last.day.in.file.name)
  
  # download data for complete year 2014 (excluding first day of 2015)
  exportToCsv(variableIDs, dateRange[1], dateRange[2], 
              target.dir = target.dir, ...)
}

# .dateLimitsYear --------------------------------------------------------------
.dateLimitsYear <- function(year, last.day.in.file.name = FALSE)
{
  from <- paste0(year, "-01-01")
  
  to <- if (year == currentYear() && last.day.in.file.name) {
    format(Sys.time(), format = "%Y-%m-%d")
  }
  else {
    paste0(year + 1, "-01-01")
  }  
  
  c(from = from, to = to)
  ### named vector with two elements: \emph{from} and \emph{to}
}

# exportToCsv ------------------------------------------------------------------
exportToCsv <- function # export Quantum variables to CSV file(s)
### export Quantum variables to CSV file(s)
(  
  variableIDs,
  ### vector of Quantum variable IDs. For a list of available IDs, see
  ### \code{\link{getQuantumVariables}}
  from,
  ### first day as character of format "yyyy-mm-dd"
  to, 
  ### one day after the last day as character of format "yyyy-mm-dd"
  status = NA,
  ### vector of stati (1 = QH:neu, 2 = QH:bestaetigt, 3 = QH:korrigiert, 5 =
  ### Hy:neu). Default: NA (data of all stati are exported)
  all.in.one = TRUE,
  ### if TRUE (default) all data are exported into one file with the data
  ### series in different columns.  
  target.dir = NULL,
  sep = ",",
  ### column separator
  dec = ".",
  ### decimal character
  user = "Rouault"
  ### user name used for login
)
{  
  password <- .getPassword(user)
  
  if (is.na(password)) {
    stop("Please run setPassword() first!")
  }
  
  sessionID <- as.character(login("Berlin - Senat", user, password))
  
  on.exit(logout(sessionID))
  
  zipfile <- exportToZip(
    sessionID, 
    variableIDs = variableIDs, 
    from = from, 
    to = to, 
    status = status,
    all.in.one = all.in.one,
    sep = sep,
    dec = dec
  )
  
  if (is.null(target.dir)) {
    target.dir <- createDirAndReturnPath(
      file.path(dirname(zipfile), .removeFileExtension(basename(zipfile)))
    )
  }
  
  # Get names of zipped files
  files <- unzip(zipfile, list = TRUE)$Name

  # Unzip files to target folder
  unzip(zipfile, exdir = target.dir)  
  
  file.path(target.dir, files)
  ### full paths of downloaded CSV file(s)
}

# login ------------------------------------------------------------------------
login <- function # login to https://wis.quantumhydro.de/wis/
### login to Quantum Hydrometrie web portal at https://wis.quantumhydro.de/wis/
(
  company, 
  ### name of company
  user, 
  ### user name
  password
  ### password
)
{
  curl <- getCurlHandle(ssl.verifypeer = FALSE, followlocation = TRUE)
  
  cat("*** Logging in... ")
  html <- postForm(
    uri = "https://wis.quantumhydro.de/wis/?show=login", 
    style = "post",  
    .params = list(
      "mandant" = company, 
      "username" = user, 
      "password" = password,
      "usecookie" = 0,
      "prefLang" = "de",
      "version" = "regular"
    ),
    curl = curl
  )
  cat("ok.\n")
  
  pattern <- "^.*WIS_SESSID_D6BAF20B=([a-z0-9]+)&amp;.*$"
  
  if (!grepl(pattern, html)) {
    stop("Login failed (no session ID found in response)!\n")
  }
  
  sub(pattern, "\\1", html)  
  ### On successful login, the session ID is returned
}

# logout -----------------------------------------------------------------------
logout <- function # logout from Quantum Hydrometrie session
### # logout from Quantum Hydrometrie session
(
  sessionID
)
{
  logout.url <- paste0(
    "https://wis.quantumhydro.de/wis/?usecookie=0&WIS_SESSID_D6BAF20B=", 
    sessionID, "&show=login&action=logout"
  )
  
  cat("*** Logging out... ")
  result <- getURI(url = logout.url, ssl.verifypeer = FALSE)
  cat("ok.\n")
}

# exportToZip ------------------------------------------------------------------
exportToZip <- function # export Quantum Hydrometrie data to zip file
### export Quantum Hydrometrie data to zip file
(
  sessionID,
  ### session ID as returned by \code{\link{login}}
  variableIDs,
  ### vector of Quantum variable IDs. For a list of available IDs, see
  ### \code{\link{getQuantumVariables}}
  from,
  ### first day as character of format "yyyy-mm-dd"
  to, 
  ### one day after the last day as character of format "yyyy-mm-dd"
  status = NA,
  ### vector of stati (1 = QH:neu, 2 = QH:bestaetigt, 3 = QH:korrigiert, 5 =
  ### Hy:neu). Default: NA (data of all stati are exported)  
  all.in.one = TRUE,
  ### if TRUE (default) all data are exported into one file with the data
  ### series in different columns.   
  sep = ";",
  ### column separator to be used in CSV file contained in zip file. Default:
  ### ";"  
  dec = ",",
  ### decimal character to be used in CSV file contained in zip file. Default:
  ### ","
  target.dir = createDirAndReturnPath(file.path(tempdir(), "QuantumDownloads")),
  ### target directory to which the zip file is downloaded. Default: subfolder
  ### "QuantumDownloads" in temporary folder \code{tempdir()}
  target.file = "qwis_download.zip",
  ### file name to be given to downloaded zip file. Default:
  ### "qwis_download_yyyy_mm_dd_HHMMSS.zip"
  dbg = FALSE
  ### if TRUE, debug messages are shown
)
{  
  export.url <- "https://wis.quantumhydro.de/wis/zip_export.php/export.zip"
  
  parameters <- exportParameters(
    sessionID = sessionID,
    variableIDs = variableIDs, 
    from = from, 
    to = to,
    status = status,
    all.in.one = all.in.one,
    filename = .filenameTemplate(from, to, status),
    sep = sep,    
    dec = dec
  )    
  
  if(dbg) {
    cat("Parameters:\n")
    print(parameters)
  }
  
  curl <- getCurlHandle(ssl.verifypeer = FALSE)
  
  cat("*** Downloading zip-file content... ")
  zip.content <- postForm(
    uri = export.url, 
    style = "post",  
    .params = parameters,
    curl = curl,
    binary = TRUE
  )  
  cat("ok.\n")
  
  zipfile <- file.path(file.path(target.dir, target.file))
  
  cat("*** Writing zip file... ")    
  writeBin(as.vector(zip.content), con = zipfile)
  cat("ok.\n")
  
  zipfile
  ### full path to the generated zip-file
}

# .filenameTemplate ------------------------------------------------------------
.filenameTemplate <- function(from, to, status)
{
  paste0(
    "qwis_export_%p%_", 
    .dateToFileNamePart(from), "__", .dateToFileNamePart(to),
    ifelse(is.na(status), "", "_status%st%")
  )
}

# .dateToFileNamePart ----------------------------------------------------------
.dateToFileNamePart <- function(x)
{
  gsub(":", "", hsSubstSpecChars(x))
}

# exportParameters -------------------------------------------------------------
exportParameters <- function # generate list of export parameters
### function that helps to generate the list of export parameters
(
  sessionID,
  variableIDs,
  from,
  to,
  status = NA,
  ### vector of stati (1=QH:neu, 2=QH:bestaetigt, 3=QH:korrigiert, 5=Hy:neu).
  ### Default: NA (all stati are exported)
  all.in.one = TRUE,
  ### if TRUE (default) all data are exported into one file with the data
  ### series in different columns. 
  filename = NULL,
  ### name of CSV file to be created. Quantum allows the following placeholders:
  ### %p% = Messpunkt-Id, %pn% = Messpunktbezeichnung, %g% = Stations-Id, %gn% =
  ### Stationsname, %s% = Intervallanfang, %e% = Intervallende, %st% Status-IDs,
  ### falls danach gefiltert; fuehrender Unterstrich. Default:
  ### "qwis_export_%p%_%s%_%e%_status%st%"
  sep = ";",
  dec = ","  
)
{
  if (is.null(filename)) {
    filename <- "qwis_export_%p%_%s%_%e%_status%st%"
  } 
  
  parameters <- list(
    usecookie = 0,
    WIS_SESSID_D6BAF20B = sessionID,
    startTimestamp = as.integer(as.POSIXct(from, tz = "UTC")),
    endTimestamp = as.integer(as.POSIXct(to, tz = "UTC")) - 1,
    min_val = "",
    max_val = "",
    eol = 0,
    charset = "ISO-8859-15",
    format = "csv",
    fn_template = filename,
    table_export = ifelse(all.in.one, 1, 0),
    field_sep = sep,
    decimal_sep = dec,
    mandantor = "",
    selectmode = "manual",
    export = 1
  )
  
  for (i in seq_len(length(variableIDs))) {
    parameterName <- paste0("i[", i, "]")
    parameters[[parameterName]] <- variableIDs[i]
  } 
  
  # More than one status to select: e.g. "&status[]=1&status[]=2&status[]=3  
  if (! is.na(status)) {
    for (i in seq_len(length(status))) {
      parameterName <-"status[]"
      parameters[[parameterName]] <- status[i]
    }     
  }
  
  parameters
}

# .removeFileExtension ---------------------------------------------------------
.removeFileExtension <- function(x) 
{
  sub("\\.[^.]+$", "", x)
}

# .csvFilesInFolder ------------------------------------------------------------
.csvFilesInFolder <- function(path) 
{
  dir(path, "\\.csv$")
}
