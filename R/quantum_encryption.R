# .getPassword -----------------------------------------------------------------
.getPassword <- function(user)
{
  kwb.utils::getPassword(
    passwordFile = .checkAndGetPasswordFilename(user),
    keyFile <- .checkAndGetKeyFilename()
  )
  
  ### NA if no password is stored
}

# .checkAndGetPasswordFilename -------------------------------------------------
.checkAndGetPasswordFilename <- function(user)
{
  passwordFile <- .passwordFile(user)
  
  if (! file.exists(passwordFile)) {
    message("There is no password file for user '", user, "'! ")
    savePasswordInPackage(user)
  }
  
  passwordFile
}

# savePasswordInPackage --------------------------------------------------------
savePasswordInPackage <- function # save password within the package
### Save the password needed to login at Quantum server in encrypted form
### within the package
(
  user = "Rouault",
  ### user login used to authenticate at Quantum server. Default: "Rouault"
  keyFile = .checkAndGetKeyFilename()
)
{
  kwb.utils::createPasswordFile(
    user, 
    keyFile = keyFile, 
    passwordFile = .passwordFile(user)
  )
}

# .passwordFile ----------------------------------------------------------------
.passwordFile <- function(user)
{
  file.path(
    system.file("extdata", package = "kwb.quantum"),
    paste0(".pw_", user)
  )
}

# .checkAndGetKeyFilename ------------------------------------------------------
.checkAndGetKeyFilename <- function()
{
  keyFile <- .keyFile()
  
  if (file.exists(keyFile) && .isValidKeyfile(readLines(keyFile))) {
    success <- TRUE
  } 
  else {
    message("Please select the file containing the encryption/decryption key ",
            "using the file selection dialog (may be hidden behind this ", 
            "window)")
    success <- setKeyFile()
  }
  
  if (!success) {
    stop("I need a key file but you did not specify it!")
  }
  
  readLines(keyFile)
}

# .isValidKeyfile --------------------------------------------------------------
.isValidKeyfile <- function(keyFile)
{
  valid <- !is.null(keyFile) & (length(keyFile) > 0) 
  
  if (valid && file.exists(keyFile)) {
    valid <- readLines(keyFile, 1) == "-----BEGIN RSA PRIVATE KEY-----"
  }    
  
  valid
}

# .keyFile ---------------------------------------------------------------------
.keyFile <- function()
{
  file.path(
    system.file("extdata", package = "kwb.quantum"), 
    "pathToKeyFile.txt"
  )
}

# setKeyFile -------------------------------------------------------------------
setKeyFile <- function # set path to the decyption key file
### set path to the decyption key file
(
  keyFile = choose.files(caption = "Choose a key file")
  ### full path to decryption key file. Use \code{\link{generateKeyFile}} to
  ### create such a file
)
{
  if (.isValidKeyfile(keyFile)) {
    writeLines(keyFile, con = .keyFile())
    TRUE
  }
  else {
    FALSE  
  }
  
  ### TRUE if key file could be set, otherwise FALSE
}
