library(PCICt)
library(Runit)

test.values <- c("1850-01-01", "0000-01-01", "2012-04-01 00:10:00", "2012-02-28")

## Tests as.pcict functions
test.as.PCICt <- function() {
  test.calendars <- c("360", "365", "gregorian", "proleptic_gregorian")
  360.only <- c("2011-02-30")
  gregorian.not.365 <- c("2012-02-29")
  gregorian.not.360 <- c("2011-01-31")

  ## Check that valid input produces valid output for all calendar types
  for(cal in test.calendars) {
    checkEquals(as.character(as.PCICt(test.values, cal=cal)), test.values)
  }

  ## Check that invalid input triggers errors
  checkException(as.PCICt(360.only, cal="365"))
  checkException(as.PCICt(360.only, cal="gregorian"))
  checkException(as.PCICt(gregorian.not.365, cal="365"))
  checkException(as.PCICt(gregorian.not.360, cal="360"))
  checkException(as.PCICt("your mom", cal="360"))
  checkException(as.PCICt("your mom", cal="365"))

  ## Check that NAs are passed through properly
  checkTrue(is.na(as.PCICt(NA, cal="360")))
  checkTrue(is.na(as.PCICt(NA, cal="365")))

  ## Check that as.PCICt.POSIXlt works as expected
  checkEquals(as.character(as.PCICt(as.POSIXlt(test.values)), cal="360"), test.values)
  checkEquals(as.character(as.PCICt(as.POSIXlt(test.values)), cal="365"), test.values)
  checkEquals(as.character(as.PCICt(as.POSIXlt(test.values)), cal="gregorian"), test.values)
  checkEquals(as.character(as.PCICt(as.POSIXlt(test.values)), cal="proleptic_gregorian"), test.values)

  ## Check that as.PCICt.POSIXct works as expected
  checkEquals(as.character(as.PCICt(as.POSIXlt(test.values)), cal="360"), test.values)
  checkEquals(as.character(as.PCICt(as.POSIXlt(test.values)), cal="365"), test.values)
  checkEquals(as.character(as.PCICt(as.POSIXlt(test.values)), cal="gregorian"), test.values)
  checkEquals(as.character(as.PCICt(as.POSIXlt(test.values)), cal="proleptic_gregorian"), test.values)
}

## Tests subset operators
test.subset <- function() {
  dat <- as.PCICt(test.values, cal="365")
  dat2 <- dat[2:4]

  ## Check that subset operator works as expected
  checkEquals(as.character(dat[2:4]), as.character(dat)[2:4])

  ## Check that subset operator preserves attributes properly
  checkEquals(attr(dat2, "cal"), attr(dat, "cal"))

  ## Check assignment
}

## Tests arithmetic operators (+, -)
test.operators <- function() {
  checkEquals(as.character(as.PCICt("2012-02-28", cal="365") + 86400), "2012-03-01")
  checkEquals(as.character(as.PCICt("2012-02-28", cal="365") + 86400), "2012-03-01")
}

test.trunc <- function() {
}

test.round <- function() {
}

test.cut <- function() {
}

test.summary <- function() {
}

test.format <- function() {
}

test.as.POSIXlt <- function() {
}

test.as.POSIXct <- function() {
}

test.julian <- function() {
}

test.range <- function() {
}

test.pretty <- function() {
}

test.c <- function() {
}

test.seq <- function() {
  ## Check that sequences work as expected
  checkEquals(seq(as.PCICt("2011-02-30", cal="360"), as.PCICt("2012-04-01 00:10:00", cal="360"), by="years"), as.PCICt(c("2011-02-30", "2012-02-30"), cal="360"))
  checkEquals(seq(as.PCICt("2011-02-28", cal="365"), as.PCICt("2012-04-01 00:10:00", cal="365"), by="years"), as.PCICt(c("2011-02-28", "2012-02-28"), cal="365"))
}

## Run all the tests
testsuite <- defineTestSuite("PCICt", dirs="./", testFileRegexp = "^test_functions.R$", testFuncRegexp = "^test\..+")
test.result <- runTestSuite(testsuite, useOwnErrorHandler=F)
## printTextProtocol(test.result)
