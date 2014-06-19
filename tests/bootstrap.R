
if(require("RUnit", quietly=TRUE)) {
  ## Run all the tests
  wd <- getwd()
  testsuite <- defineTestSuite("PCICt", dirs=wd, testFileRegexp = "^test_functions.R$", testFuncRegexp = "^PCICt.test.+")
  PCICt.test.result <- runTestSuite(testsuite, useOwnErrorHandler=F)
  printTextProtocol(PCICt.test.result)
  stopifnot(climdex.pcic.test.result$PCICt$nFail == 0 && climdex.pcic.test.result$PCICt$nErr == 0)
}
