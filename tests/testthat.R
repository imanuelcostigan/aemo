library(testthat)
if (require(httr)) {install.packages('httr'); require(httr)}
library(aemo)
test_check("aemo")
