context("URL")

library(httr)
# URL scheme based on website on 15 Jun 2014
# Some months' data uses the nemmco url e.g Nov 1999.

test_that("URL stub is correct", {
  expect_equal(
    aemo_data_url_stub(),
    "https://www.aemo.com.au/aemo/data/nem/priceanddemand/PRICE_AND_DEMAND_"
  )
  expect_equal(
    aemo_data_url_stub(),
    "https://www.aemo.com.au/aemo/data/nem/priceanddemand/PRICE_AND_DEMAND_"
  )
})

test_that("AEMO data URL creator is correct across regions and months", {
  regions <- c(rep("nsw", 2), "qld", "sa", "tas", "vic", "nsw")
  yy <- c(rep(2011, 6), 1999)
  mm <- c(1, 11, 8, 5, 12, 3, 12)
  results <- c(
    "https://www.aemo.com.au/aemo/data/nem/priceanddemand/PRICE_AND_DEMAND_201101_NSW1.csv",
    "https://www.aemo.com.au/aemo/data/nem/priceanddemand/PRICE_AND_DEMAND_201111_NSW1.csv",
    "https://www.aemo.com.au/aemo/data/nem/priceanddemand/PRICE_AND_DEMAND_201108_QLD1.csv",
    "https://www.aemo.com.au/aemo/data/nem/priceanddemand/PRICE_AND_DEMAND_201105_SA1.csv",
    "https://www.aemo.com.au/aemo/data/nem/priceanddemand/PRICE_AND_DEMAND_201112_TAS1.csv",
    "https://www.aemo.com.au/aemo/data/nem/priceanddemand/PRICE_AND_DEMAND_201103_VIC1.csv",
    "https://www.aemo.com.au/aemo/data/nem/priceanddemand/PRICE_AND_DEMAND_199912_NSW1.csv"
  )
  expect_equal(aemo_data_url(regions, yy, mm), results)
  expect_equal(
    aemo_data_url("nsw", 1999, 11),
    "https://www.aemo.com.au/aemo/data/nem/priceanddemand/PRICE_AND_DEMAND_199911_NSW1.csv"
  )
})

test_that("AEMO data URLs still correct", {
  web_url <- "https://www.aemo.com.au/aemo/data/nem/priceanddemand/PRICE_AND_DEMAND_201101_NSW1.csv"
  co_url <- "https://www.aemo.com.au/aemo/data/nem/priceanddemand/PRICE_AND_DEMAND_199911_NSW1.csv"
  expect_equal(GET(web_url)$status, 200)
  expect_equal(GET(co_url)$status, 200)
})
