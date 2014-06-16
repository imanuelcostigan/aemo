context ('URL')

# URL scheme based on website on 15 Jun 2014
# Some months' data uses the nemmco url e.g Nov 1999.

test_that('AEMO regions complete', {
  expect_equal(aemo_regions(), c('NSW', 'QLD', 'VIC', 'SA', 'TAS', 'SNOWY'))
})

test_that('URL stub is correct', {
  expect_equal(aemo_data_url_stub('nsw', 1999, 11),
    'http://www.nemmco.com.au/mms/data/DATA')
  expect_equal(aemo_data_url_stub('nsw', 2011, 11),
    'http://www.nemweb.com.au/mms.GRAPHS/data/DATA')
})

test_that('AEMO data URL creator is correct across regions and months', {
  regions <- c(rep('nsw', 2), 'qld', 'sa', 'tas', 'vic', 'nsw')
  yy <- c(rep(2011, 6), 1999)
  mm <- c(1, 11, 8, 5, 12, 3, 12)
  results <- c('http://www.nemweb.com.au/mms.GRAPHS/data/DATA201101_NSW1.csv',
    'http://www.nemweb.com.au/mms.GRAPHS/data/DATA201111_NSW1.csv',
    'http://www.nemweb.com.au/mms.GRAPHS/data/DATA201108_QLD1.csv',
    'http://www.nemweb.com.au/mms.GRAPHS/data/DATA201105_SA1.csv',
    'http://www.nemweb.com.au/mms.GRAPHS/data/DATA201112_TAS1.csv',
    'http://www.nemweb.com.au/mms.GRAPHS/data/DATA201103_VIC1.csv',
    'http://www.nemweb.com.au/mms.GRAPHS/data/DATA199912_NSW1.csv')
  expect_equal(aemo_data_url(regions, yy, mm), results)
  expect_equal(aemo_data_url('nsw', 1999, 11),
    "http://www.nemmco.com.au/mms/data/DATA199911_NSW1.csv")
})
