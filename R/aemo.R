#' Download data from AEMO website
#'
#' @param regions a case invariant character vector of abbreviated AEMO regions.
#' @param years a numeric vector of years in the form \code{YYYY}.
#' @param months a numeric vector of months in the form \code{MM}.
#' @return data files whose number corresponds to the maximum lenght of one of
#' the parameters. The file names have the pattern: "[REGION][YYYY][MM].csv"
#' @importFrom assertthat assert_that
#' @export
get_aemo_data <- function (regions, years, months)
{
  regions <- toupper(regions)
  periods <- as.numeric(paste0(years, formatC(months, width = 2, flag = '0')))
  assert_that(all(regions %in% aemo_regions()),
    all(periods %in% aemo_periods()))
  url <- aemo_data_url(regions, years, months)
  destfile <- aemo_data_file_name(regions, years, months)
  invisible(Map(download.file, url = url, destfile = destfile))
}

#' @importFrom dplyr rbind_all
collate_aemo_data <- function (path = '.', remove_files = TRUE)
{
  aemo_files <- list_aemo_data_files(path)
  if (identical(aemo_files, character()))
  {
    info <- all_regions_periods_combos()
    message('Downloading data from AEMO....')
    get_aemo_data(info$region, info$years, info$months)
    aemo_files <- list_aemo_data_files(path)
  }
  message('Reading AEMO data files...')
  aemo_dfs <- lapply(aemo_files, read.csv)
  if (remove_files) {
    message('Removing AEMO data files...')
    clean_up_aemo_data_files(path)
  }
  message('Collating AEMO data...')
  rbind_all(aemo_dfs)
}

aemo_regions <- function ()
  c('NSW', 'QLD', 'VIC', 'SA')

aemo_periods <- function ()
{
  yy_seq <- 1998:year(today())
  period_min <- 199812
  period_max <- as.numeric(paste0(year(today()),
    formatC(month(today()) - 1, width = 2, flag = '0')))
  all_periods <- outer(yy_seq, formatC(1:12, width = 2, flag = '0'), paste0)
  all_periods <- sort(as.numeric(all_periods))
  in_period <- all_periods >= period_min & all_periods <= period_max
  all_periods[in_period]
}

aemo_data_url_stub <- function (regions, years, months)
{
  stubs <- vector('character', NROW(paste0(regions, years, months)))
  stubs <- 'http://www.nemweb.com.au/mms.GRAPHS/data/DATA'
  stubs[years == 1999 & months == 11] <-
    'http://www.nemmco.com.au/mms/data/DATA'

}

aemo_data_url <- function (regions, years, months)
{
  paste0(aemo_data_url_stub(regions, years, months), years,
    formatC(months, width = 2, flag = '0'), '_', toupper(regions), '1.csv')
}

aemo_data_file_name <- function (regions, years, months)
  paste0(toupper(regions), years, formatC(months, width = 2, flag = '0'), '.csv')

list_aemo_data_files <- function (path = '.')
  list.files(path, '(NSW|QLD|VIC|SA|TAS|SNOWY)[[:digit:]]{6}\\.csv')

clean_up_aemo_data_files <- function (path = '.')
  file.remove(list_aemo_data_files(path))

#' @importFrom stringr str_sub
all_regions_periods_combos <- function ()
{
  all_periods <- aemo_periods()
  regions <- rep(aemo_regions(), each = NROW(all_periods))
  years <- as.numeric(rep(str_sub(all_periods, 1, 4), NROW(aemo_regions())))
  months <- as.numeric(rep(str_sub(all_periods, 5), NROW(aemo_regions())))
  list(regions = regions, years = years, months = months)
}
