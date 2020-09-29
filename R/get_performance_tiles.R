#' Pull Performance Tiles
#'
#' @param service Either 'mobile' or 'fixed' depending on whether you want mobile network or fixed broadband data
#' @param quarter Date indicating the start of the desired quarter (e.g., "2020-04-01" for 2020-Q2)
#' @param bbox Bounding box to pull tiles from. This way you can just get the specific tiles that you need for an analysis
#' @param quarter_end
#' @param sf Default `FALSE`. Return object as `sf` dataframe
#'
#' @return either a tibble or `sf` dataframe
#' @export
#'
#' @examples
#' get_performance_tiles(service = "fixed", quarter_start = "2020-04-01", sf = TRUE) # Pulls all fixed broadband tiles from Q2 2020

get_performance_tiles <- function(service = c("mobile", "fixed"), quarter, bbox = NULL, sf = FALSE) {
  service <- rlang::arg_match(service)

  assertthat::assert_that(
    assertthat::is.date(quarter) || assertthat::is.string(quarter),
    assertthat::is.flag(sf)
  )

  if (lubridate::day(quarter) != 1 || lubridate::month(quarter) %% 3 != 1) {
    rlang::abort(stringr::str_glue("'{quarter}' does not appear to be the beginning of a quarter"))
  }

  target_url <- stringr::str_glue("https://ookla-open-data.s3.us-west-2.amazonaws.com/parquet/performance/type={service}/year={lubridate::year(quarter)}/quarter={lubridate::quarter(quarter)}/{quarter}_performance_{service}_tiles.parquet")

  result <- httr::GET(url = target_url, httr::progress())
  httr::stop_for_status(result)
  arrow::read_parquet(httr::content(result))
}
