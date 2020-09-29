#' Pull Performance Tiles
#'
#' @description `get_performance_tiles()` retrieves Ookla broadband performance data for a given quarter.
#' Data are grouped and summarized by [quadkey](https://docs.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system) using a zoom level of 16.
#' This equates to a tile that is approximately 610.8 meters by 610.8 meters at the equator (18 arcsecond blocks).
#'
#' @param service Either 'mobile' or 'fixed' depending on whether you want mobile network or fixed broadband data
#' @param quarter Date indicating the start of the desired quarter (e.g., "2020-04-01" for 2020-Q2)
#' @param bbox Bounding box to pull tiles from. This way you can just get the specific tiles that you need for an analysis
#' @param sf Default `FALSE`. Return object as `sf` dataframe
#' @param ... Additional arguments passed to [arrow::read_parquet]
#'
#' @return either a data frame or `sf` data frame with the following fields:
#' * `avg_d_kbps`: The average download speed of all tests performed in the tile, represented in kilobits per second
#' * `avg_u_kbps`: The average upload speed of all tests performed in the tile, represented in kilobits per second
#' * `avg_lat_ms`: The average latency of all tests performed in the tile, represented in milliseconds
#' * `tests`: The number of tests taken in the tile
#' * `devices`: The number of unique devices contributing tests in the tile
#' * `quadkey`: The quadkey representing the tile. Quadkeys can act as a unique identifier for the tile.

#' @export
#'
#' @examples
#' \dontrun{
#' # Pulls all fixed broadband tiles from Q2 2020
#' get_performance_tiles(service = "fixed", quarter = "2020-04-01", sf = TRUE)
#'
#' # Get mobile quadkey and average download speeds for Q1 2020
#' get_performance_tiles(service = "mobile", quarter = "2020-01-01", col_select = c("quadkey", "avg_d_kbps"))
#' }
get_performance_tiles <- function(service = c("mobile", "fixed"), quarter, bbox = NULL, sf = FALSE, ...) {
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

  tiles <- arrow::read_parquet(httr::content(result), ...)

  # Convert to sf data frame if requested
  if (sf) {
    rlang::warn("sf option is currently not implemented")
  }

  tiles
}
