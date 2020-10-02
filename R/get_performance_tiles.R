#' Pull Performance Tiles
#'
#' @description `get_performance_tiles()` retrieves Ookla broadband performance data for a given quarter.
#' Data are grouped and summarized by [quadkey](https://docs.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system) using a zoom level of 16.
#' This equates to a tile that is approximately 610.8 meters by 610.8 meters at the equator (18 arcsecond blocks).
#'
#' @param service Either 'mobile' or 'fixed' depending on whether you want mobile network or fixed broadband data
#' @param year Numeric value indicating the year of the time period of interest (e.g., `2020`)
#' @param quarter Numeric value indicating the quarter of interest (e.g., `2` for Q2)
#' @param sf Whether or not to return object as `sf` dataframe (default: `FALSE`)
#' @param ... Additional arguments passed to [arrow::read_parquet()]
#'
#' @return A data frame or `sf` data frame with the following fields:
#' * `avg_d_kbps`: The average download speed of all tests performed in the tile, represented in kilobits per second
#' * `avg_u_kbps`: The average upload speed of all tests performed in the tile, represented in kilobits per second
#' * `avg_lat_ms`: The average latency of all tests performed in the tile, represented in milliseconds
#' * `tests`: The number of tests taken in the tile
#' * `devices`: The number of unique devices contributing tests in the tile
#' * `quadkey`: The quadkey representing the tile. Quadkeys can act as a unique identifier for the tile.
#' * `tile`: The WKT representation of tile geometry

#' @export
#'
#' @examples
#' \dontrun{
#' # Pulls all fixed broadband tiles from Q2 2020 and returns an `sf` data frame
#' get_performance_tiles(service = "fixed", year = 2020, quarter = 2, sf = TRUE)
#'
#' # Get mobile quadkey and average download speeds for Q1 2020
#' get_performance_tiles(service = "mobile", year = 2020, quarter = 1, col_select = c("quadkey", "avg_d_kbps"))
#' }
get_performance_tiles <- function(service = c("mobile", "fixed"), year, quarter, sf = FALSE, ...) {
  service <- rlang::arg_match(service)

  assertthat::assert_that(
    assertthat::is.count(year),
    year >= 2020,
    assertthat::is.count(quarter),
    quarter >= 1,
    quarter <= 4,
    assertthat::is.flag(sf)
  )

  quarter_start <- paste(year, sprintf("%02d", ((quarter - 1) * 3) + 1), "01", sep = "-")

  target_url <- stringr::str_glue("https://ookla-open-data.s3.us-west-2.amazonaws.com/parquet/performance/type={service}/year={year}/quarter={quarter}/{quarter_start}_performance_{service}_tiles.parquet")

  result <- httr::GET(url = target_url, httr::progress())
  httr::stop_for_status(result)

  tiles <- arrow::read_parquet(httr::content(result), ...)

  # Convert to sf data frame if requested
  if (sf) {
    if (rlang::is_installed("sf")) {
      tiles <- sf::st_as_sf(tiles, wkt = "tile", crs = 4326)
    } else {
      rlang::warn("Please install sf, returning a data frame")
    }
  }

  tiles
}
