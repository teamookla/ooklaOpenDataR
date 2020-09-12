#' Pull Performance Tiles
#'
#' @param service Either 'mobile' or 'fixed' depending on whether you want mobile network or fixed broadband data
#' @param bbox Bounding box to pull tiles from. This way you can just get the specific tiles that you need for an analysis
#' @param quarter_start
#' @param quarter_end
#' @param sf Default `FALSE`. Return object as `sf` dataframe
#'
#' @return either a tibble or `sf` dataframe
#' @export
#'
#' @examples
#' get_performance_tiles(service = "fixed", quarter_start = "2020-04-01", sf = TRUE) # Pulls all fixed broadband tiles from Q2 2020

get_performance_tiles <- function(service = c("mobile", "fixed"), bbox = NULL, quarter_start, quarter_end = NULL, sf = FALSE) {

}
