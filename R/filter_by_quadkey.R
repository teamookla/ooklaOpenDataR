as_binary <- function(x) {
  tmp <- rev(as.integer(intToBits(x)))
  id <- seq_len(match(1, tmp, length(tmp)) - 1)
  tmp[-id]
}

deg2num <- function(lat_deg, lon_deg, zoom) {
  lat_rad <- lat_deg * pi / 180
  n <- 2.0^zoom
  xtile <- floor((lon_deg + 180.0) / 360.0 * n)
  ytile <- floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
  c(xtile, ytile)
}

# reference JavaScript implementations
# https://developer.here.com/documentation/traffic/dev_guide/common/map_tile/topics/quadkeys.html

tileXYToQuadKey <- function(xTile, yTile, z) {
  quadKey <- ""
  for (i in z:1) {
    digit <- 0
    mask <- bitwShiftL(1, i - 1)
    xtest <- as_binary(bitwAnd(xTile, mask))
    if (any(xtest)) {
      digit <- digit + 1
    }

    ytest <- as_binary(bitwAnd(yTile, mask))
    if (any(ytest)) {
      digit <- digit + 2
    }
    quadKey <- paste0(quadKey, digit)
  }
  quadKey
}


#' Filter Tiles by Quadkey
#'
#' @description `filter_by_quadkey()` uses a bounding box to filter the tiles using the quadkey system as an efficient alternative to a spatial join.
#'
#' @param tiles From `get_performance_tiles()`
#' @param bbox [sf::st_bbox()] bounding box describing area from which to include tiles.

#' @return A filtered version of the `tiles` input
#' @export
#'
#' @examples
#' \dontrun{
#' # Filters tiles to a bounding box specified by coordinates
#' filter_by_quadkey(tiles, bbox = sf::st_bbox(c(xmin = 16.1, xmax = 16.6, ymax = 48.6, ymin = 47.9), crs = st_crs(4326)))
#'
#' # Filters tiles to a bounding box specified by an `sf` object
#' nc <- st_read(system.file("gpkg/nc.gpkg", package = "sf"), quiet = TRUE)
#' filter_by_quadkey(tiles, bbox = sf::st_bbox(nc))
#' }
#'
filter_by_quadkey <- function(tiles, bbox) {
  assertthat::assert_that(inherits(bbox, "bbox"))

  # make sure the coordinates are lat/lon if sf is installed
  if (rlang::is_installed("sf")) {
    bbox <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bbox), 4326))
  }

  tile_grid <- slippymath::bbox_to_tile_grid(bbox, zoom = 16)

  quadkeys <- mapply(tileXYToQuadKey, xTile = tile_grid$tiles$x, yTile = tile_grid$tiles$y, MoreArgs = list(z = 16))

  tiles[tiles$quadkey %in% quadkeys, ]
}
