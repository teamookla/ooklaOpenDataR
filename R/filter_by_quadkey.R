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
#' @param parallel Enables use of a parallel backend using [parallel::mcmapply()]. Turned off by default. This setting is not recommended for use on Windows machines.
#' @param ncores Explicitly set number of cores to use if using [parallel::mcmapply()] is enabled. Will otherwise default the max available minus 1.

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
filter_by_quadkey <- function(tiles, bbox, parallel = FALSE, ncores = NULL) {
  assertthat::assert_that(inherits(bbox, "bbox"))

  # Check if paralellization is feasible
  if(parallel) {
    assertthat::see_if(.Platform$OS.type != "windows", msg = "Parallel likely will not work on Windows.")
    if(is.null(ncores)) ncores = parallel::detectCores()-1L
    else {
      assertthat::assert_that(is.numeric(ncores))
      assertthat::see_if(ncores <= parallel::detectCores(), msg = "More cores selected than are available.")
    }
  }

  # make sure the coordinates are lat/lon if sf is installed
  if (rlang::is_installed("sf")) {
    bbox <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bbox), 4326))
  }

  tile_grid <- slippymath::bbox_to_tile_grid(bbox, zoom = 16L)

  # Use parallelism if enabled and default to regular mapply otherwise
  if(!parallel) {
    quadkeys <- mapply(tileXYToQuadKey, xTile = tile_grid$tiles$x, yTile = tile_grid$tiles$y, MoreArgs = list(z = 16L))
  } else {
    quadkeys <- parallel::mcmapply(tileXYToQuadKey, xTile = tile_grid$tiles$x, yTile = tile_grid$tiles$y, MoreArgs = list(z = 16L),
                         SIMPLIFY = TRUE, USE.NAMES = TRUE,
                         mc.preschedule = TRUE, mc.set.seed = TRUE,
                         mc.silent = FALSE, mc.cores = ncores, mc.cleanup = TRUE)
  }

  tiles[tiles$quadkey %in% quadkeys, ]
}
