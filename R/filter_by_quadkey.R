as_binary <- function(x){
  tmp <- rev(as.integer(intToBits(x)))
  id <- seq_len(match(1, tmp, length(tmp)) - 1)
  tmp[-id]
}

deg2num <- function(lat_deg, lon_deg, zoom) {
  lat_rad <- lat_deg * pi /180
  n = 2.0 ^ zoom
  xtile = floor((lon_deg + 180.0) / 360.0 * n)
  ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
  c(xtile, ytile)
}

# reference JavaScript implementations
# https://developer.here.com/documentation/traffic/dev_guide/common/map_tile/topics/quadkeys.html

tileXYToQuadKey <- function(xTile, yTile, z) {
  quadKey <- ""
  for (i in z:1) {
    digit = 0
    mask = bitwShiftL(1, i - 1)
    xtest = as_binary(bitwAnd(xTile, mask))
    if(any(xtest)) {
      digit = digit + 1
    }

    ytest <- as_binary(bitwAnd(yTile, mask))
    if(any(ytest)) {
      digit <- digit + 2
    }
    quadKey <- paste0(quadKey, digit)
  }
  quadKey
}


#' Pull Performance Tiles
#'
#' @param tiles Tiles to filter
#' @param bbox Bounding box to pull tiles from. This way you can just get the specific tiles that you need for an analysis
#'
#' @return either a tibble or `sf` dataframe

filter_by_quadkey <- function(tiles, bbox) {
  tile_grid <- slippymath::bbox_to_tile_grid(bbox, zoom = 16)
  quadkeys <- pmap(list(tile_grid$tiles$x, tile_grid$tiles$y, 16), tileXYToQuadKey)
  perf_tiles <- tiles %>%
    filter(quadkey %in% quadkeys)
  return(perf_tiles)
}
