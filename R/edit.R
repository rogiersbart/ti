#' Compress a TIFF image
#'
#' @param path Input image path.
#' @param type Type of compression to apply (`"none"`, `"LZW"`, `"PackBits"`, `"RLE"`, `"JPEG"`, `"deflate"` (default) or `"Zip"`).
#' @param out Output image path. Defaults to the input image path.
#' @return The output image, invisibly.
#' @export
ff_compress <- function(path, type = "deflate", out = path) {
  img <- ff_read(path)
  ff_write(img, out, compression = type)
}

#' Stretch the contrast of a TIFF image
#'
#' @param path Input image path.
#' @param out Output image path. Defaults to the input image path.
#' @return The output image, invisibly.
#' @export
ff_stretch <- function(path, out = path) {
  img <- ff_read(path)
  img_range <- range(img)
  img_min <- img_range[1]
  img_max <- img_range[2]
  img <- ((img - img_min) * 255L) %/% (img_max - img_min)
  ff_write(img, out)
}

#' Invert a TIFF image
#'
#' @param path Input image path.
#' @param out Output image path. Defaults to the input image path.
#' @return The output image, invisibly.
#' @export
ff_invert <- function(path, out = path) {
  img <- ff_read(path)
  img <- 255L - img
  ff_write(img, out)
}

#' Transpose a TIFF image
#'
#' @param path Input image path.
#' @param dims The dimension permutation vector.
#' @param out Output image path. Defaults to the input image path.
#' @return The output image, invisibly.
#' @export
ff_transpose <- function(path, dims, out = path) {
  img <- ff_read(path)
  img <- aperm(img, dims)
  ff_write(img, out)
}

