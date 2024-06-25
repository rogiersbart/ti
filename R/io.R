#' Read a TIFF image
#'
#' @param path Image path.
#' @return
#' @export
ff_read <- function(path) {
  img <- ijtiff::tif_read(path, msg = FALSE)
  if (dim(img)[3] > 1) {
    std::err("! The image has more than one channel!")
    std::err("! Consider using {.pkg ijtiff} instead.")
  }
  mode(img) <- "integer"
  if (dim(img)[3] == 1) return(img[,,1,])
  img
}

#' Write a TIFF image
#'
#' @param img 2D or 3D array.
#' @param path Path to write to.
#' @param compression Type of compression to apply (`"none"`, `"LZW"`, `"PackBits"`, `"RLE"`, `"JPEG"`, `"deflate"` (default) or `"Zip"`).
#' @return
#' @export
ff_write <- function(img, path, compression = "deflate") {
  if (!length(dim(img)) %in% c(2, 3)) {
    std::err("! {.arg img} is not a 2D or 3D array!")
    std::err("! Consider using {.pkg ijtiff} instead.")
  }
  ijtiff::tif_write(
    img,
    path,
    compression = compression,
    msg = FALSE,
    overwrite = TRUE
  )
}
