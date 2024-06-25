#' Convert a brick of bytes to a TIFF image
#'
#' @param path Input image path.
#' @param dims Dimension vector for the TIF image dimensions.
#' @param out Output image path. Defaults to the input image path, with .tif extension.
#' @return The image array, invisibly.
#' @export
ff_bob2tif <- function(path, dims, out = path |> fs::path_ext_set("tif")) {
  # TODO if missing dims, consider size of file ^(1/3)? with warning?
  img <- readBin(path, what = "integer", n = prod(dims), size = 1, signed = FALSE) |>
    array(dim = dims)
  ff_write(img, out)
  invisible(img)
}

#' Convert a TIFF image to a brick of bytes
#'
#' @param path Input image path.
#' @param out Output image path. Defaults to the input image path, with .tif extension.
#' @return The image array, invisibly.
#' @export
ff_tif2bob <- function(path, out = path |> fs::path_ext_set("bob")) {
  img <- ff_read(path)
  writeBin(c(img), out, size = 1)
  invisible(img)
}

#' Convert a PNG image to a TIFF image
#'
#' @param path Input image path.
#' @param out Output image path. Defaults to the input image path, with .tif extension.
#' @return The image array, invisibly.
#' @export
ff_png2tif <- function(path, out = path |> fs::path_ext_set("tif")) {
  img <- png::readPNG(path) * 255L
  ff_write(img, out)
  invisible(img)
}

#' Convert a TIFF image to a PNG image
#'
#' @param path Input image path.
#' @param out Output image path. Defaults to the input image path, with .tif extension.
#'
#' @return The image array, invisibly.
#' @export
ff_tif2png <- function(path, out = path |> fs::path_ext_set("png")) {
  img <- ff_read(path)
  if (length(dim(img)) > 2) stop("We can only convert 2D tifs to PNG.")
  img2 <- img / 255L
  mode(img2) <- "double"
  png::writePNG(img2, out)
  invisible(img)
}

#' Convert a CSV file to a TIFF image
#'
#' @param path Input image path.
#' @param dims Dimension vector for the TIF image dimensions.
#' @param out Output image path. Defaults to the input image path, with .tif extension.
#' @return The image array, invisibly.
#' @export
ff_csv2tif <- function(path, dims, out = path |> fs::path_ext_set("tif")) {
  img <- read.csv(path, header = FALSE)
  if (missing(dims)) dims <- rep(dim(img)[2], 3)
  img <- aperm(array(c(as.matrix(img)), dims), c(1, 3, 2))
  ff_write(img, out)
  invisible(img)
}

#' Convert a TIFF image to a CSV file
#'
#' @param path Input image path.
#' @param out Output image path. Defaults to the input image path, with .tif extension.
#' @return The image array, invisibly.
#' @export
ff_tif2csv <- function(path, out = path |> fs::path_ext_set("csv")) {
  img <- ff_read(path)
  if (length(dim(img)) == 2) img <- as.matrix(img)
  if (length(dim(img)) == 3) img <- matrix(aperm(img, c(2, 1, 3)), ncol = dim(img)[2], byrow = TRUE)
  write.table(img, out, row.names = FALSE, col.names = FALSE, sep = ",")
  invisible(img)
}

#' Convert a NumPy array to a TIFF image
#'
#' @param path Input image path.
#' @param out Output image path. Defaults to the input image path, with .tif extension.
#' @return The image array, invisibly.
#' @export
ff_npy2tif <- function(path, out = path |> fs::path_ext_set("tif")) {
  reticulate_available <- requireNamespace("reticulate", quietly = TRUE)
  if (reticulate_available) {
    np <- reticulate::import("numpy")
    img <- np$load(path)
    ff_write(img, out)
    return(invisible(img))
  }
  std::err("! Please make sure {.pkg reticulate} and NumPy are available.")
  std::err("e Please make sure {.pkg reticulate} and NumPy are available.")
}

#' Convert a TIFF image to a NumPy array
#'
#' @param path Input image path.
#' @param out Output image path. Defaults to the input image path, with .tif extension.
#' @return The image array, invisibly.
#' @export
ff_tif2npy <- function(path, out = path |> fs::path_ext_set("npy")) {
  # NOTE that this results in 32 bit npy integers as we go through R
  reticulate_available <- requireNamespace("reticulate", quietly = TRUE)
  if (reticulate_available) {
    np <- reticulate::import("numpy")
    img <- ff_read(path)
    np$save(out, reticulate::np_array(img, "uint8"))
    return(invisible(img))
  }
  std::err("! Please make sure {.pkg reticulate} and NumPy are available.")
  std::err("e Please make sure {.pkg reticulate} and NumPy are available.")
}
