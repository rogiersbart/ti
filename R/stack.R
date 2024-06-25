#' Merge single-frame images into a multi-frame TIFF
#'
#' @param paths Paths to the single-frame images to be merged.
#' @param out Path to the output multi-frame TIFF.
#' @return
#' @export
ff_merge <- function(paths, out) {
  process_imgs <- if (fs::path_ext(paths[1]) %in% c("tif", "tiff")) {
    \(x) purrr::map(x, \(y) ff_read(y)) |> abind::abind(along = 3)
  } else {
    \(x) purrr::map(x, \(y) png::readPNG(y) * 255L) |> abind::abind(along = 3)
  }
  img <- process_imgs(paths)
  ff_write(img, out)
}

#' Split a multi-frame TIFF into single-frame images
#'
#' @param path Path of the multi-frame TIFF.
#' @param out Path for the output single-frame images, using "%frame" as placeholder for the frame number. Defaults to the input file path with extension ".001.tif" etc.
#' @return
#' @export
ff_split <- function(path, out = path |> fs::path_ext_set("%frame.tif")) {
  img <- ff_read(path)
  nframes <- dim(img)[3]
  rm(img)
  ff_extract(path, frames = 1:nframes, out = out)
}

#' Extract specific frames from a multi-frame TIFF
#'
#' @param path Path of the multi-frame TIFF.
#' @param frames Frame numbers to extract.
#' @param out Path for the output single-frame images, using "%frame" as placeholder for the frame number. Defaults to the input file path with extension ".001.tif" etc.
#' @return
#' @export
ff_extract <- function(path, frames, out = path |> fs::path_ext_set("%frame.tif")) {
  img <- ff_read(path)
  nframes <- dim(img)[3]
  for (frame in frames) {
    ff_write(
      img[, , frame],
      gsub(
        "%frame",
        paste0(paste0(rep("0", nchar(nframes) - nchar(frame)), collapse = ""), frame),
        out
      )
    )
  }
}
