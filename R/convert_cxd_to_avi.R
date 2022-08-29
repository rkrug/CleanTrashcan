#' Function to convert the video files to .avi format using lossless conversion
#'
#' Function uses bftools to convert \code{cxd} files to \code{avi} and ffmpeg to
#' compress these to lossles avi
#' @param cxd_file one or more \code{cxd} file to be converted or a directory with \code{.cxd} files.
#' @param avi_dir directory for the converted \code{cxd} files and the metadata files
#' @param compression_level compression level - defaults to 6.
#'   Smaller numbers: faster and larger, larger numbers (maximum 9) smaller and slower.
#' @param ffmpeg execuable ffmpeg. May have to be including path.
#' @param bfconvert executable bfconvert from bftools. May have to be including path.
#' @param showinf executable showinf from bftools. May have to be including path.
#' @param delete_cxd if \code{TRUE}, the \code{.cxd} file will be deleted after successful conversion.
#'   Default: \code{FALSE}
#' @param mc.cores Number of cores to be used when more than one xd file is given.
#' @return returns nothing (NULL)
#'
#' @importFrom parallel mclapply
#' @export

convert_cxd_to_avi <- function (
    cxd_file,
    avi_dir,
    compression_level = 6,
    ffmpeg = system.file(package = "CleanTrashcan", "ffmpeg"),
    bfconvert = system.file(package = "CleanTrashcan", "bftools", "bfconvert"),
    showinf =  system.file(package = "CleanTrashcan", "bftools", "showinf"),
    delete_cxd = FALSE,
    mc.cores = 1
) {
  cxd_file <- normalizePath(cxd_file)
  avi_dir <- normalizePath(avi_dir, mustWork = FALSE)

  if (length(cxd_file) == 1 && dir.exists(cxd_file)) {
    avi_dir <- paste0(avi_dir, "_comp_", compression_level)
    convert_cxd_to_avi(
      cxd_file = list.files(
        cxd_file,
        pattern = "\\.cxd$",
        full.names = TRUE
      ),
      avi_dir = avi_dir,
      compression_level = compression_level,
      ffmpeg = ffmpeg,
      bfconvert = bfconvert,
      showinf = showinf,
      delete_cxd = delete_cxd,
      mc.cores = mc.cores
    )
    return(invisible(NULL))
  } else if (length(cxd_file) > 1) {
    message("<<< BEGIN mclapply convert to avi")
    message("    mc.cores = ", mc.cores)
    parallel::mclapply(
      cxd_file,
      convert_cxd_to_avi,
      avi_dir = avi_dir,
      compression_level = compression_level,
      ffmpeg = ffmpeg,
      bfconvert = bfconvert,
      showinf = showinf,
      delete_cxd = delete_cxd,
      mc.cores = mc.cores
    )
    message(">>> END mclapply convert to avi")
    return(invisible(NULL))
  }


# The magic begins here ---------------------------------------------------


  message("    BEGIN converting ", basename(cxd_file))

  tmpdir <- file.path(avi_dir, "tmp_convert", basename(cxd_file)) # tempfile()
  dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
  tiff_dir_tmp <- file.path(tmpdir, "tiff")
  dir.create(tiff_dir_tmp, showWarnings = FALSE)
  on.exit(
    {
      unlink(tmpdir, recursive = TRUE, force = TRUE)
    }
  )

  avi_file <- file.path(
    avi_dir,
    gsub("\\.cxd$", ".avi", basename(cxd_file))
  )
  avi_tmp <- file.path(tmpdir, basename(avi_file))

  cxd_metadata_file <- file.path(
    avi_dir,
    paste0(basename(cxd_file), ".metadata")
  )
  cxd_metadata_tmp <- file.path(tmpdir, basename(cxd_metadata_file))


  message("      Extracting Metadata from ", cxd_file, " -->> ", cxd_metadata_tmp)
  arguments <- paste0(
    " -nopix",
    " -no-upgrade",
    " '", cxd_file, "'"
  )
  system2(
    command = showinf,
    args = arguments,
    stdout = cxd_metadata_tmp
  )

  message("      Converting ", basename(cxd_file), " -->> tiffs")
  arguments <- paste0(
    " -overwrite",
    " -no-upgrade ",
    " '", cxd_file, "'",
    " -padded",
    " '", file.path(tiff_dir_tmp, "frame%t.tiff"), "'"
  )
  system2(
    command = bfconvert,
    args = arguments,
    stdout = NULL
  )

  message("      Converting tiffs", " -->>", avi_tmp)
  fps <- get_fps_cxd(cxd_file, showinf = showinf, mc.cores = mc.cores)
  arguments <- paste0(
    " -framerate ", fps,
    " -pattern_type glob",
    " -i  '", file.path(tiff_dir_tmp, "*.tiff"), "'",
    " -vcodec png",
    " -vtag 'PNG '",
    " -compression_level ", compression_level,
    " '", avi_tmp, "'"
  )
  system2(
    command = ffmpeg,
    args = arguments,
    stdout = NULL
  )

  # message("      Moving ", basename(avi_tmp), " -->> ", basename(avi_file))
  # dir.create(dirname(avi_file), showWarnings = FALSE, recursive = TRUE)
  # file.copy(
  #   from = avi_tmp,
  #   to = avi_file
  # )
  message("      Moving ", basename(cxd_metadata_tmp), " -->> ", basename(cxd_metadata_file))
  file.rename(
    from = cxd_metadata_tmp,
    to = cxd_metadata_file
  )
  file.rename(
    from = avi_tmp,
    to = avi_file
  )
  if (delete_cxd) {
    message("      Deleting ", cxd_file)
    unlink(cxd_file)
  }
  unlink(
    x = tmpdir,
    recursive = TRUE
  )

  message("    END converting ", cxd_file)

  invisible(NULL)
}





#' Extract delays from the \code{.cxd} file
#'
#' This function reads the metadata from the \code{.cxd} file and returns the
#' delays between the images were taken.
#' @param file file name, of the \code{.cxd} file
#' @param mc.cores Number of cores to be used when more than one xd file is given.
#'
#' @return a vector of the length of number of frames -1 specifying the delay between the images taken.
#' @export
#'
#' @examples
get_delays_cxd <- function(
    file,
    showinf = par_showinf(),
    mc.cores = par_mc.cores()
) {
  file <- normalizePath(file)

  if ((length(file) == 1) && dir.exists(file)) {
    delays <- get_delays_cxd(
      file = list.files(
        file,
        pattern = "\\.cxd$",
        full.names = TRUE
      ),
      mc.cores = mc.cores
    )
    return(delays)
  } else if (length(file) > 1) {
    delays <- parallel::mclapply(
      file,
      FUN = get_delays_cxd,
      mc.cores = mc.cores
    )
    names(delays) <- basename(file)
    return(delays)
  }

  file <- normalizePath(file)

  arguments <- paste0(
    " -nopix",
    " -no-upgrade",
    " '", file, "'"
  )
  md <- system2(
    command = showinf,
    args = arguments,
    stdout = TRUE
  )
  md <- grep("Time_From_Last", md, value = TRUE)
  delay <- sapply(
    strsplit(md, ": "),
    "[",
    2
  )[-1]
  delay <- as.numeric(delay)
  return(delay)
}


#' Extract fps (frames per second) from the \code{.cxd} file
#'
#' The fps is based on the results of \code{get_delays_cxd(file)} by averaging these and
#' calculating from this mean the fps.
#' @param file  file name, of the \code{.cxd} file
#'
#' @return the fps (frames per second) of the video
#' @export
#'
#' @examples
get_fps_cxd <- function(
    file,
    showinf = par_showinf(),
    mc.cores = par_mc.cores()
) {
  if (length(file) != 1) {
    stop("File has to be of length 1")
  }

  file <- normalizePath(file)

  x <- get_delays_cxd(file, showinf = showinf, mc.cores = mc.cores)
  fps <- 1 / mean(x)
  return(fps)
}

#' Extract fps (frames per second) using ffmpeg
#'
#' The fps is based on using the output of \code{ffmpeg} and
#' should therefore work for all ffmpeg supported video formats,
#' but at the moment only supported for \code{avi} files.
#' @param file  file name(s), of the video (\code{avi}) file(s) or directory in which the video files are
#' @param mc.cores Number of cores to be used when more than one xd file is given.
#'
#' @return named vector with the fps of the video(s) and the names are the file names
#' @export
#'
#' @examples
get_fps_avi <- function(
    file,
    ffmpeg = par_ffmpeg(),
    mc.cores = par_mc.cores()
) {

  file <- normalizePath(file)

  if ((length(file) == 1) && dir.exists(file)) {
    fps <- get_fps_avi(
      file = list.files(
        file,
        pattern = "\\.avi$",
        full.names = TRUE
      ),
      ffmpeg = ffmpeg,
      mc.cores = mc.cores
    )
    return(simplify2array(fps))
  } else if (length(file) > 1) {
    fps <- parallel::mclapply(
      file,
      FUN = get_fps_avi,
      ffmpeg = ffmpeg,
      mc.cores = mc.cores
    )
    return(simplify2array(fps))
  }

  arguments <- paste0(
    " -i '", file, "'"
  )
  output <- suppressWarnings(
    system2(
      command = ffmpeg,
      args = arguments,
      stderr = TRUE
    )
  )
  fps <- grep(
    "fps",
    output,
    value = TRUE
  )
  fps <- strsplit(
    fps,
    ","
  )[[1]]
  fps <- grep(
    "fps",
    fps,
    value = TRUE
  )
  fps <- gsub("fps", "", fps)
  fps <- as.numeric(fps)
  names(fps) <- basename(file)
  return(simplify2array(fps))
}

#' Extract duration in seconds using ffmpeg
#'
#' The duration is based on using the output of \code{ffmpeg} and
#' should therefore work for all ffmpeg supported video formats,
#' but is only implemented for \code{.avi} files.
#' @param file  file name, of the video file
#' @param mc.cores Number of cores to be used when more than one xd file is given.
#'
#' @return named vector with the duration (in seconds) of the video(s) and the names are the file names
#' @export
#'
#' @examples
get_duration_avi <- function(
    file,
    ffmpeg = par_ffmpeg(),
    mc.cores = par_mc.cores()
) {

  file <- normalizePath(file)
  if ((length(file) == 1) && dir.exists(file)) {
    duration <- get_duration_avi(
      file = list.files(
        file,
        pattern = "\\.avi$",
        full.names = TRUE
      ),
      ffmpeg = ffmpeg,
      mc.cores = mc.cores
    )
    return(simplify2array(duration))
  } else if (length(file) > 1) {
    duration <- parallel::mclapply(
      file,
      FUN = get_duration_avi,
      ffmpeg = ffmpeg,
      mc.cores = mc.cores
    )
    return(simplify2array(duration))
  }

  arguments <- paste0(
    " -i '", file, "'"
  )
  output <- suppressWarnings(
    system2(
      command = ffmpeg,
      args = arguments,
      stderr = TRUE
    )
  )
  duration <- grep(
    "Duration",
    output,
    value = TRUE
  )
  duration <- strsplit(
    duration,
    ","
  )[[1]]
  duration <- grep(
    "Duration",
    duration,
    value = TRUE
  )
  duration <- gsub("Duration:", "", duration)
  duration <- strsplit(
    duration,
    ":"
  )[[1]]
  duration <- as.numeric(duration)
  duration <- duration[[1]] * 60 * 60 + duration[[2]] * 60 + duration[[3]]
  names(duration) <- basename(file)
  return(simplify2array(duration))
}

#' Extract height in pixels using ffmpeg
#'
#' The height is based on using the output of \code{ffmpeg} and
#' should therefore work for all ffmpeg supported video formats,
#' but is only implemented for \code{.avi} files.
#' @param file  file name, of the video file
#' @param mc.cores Number of cores to be used when more than one xd file is given.
#'
#' @return named vector with the height (in pixels) of the video(s) and the names are the file names
#' @export
#'
#' @examples
get_height_avi <- function(
    file,
    ffmpeg = par_ffmpeg(),
    mc.cores = par_mc.cores()
) {

  file <- normalizePath(file)
  if ((length(file) == 1) && dir.exists(file)) {
    height <- get_height_avi(
      file = list.files(
        file,
        pattern = "\\.avi$",
        full.names = TRUE
      ),
      ffmpeg = ffmpeg,
      mc.cores = mc.cores
    )
    return(simplify2array(height))
  } else if (length(file) > 1) {
    height <- parallel::mclapply(
      file,
      FUN = get_height_avi,
      ffmpeg = ffmpeg,
      mc.cores = mc.cores
    )
    return(simplify2array(height))
  }

  arguments <- paste0(
    " -i '", file, "'"
  )
  output <- suppressWarnings(
    system2(
      command = ffmpeg,
      args = arguments,
      stderr = TRUE
    )
  )
  height <- grep(
    "([0-9]+)x([0-9]+)",
    output,
    value = TRUE
  )
  height <- strsplit(
    height,
    ","
  )[[1]]
  height <- grep(
    "([0-9]+)x([0-9]+)",
    height,
    value = TRUE
  )
  height <- gsub(" ", "x", trimws(height[-1]))
  height <- strsplit(
    height,
    "x"
  )[[1]]
  height <- as.numeric(height[[2]])
  names(height) <- basename(file)
  return(simplify2array(height))
}

#' Extract width in pixels using ffmpeg
#'
#' The width is based on using the output of \code{ffmpeg} and
#' should therefore work for all ffmpeg supported video formats,
#' but is only implemented for \code{.avi} files.
#' @param file  file name, of the video file
#' @param mc.cores Number of cores to be used when more than one xd file is given.
#'
#' @return named vector with the width (in pixels) of the video(s) and the names are the file names
#' @export
#'
#' @examples
get_width_avi <- function(
    file,
    ffmpeg = par_ffmpeg(),
    mc.cores = par_mc.cores()
) {

  file <- normalizePath(file)
  if ((length(file) == 1) && dir.exists(file)) {
    width <- get_width_avi(
      file = list.files(
        file,
        pattern = "\\.avi$",
        full.names = TRUE
      ),
      ffmpeg = ffmpeg,
      mc.cores = mc.cores
    )
    return(simplify2array(width))
  } else if (length(file) > 1) {
    width <- parallel::mclapply(
      file,
      FUN = get_width_avi,
      ffmpeg = ffmpeg,
      mc.cores = mc.cores
    )
    return(simplify2array(width))
  }

  arguments <- paste0(
    " -i '", file, "'"
  )
  output <- suppressWarnings(
    system2(
      command = ffmpeg,
      args = arguments,
      stderr = TRUE
    )
  )
  width <- grep(
    "([0-9]+)x([0-9]+)",
    output,
    value = TRUE
  )
  width <- strsplit(
    width,
    ","
  )[[1]]
  width <- grep(
    "([0-9]+)x([0-9]+)",
    width,
    value = TRUE
  )
  width <- gsub(" ", "x", trimws(width[-1]))
  width <- strsplit(
    width,
    "x"
  )[[1]]
  width <- as.numeric(width[[1]])
  names(width) <- basename(file)
  return(simplify2array(width))
}

