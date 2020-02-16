#' Make a self-contained video file from a playlist
#'
#' Requires that ffmpeg be available on the system path. Note that the processing of each clip is done inside of a `future_lapply` call, and so you can have this part of the processing done in parallel by setting an appropriate futures plan before calling this function.
#'
#' This function is experimental. In particular it is unlikely to work well with all video formats, and especially if the playlist comprises clips from different videos with different resolution/encoding/etc.
#'
#' @param playlist data.frame: a playlist as returned by `dv_video_playlist`. Note that only local video sources are supported
#' @param filename string: file to write to. If not specified (or `NULL`), a file in the temporary directory will be created. If `filename` exists, it will be overwritten. The extension of `filename` will determine the output format
#'
#' @return Filename of the created file.
#'
#' @seealso \code{\link{dv_video_playlist}}
#' @examples
#' \dontrun{
#'   my_playlist <- dv_video_playlist(..., type = "local")
#'   video_file <- dv_create_video(my_playlist)
#'   browseURL(video_file)
#'
#'   ## run in parallel
#'   library(future.apply)
#'   plan(multisession)
#'   video_file <- dv_create_video(my_playlist)
#' }
#'
#' @export
dv_create_video <- function(playlist, filename) {
    if (missing(filename) || is.null(filename)) filename <- tempfile(fileext = ".mp4")
    ## find ffmpeg
    chk <- sys::exec_internal("ffmpeg", "-version")
    if (chk$status != 0) stop("could not find the ffmpeg executable")
    tempfiles <- future.apply::future_lapply(seq_len(nrow(playlist)), function(ri) {
        outfile <- tempfile(fileext = paste0(".", fs::path_ext(playlist$video_src[ri])))
        if (file.exists(outfile)) unlink(outfile)
        infile <- fs::path_real(playlist$video_src[ri])
        ##sys::exec_wait("ffmpeg", c("-i", infile, "-ss", playlist$start_time[ri], "-t", playlist$duration[ri], "-q", 0, "-c:a", "copy", outfile)) ## works, but slow because it uses very slow seek method to find the start of the clip
        ##sys::exec_wait("ffmpeg", c("-ss", playlist$start_time[ri], "-i", infile, "-t", playlist$duration[ri], "-q", 0, "-c:a", "copy", outfile)) ## faster but glitchy because keyframes are typically sparse
        ##sys::exec_wait("ffmpeg", c("-i", infile, "-ss", playlist$start_time[ri], "-t", playlist$duration[ri], "-c", "copy", outfile)) ## fast but glitchy because keyframes are typically sparse
        ## reencode
        res <- sys::exec_internal("ffmpeg", c("-ss", playlist$start_time[ri], "-i", infile, "-strict", "-2", "-t", playlist$duration[ri], outfile))
        if (res$status != 0) stop("failed to get video clip, ", rawToChar(res$stderr))
        outfile
    })
    tempfiles <- unlist(tempfiles)
    ## concatentate them
    cfile <- tempfile(fileext = ".txt")
    on.exit(unlink(c(cfile, tempfiles)))
    cat(paste0("file ", tempfiles), file = cfile, sep = "\n")
    if (file.exists(filename)) unlink(filename)
    res <- sys::exec_internal("ffmpeg", c("-safe", 0, "-f", "concat", "-i", cfile, "-c", "copy", filename))
    if (res$status != 0) stop("failed to combine clips, ", rawToChar(res$stderr))
    filename
}
