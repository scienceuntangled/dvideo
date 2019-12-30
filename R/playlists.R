#' Create video playlist
#'
#' @param x data.frame: a datavolleyplays object. Normally this will be a selected subset of the \code{plays} component of a datavolley object (i.e. a selected set of actions that you want the video playlist to contain)
#' @param meta list: either the \code{meta} component of a datavolley object, or a list of such objects, or a data.frame with the columns "match_id" and "video_src". Entries in \code{video_src} should be paths or URLs to the video file associated with the corresponding \code{match_id}
#' @param timing list: a named list giving the relative timing for each skill type. Each element in the list should be named (with the name of the skill, as it appears in \code{x}) and should consist of a two-element numeric vector giving the starting and ending time offset relative to the recorded \code{video_time} of the event. See \code{\link{dv_video_timing}} for further details
#' @param extra_cols character: names of additional columns from \code{x} to include in the returned data frame
#'
#' @return A data.frame with columns \code{src}, \code{start_time}, \code{duration}, plus any extras specified in \code{extra_cols}
#'
#' @export
dv_video_playlist <- function(x, meta, timing = dv_video_timing(), extra_cols = NULL) {
    assert_that(is.data.frame(x))
    if (!is.null(extra_cols)) assert_that(is.character(extra_cols))
    assert_that(has_name(x, c("video_time", "skill", "match_id")))
    if (is.data.frame(meta)) {
        assert_that(has_name(meta, c("match_id", "video_src")))
    } else if (is.list(meta)) {
        if ("match_id" %in% names(meta)) {
            ## this is a single metadata object
            meta <- list(meta)
        }
        ## assume meta is a list of metadata objects
        video_file_from_meta <- function(z) {
            out <- z$meta$video
            if (is.null(out)) out <- z$video
            if (is.null(out)) {
                NA_character_
            } else if (nrow(out) < 1) {
                NA_character_
            } else if (nrow(out) > 1) {
                warning("multiple video files found")
                NA_character_
            } else {
                out$file
            }
        }
        match_id_from_meta <- function(z) {
            out <- z$meta$match_id
            if (is.null(out)) out <- z$match_id
            if (is.null(out) || !nzchar(out)) {
                NA_character_
            } else {
                out
            }
        }
        meta <- bind_rows(lapply(meta, function(z) tibble(match_id = match_id_from_meta(z), video_src = video_file_from_meta(z))))
    } else {
        stop("meta is an unexpected format")
    }
    if (!all(x$match_id %in% meta$match_id)) stop("x contains match_ids that do not appear in meta")
    if (any(is.na(meta$video_src))) {
        missing_vid_matches <- meta$match_id[is.na(meta$video_src)]
        stop("no video for matches with match_id: ", paste(missing_vid_matches, collapse = ", "))
    }
    x <- left_join(x, meta, by = "match_id")
    ## convert timing to a data.frame
    timing <- bind_rows(lapply(names(timing), function(z) tibble(skill = z, start_offset = timing[[z]][1], duration = abs(diff(timing[[z]])))))
    x <- left_join(x, timing, by = "skill")
    x$start_time <- x$video_time + x$start_offset
    x <- x[!is.na(x$skill), ]
    ## TODO check for NA video_time
    x$video_src <- as.character(x$video_src)
    x[, c("video_src", "start_time", "duration", extra_cols)]
}

#' Timing to use when creating video playlist
#'
#' By default, all skills have a timing of `c(-5, 3)`, meaning that the video clip will start 5 seconds before the recorded time of the event and end 3 seconds after its recorded time.
#'
#' @param ... : named parameters that will override the defaults. Each parameter should be a two-element numeric vector
#'
#' @return A named list, with names corresponding to skills ("Serve", "Reception", etc).
#'
#' @seealso \code{\link{dv_video_playlist}}
#'
#' @examples
#'
#' ## defaults
#' dv_video_timing()
#'
#' ## with different settings for serve and reception
#' dv_video_timing(serve = c(-2, 2), reception = c(-3, 1))
#'
#' @export
dv_video_timing <- function(...) {
    skills <- c("Serve", "Reception", "Set", "Attack", "Block", "Dig", "Freeball")
    out <- rep(list(c(-5, 3)), length(skills))
    names(out) <- skills
    ## override with any user-specified parms
    user <- list(...)
    if (length(user) > 0 && is.null(names(user))) {
        warning("inputs to dv_video_timing must be named")
    } else {
        if (!all(nzchar(names(user)))) warning("all inputs to dv_video_timing must be named")
        user <- user[nzchar(names(user))]
        ## Convert To Title Case
        names(user) <- paste0(toupper(substr(names(user), 1, 1)), tolower(substr(names(user), 2, nchar(names(user)))))
        for (usr in names(user)) out[[usr]] <- user[[usr]]
    }
    out
}
