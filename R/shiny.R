#' Launch a Shiny app to help with video synchronisation
#'
#' @param dvw string or datavolley: either the path to a dvw file (which will be read by \code{\link[datavolley]{read_dv}}) or a datavolley object as returned by that function
#' @param video_file string: optionally, the path to the video file. If not supplied (or \code{NULL}) the video file specified in the dvw file will be used
#' @param launch_browser logical: if \code{TRUE}, launch the app in the system's default web browser (passed to \code{\link[shiny]{runApp}}'s \code{launch.browser} parameter)
#' @param ... : extra parameters passed to \code{\link[datavolley]{read_dv}} (if \code{dvw} is a provided as a string)
#'
#' @seealso \code{\link[datavolley]{read_dv}}
#'
#' @export
dv_shiny_video_sync <- function(dvw, video_file = NULL, launch_browser = TRUE, ...) {
    assert_that(is.flag(launch_browser), !is.na(launch_browser))
    dots <- list(...)
    if (is.string(dvw)) {
        if (!"skill_evaluation_decode" %in% names(dots)) dots$skill_evaluation_decode <- "guess"
        rgs <- dots
        rgs$filename <- dvw
        dvw <- do.call(datavolley::read_dv, rgs)
    } else {
        if (!inherits(dvw, "datavolley")) stop("dvw should be a datavolley object or the path to a .dvw file")
    }
    ## deal with video_file parm
    if (is.null(dvw$meta$video)) dvw$meta$video <- tibble(camera = character(), file = character())
    if (!is.null(video_file)) {
        dvw$meta$video <- tibble(camera = "Camera0", file = fs::path_real(video_file))
    }
    if (nrow(dvw$meta$video) > 1) {
        stop("multiple video files have been specified in the dvw file metadata, can't handle this yet")
    } else if (nrow(dvw$meta$video) < 1) {
        stop("no video files specified, either in the dvw file or via the video_file parameter")
    } else {
        if (!file.exists(dvw$meta$video$file)) stop("specified video file (", dvw$meta$video$file, ") does not exist. Perhaps specify the local path via the video_file parameter?")
    }
    ## finally the shiny app
    shiny_data <- list(dvw = dvw, dv_read_args = dots)
    this_app <- shiny::shinyApp(ui = dv_shiny_video_sync_ui(data = shiny_data), server = dv_shiny_video_sync_server)
    myrunapp <- function(app, data, ...) {
        ## uurgh
        .GlobalEnv$SHINY_DATA <- data
        on.exit(rm("SHINY_DATA", envir = .GlobalEnv))
        shiny::runApp(app, ...)
    }
    myrunapp(this_app, data = shiny_data, display.mode = "normal", launch.browser = launch_browser)
}
