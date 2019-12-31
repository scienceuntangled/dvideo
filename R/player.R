#' Inject javascript for an HTML video player
#'
#' @param youtube logical: set to \code{TRUE} to include the Youtube API javascript
#'
#' @return A tag list
#'
#' @seealso \code{\link{dv_video_playlist}}
#'
#' @export
dv_video_js <- function(youtube = FALSE) {
    assert_that(is.flag(youtube), !is.na(youtube))
    js <- readLines(system.file("extdata/js/vid.js", package = "dvideo"))
    js <- paste(js, collapse = "\n")
    out <- list(tags$script(HTML(js)), if (youtube) tags$script(src = "https://www.youtube.com/iframe_api"))
    tagList(Filter(Negate(is.null), out))
}

#' Video player tag element
#'
#' @param id string: the id of the tag
#' @param controls logical: add "previous", "next", and "stop" buttons
#' @param ... : other parameters to be passed to `tags$div`
#'
#' @return HTML tags. The outermost element is a div with id `paste0(id, "_container")`, with the player and optionally buttons nested within it.
#'
#' @examples
#' \dontrun{
#'   library(shiny)
#'
#'   ## hand-crafted playlist for this example
#'   playlist <- data.frame(video_src = "xL7qEpOdtio",
#'                          start_time = c(5417, 7252, 6222, 7656, 7369),
#'                          duration = 8,
#'                          type = "youtube")
#'   shinyApp(
#'       ui = fluidPage(
#'           dv_video_js(youtube = TRUE),
#'           dv_video_player(id = "yt_player", controls = TRUE,
#'                           style = "height: 480px; background-color: black;"),
#'           tags$button("Go", onclick = dv_playlist_as_onclick(playlist, "yt_player"))
#'       ),
#'       server = function(input, output) {},
#'   )
#' }
#'
#' @export
dv_video_player <- function(id, controls = FALSE, ...) {
    assert_that(is.flag(controls), !is.na(controls))
    plyr <-     do.call(tags$div, c(list(id = id), list(...)))
    if (controls) {
        tags$div(id = paste0(id, "_container"), plyr, tags$div(tags$button("Prev", onclick = "dvjs_video_prev();"), tags$button("Next", onclick = "dvjs_video_next();"), tags$button("Stop", onclick = "dvjs_video_stop();")))
    } else {
        tags$div(id = paste0(id, "_container"), plyr)
    }
}
