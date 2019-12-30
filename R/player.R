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
