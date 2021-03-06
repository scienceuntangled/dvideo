% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/player.R
\name{dv_video_player}
\alias{dv_video_player}
\title{Video player tag element}
\usage{
dv_video_player(id, type, controls = FALSE, ...)
}
\arguments{
\item{id}{string: the id of the tag}

\item{type}{string: either "youtube" or "local"}

\item{controls}{logical: add "previous", "next", "pause", and "stop" buttons. If \code{controls} is an object of class \code{shiny.tag} (created by \code{htmltools::tags}) or \code{shiny.tag.list} (\code{htmltools::tagList}) then it will be appended to the controls}

\item{...}{: other attributes of the player element (passed to the player \code{tags$div} call for youtube or \code{tags$video} for local)}
}
\value{
HTML tags. The outermost element is a div with id \code{paste0(id, "_container")}, with the player and optionally buttons nested within it.
}
\description{
Video player tag element
}
\examples{
\dontrun{
  library(shiny)

  ## hand-crafted playlist for this example
  playlist <- data.frame(video_src = "xL7qEpOdtio",
                         start_time = c(5417, 7252, 6222, 7656, 7369),
                         duration = 8,
                         type = "youtube")
  shinyApp(
      ui = fluidPage(
          dv_video_js(youtube = TRUE),
          dv_video_player(id = "yt_player", type = "youtube", controls = TRUE,
                          style = "height: 480px; background-color: black;"),
          tags$button("Go", onclick = dv_playlist_as_onclick(playlist, "yt_player"))
      ),
      server = function(input, output) {},
  )

  shinyApp(
      ui = fluidPage(
          dv_video_js(youtube = TRUE),
          dv_video_player(id = "yt_player", type = "youtube",
                          style = "height: 480px; background-color: black;",
                          controls = tags$button("Go",
                                       onclick = dv_playlist_as_onclick(playlist, "yt_player")))
      )),
      server = function(input, output) {},
  )
}

}
