% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/playlists.R
\name{dv_video_timing}
\alias{dv_video_timing}
\title{Timing to use when creating video playlist}
\usage{
dv_video_timing(...)
}
\arguments{
\item{...}{: named parameters that will override the defaults. Each parameter should be a two-element numeric vector}
}
\value{
A named list, with names corresponding to skills ("Serve", "Reception", etc).
}
\description{
By default, all skills have a timing of \code{c(-5, 3)}, meaning that the video clip will start 5 seconds before the recorded time of the event and end 3 seconds after its recorded time.
}
\examples{

## defaults
dv_video_timing()

## with different settings for serve and reception
dv_video_timing(serve = c(-2, 2), reception = c(-3, 1))

}
\seealso{
\code{\link{dv_video_playlist}}
}
