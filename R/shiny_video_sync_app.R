## not exported

dv_shiny_video_sync_ui <- function(data) {
    ## some startup stuff
    running_locally <- !nzchar(Sys.getenv("SHINY_PORT"))
    video_src <- data$dvw$meta$video$file[1]

    have_lighttpd <- FALSE
    video_server_port <- sample.int(4000, 1) + 8000 ## random port from 8001
    if (.Platform$OS.type == "unix") {
        tryCatch({
            chk <- sys::exec_internal("lighttpd", "-version")
            have_lighttpd <- TRUE
        }, error = function(e) warning("could not find the lighttpd executable, install it with e.g. 'apt install lighttpd'. Using \"standalone\" video option"))
    }
    video_serve_method <- if (have_lighttpd) "lighttpd" else "servr"
    if (video_serve_method == "lighttpd") {
        ## build config file to pass to lighttpd
        lighttpd_conf_file <- tempfile(fileext = ".conf")
        cat("server.document-root = \"", dirname(video_src), "\"\nserver.port = \"", video_server_port, "\"\n", sep = "", file = lighttpd_conf_file, append = FALSE)
        lighttpd_pid <- sys::exec_background("lighttpd", c("-D", "-f", lighttpd_conf_file), std_out = FALSE) ## start lighttpd not in background mode
        lighttpd_cleanup <- function() {
            message("cleaning up lighttpd")
            try(tools::pskill(lighttpd_pid), silent = TRUE)
        }
        onStop(function() try({ lighttpd_cleanup() }, silent = TRUE))
    } else {
        ## start servr instance serving from the video source directory
        servr::httd(dir = dirname(video_src), port = video_server_port)
        onStop(function() {
            message("cleaning up servr")
            servr::daemon_stop()
        })
    }
    video_server_base_url <- paste0("http://localhost:", video_server_port)
    message(paste0("video server ", video_serve_method, " on port: ", video_server_port))
    fluidPage(theme=if (running_locally) "spacelab.css" else "https://cdnjs.cloudflare.com/ajax/libs/bootswatch/3.3.7/spacelab/bootstrap.min.css",
              if (!running_locally) htmltools::htmlDependency("bootstrap", "3.3.7",
                                                              src = c(href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/"),
                                                              script = "js/bootstrap.min.js", stylesheet = "css/bootstrap.min.css"),
              shinyjs::useShinyjs(),
              tags$head(tags$style("body{font-size:15px} .well{padding:15px;} .myhidden {display:none;} table {font-size: small;} #headerblock h1,#headerblock h2,#headerblock h3,#headerblock h4 {font-weight: normal; color:#ffffff;} h2, h3, h4 {font-weight: bold;} .shiny-notification { height: 100px; width: 400px; position:fixed; top: calc(50% - 50px); left: calc(50% - 200px); }"),
                        ##tags$style("table {font-size: 10px; line-height: 1.0;"),
                        tags$script("$(document).on('shiny:sessioninitialized', function(){ document.getElementById('main_video').addEventListener('focus', function(){ this.blur(); }, false); });"),
                        tags$script("$(document).on('keypress', function (e) { Shiny.onInputChange('cmd', e.which + '@' + new Date().getTime()); });"),
                        tags$script("$(document).on('keydown', function (e) { Shiny.onInputChange('arrows', e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.which + '@' + new Date().getTime()); });"),
                        tags$script("$(document).on('shiny:sessioninitialized',function(){ Shiny.onInputChange('window_height', $(window).innerHeight()); Shiny.onInputChange('window_width', $(window).innerWidth()); });"),
                        tags$script("var rsztmr; $(window).resize(function() { clearTimeout(rsztmr); rsztmr = setTimeout(doneResizing, 500); }); function doneResizing(){ Shiny.onInputChange('window_height', $(window).innerHeight()); Shiny.onInputChange('window_width', $(window).innerWidth()); }"),
                        tags$title("Volleyball video sync")
                        ),
              fluidRow(id = "headerblock", style = "border-radius:4px;padding:10px;margin-bottom:10px;min-height:160px;color:white;background: #ffffff url(\"https://untan.gl/images/bgrev.jpg\") 0 0/100% auto no-repeat;", ## background image in header block
                       column(6, offset = 2, tags$h2("Volleyball video sync")),
                       column(4,tags$div(style="float:right;padding:10px;", tags$a(href = "https://untan.gl", tags$img(style = "width:16em;max-width:100%", src = "https://untan.gl/images/su_title-w.png"))))
                       ),
              fluidRow(column(8, tags$video(id = "main_video", style = "border: 1px solid black; width: 90%;", src = file.path(video_server_base_url, basename(video_src)), controls = "controls", autoplay = "false"),
                              fluidRow(column(3, actionButton("all_video_from_clock", label = "Open video/clock time operations menu")),
                                       column(3, uiOutput("save_file_ui")),
                                       column(4, offset = 2, uiOutput("current_event"))),
                              fluidRow(column(6, tags$p(tags$strong("Keyboard controls")), tags$ul(tags$li("[r or 5] sync selected event video time"),
                                                                                          tags$li("[8] move to previous skill row"),
                                                                                          tags$li("[2] move to next skill row")
                                                                                          )),
                                       column(6, tags$p(tags$strong("Video controls")), sliderInput("playback_rate", "Playback rate:", min = 0.1, max = 2.0, value = 1.0, step = 0.1), tags$ul(tags$li("[e or 6] forward 2s, [E or ^] forward 10s"), tags$li("[w or 4] backward 2s, [W or $] backward 10s"), tags$li("[q or 0] pause video"), tags$li("[g or #] go to currently-selected event")))
                                       )),
                       column(4, DT::dataTableOutput("playslist", width = "90%"))
                       )
              )
    ## find negative time intervals and fix them
}

if (getRversion() >= "2.15.1")  utils::globalVariables("SHINY_DATA") ## avoid check complaints

dv_shiny_video_sync_server <- function(input, output, session) {
    things <- reactiveValues(dvw = SHINY_DATA$dvw, plays_row_to_select = NULL)
    video_state <- reactiveValues(paused = FALSE)
    handlers <- reactiveValues()
    done_first_playlist_render <- FALSE
    video_time_decimal_places <- 0L
    debug <- 0L
    `%eq%` <- function (x, y) x == y & !is.na(x) & !is.na(y)
    plays_cols_to_show <- c("clock_time", "video_time", "set_number", "home_team_score", "visiting_team_score", "code")

    observeEvent(input$video_time, {
        ##cat("input$video_time: "); cat(str(input$video_time))
        temp <- strsplit(input$video_time, split = "&", fixed = TRUE)[[1]]
        this_handler_id <- temp[2]
        ##cat("running handler: ", this_handler_id, "\n")
        handlers[[this_handler_id]](as.numeric(temp[1]))
        ## then clear it
        ##handlers[[this_handler_id]] <- NULL
    })

    observeEvent(input$all_video_from_clock, {
        current_video_time <- selected_event()$video_time
        current_clock_time <- selected_event()$time
        all_clock_times <- things$dvw$plays$time
        current_is_no_good <- is.null(current_video_time) || is.na(current_video_time) || is.null(current_clock_time) || is.na(current_clock_time)
        showModal(modalDialog(
            title = "Video times from clock times",
            easyClose = TRUE, size = "l",
            if (all(is.na(all_clock_times))) {
                tags$div(class = "alert alert-danger", "Your file has no clock times, so this tool can't do anything.")
            } else {
                tags$div(tags$h4("Options:"),
                         fluidRow(column(8, tags$strong("Set missing video times"), "of events based on their clock times, and the video and clock time of the currently-selected event."),
                                  column(4, if (current_is_no_good) tags$div(class = "alert alert-danger", "The currently-selected event needs to have a video time AND clock time set before using this option.") else actionButton("infer_missing_video_from_current", label = tags$span("Infer MISSING video times", tags$br(), "relative to the current event")))),
                         tags$hr(),
                         fluidRow(column(8, tags$strong("Set the video times of ALL events"), "based on their clock times, and the video and clock time of the currently-selected event. This applies to ALL events, whether they are missing their video time or not."),
                                  column(4, if (current_is_no_good) tags$div(class = "alert alert-danger", "The currently-selected event needs to have a video time AND clock time set before using this option.") else actionButton("infer_all_video_from_current", label = tags$span("Infer ALL video times", tags$br(), "relative to the current event")))),
                         tags$hr(),
                         fluidRow(column(8, tags$strong("Set missing video times"), "of events based on their clock times, and the video and clock time of surrounding events."),
                                  column(4, "Not implemented yet.")##actionButton("infer_missing_video_from_surrounding", label = tags$span("Infer MISSING video times", tags$br(), "relative to surrounding events")))
                                  ),
                         tags$hr(),
                         fluidRow(column(8, tags$strong("Set the clock time"), "of the currently-selected event."),
                                  column(4, actionButton("set_selected_clock_time", label = tags$span("Set clock time of", tags$br(), "selected event.")))),
                         tags$hr(),
                         fluidRow(column(8, tags$strong("Set missing clock times"), "of events based on their video times, and the video and clock time of the currently-selected event."),
                                  column(4, if (current_is_no_good) tags$div(class = "alert alert-danger", "The currently-selected event needs to have a video time AND clock time set before using this option.") else actionButton("infer_missing_clock_from_current", label = tags$span("Infer MISSING clock times", tags$br(), "relative to the current event")))),
                         tags$hr(),
                         fluidRow(column(8, tags$strong("Set the clock times of ALL events"), "based on their video times, and the video and clock time of the currently-selected event. This applies to ALL events, whether they are missing their clock time or not."),
                                  column(4, if (current_is_no_good) tags$div(class = "alert alert-danger", "The currently-selected event needs to have a video time AND clock time set before using this option.") else actionButton("infer_all_clock_from_current", label = tags$span("Infer ALL clock times", tags$br(), "relative to the current event"))))
                         )
            }
        ))
    })

    ## video/clock time sync functions
    observe({
        if (isTruthy(input$infer_all_video_from_current) || isTruthy(input$infer_missing_video_from_current)) {
            isolate({
                removeModal()
                this_clock_time <- selected_event()$time
                this_video_time <- selected_event()$video_time
                if (is.null(this_clock_time) || is.na(this_clock_time) || is.null(this_video_time) || is.na(this_video_time)) {
                    stop("selected event is missing video or clock time")
                }
                clock_time_diff <- difftime(things$dvw$plays$time, this_clock_time, units = "secs")
                midx <- if (isTruthy(input$infer_all_video_from_current)) rep(TRUE, nrow(things$dvw$plays)) else is.na(things$dvw$plays$video_time)
                new_video_time <- this_video_time + clock_time_diff[midx]
                things$dvw$plays$video_time[midx] <- round(new_video_time, digits = video_time_decimal_places)
            })
        }
    })
    observe({
        if (isTruthy(input$infer_all_clock_from_current) || isTruthy(input$infer_missing_clock_from_current)) {
            isolate({
                removeModal()
                this_time <- selected_event()$time
                this_video_time <- selected_event()$video_time
                if (is.null(this_time) || is.na(this_time) || is.null(this_video_time) || is.na(this_video_time)) {
                    stop("selected event is missing video or clock time")
                }
                video_time_diff <- things$dvw$plays$video_time - this_video_time
                midx <- if (isTruthy(input$infer_all_clock_from_current)) rep(TRUE, nrow(things$dvw$plays)) else is.na(things$dvw$plays$time)
                things$dvw$plays$time[midx] <- this_time + video_time_diff[midx]
            })
        }
    })

    observeEvent(input$set_selected_clock_time, {
        removeModal()
        if (is.null(selected_event())) {
            showModal(modalDialog(title = "Error", tags$div(class = "alert alert-danger", "No event selected.")))
        } else {
            showModal(modalDialog(
                title = "Set clock time of selected event",
                easyClose = TRUE, size = "l",
                tags$div(shinyTime::timeInput("selected_clocktime", label = "Time:", value = if (!is.na(selected_event()$time)) selected_event()$time else NULL), actionButton("do_set_clocktime", "Set time"))
            ))
        }
    })
    observeEvent(input$do_set_clocktime, {
        removeModal()
        ridx <- input$playslist_rows_selected
        if (!is.null(ridx) && !is.na(ridx)) {
            ##cat("x time: "); cat(str(things$dvw$plays$time))
            tm <- input$selected_clocktime
            ##cat("time:"); cat(str(tm))
            if (inherits(things$dvw$plays$time, "POSIXct")) tm <- as.POSIXct(tm, tz = lubridate::tz(things$dvw$plays$time))
            ##cat("time cast:"); cat(str(tm))
            things$dvw$plays$time[ridx] <- tm
        }
    })

    ## sync the selected event to the current video time
    sync_single_video_time <- function() {
        myfid <- UUIDgenerate()
        handlers[[myfid]] <- handler_sync_single_video_time
        do_video("get_time_fid", myfid)
    }
    handler_sync_single_video_time <- function(tm) {
        ##cat("handler_sync_single_video_time: "); cat(tm); cat("\n")
        ridx <- input$playslist_rows_selected
        ##cat("rowidx: ", ridx, "\n")
        if (!is.null(ridx)) {
            things$dvw$plays$video_time[ridx] <- round(tm, digits = video_time_decimal_places)
            ## advance to the next skill row
            if (ridx < nrow(things$dvw$plays)) {
                next_skill_row <- find_next_skill_row(ridx)
                if (length(next_skill_row) > 0) things$plays_row_to_select <- next_skill_row
            }
        }
        NULL
    }

    selected_event <- reactive({
        if (length(input$playslist_rows_selected) == 1) {
            things$dvw$plays[input$playslist_rows_selected, ]
        } else {
            NULL
        }
    })

    output$current_event <- renderUI({
        tags$span(style = "font-size: large;", tags$strong("Current: "), selected_event()$code)
    })

    observe({
        if (!is.null(things$dvw) && nrow(things$dvw$plays) > 0) {
            things$dvw$plays <- mutate(things$dvw$plays, clock_time = format(.data$time, "%H:%M:%S"))
        }
    })

    ## the plays display in the RHS table
    output$playslist <- DT::renderDataTable({
        isolate(mydat <- things$dvw$plays) ## render once, then isolate from further renders - will be done by replaceData below
        if (!is.null(input$window_height) && !is.na(input$window_height)) {
            plh <- input$window_height*0.6
        } else {
            plh <- 200
        }
        if (!is.null(mydat)) {
            isolate({
                first_skill_row <- find_next_skill_row(-1)
                sel <- list(mode = "single")
                if (length(first_skill_row) > 0) {
                    sel$target <- "row"
                    sel$selected <- first_skill_row
                }
            })
            DT::datatable(names_first_to_capital(mydat[, plays_cols_to_show, drop = FALSE]), rownames = FALSE, ##colnames = NULL,
                          extensions = "Scroller",
                          selection = sel, options = list(scroller = TRUE, lengthChange = FALSE, sDom = '<"top">t<"bottom">rlp', paging = TRUE, "scrollY" = paste0(plh, "px")))
        } else {
            NULL
        }
    }, server = TRUE)
    playslist_proxy <- DT::dataTableProxy("playslist")
    observe({
        if (!is.null(things$plays_row_to_select)) {
            DT::selectRows(playslist_proxy, things$plays_row_to_select)
        }
    })

    observe({
        DT::replaceData(playslist_proxy, data = things$dvw$plays[, plays_cols_to_show, drop = FALSE], rownames = FALSE, clearSelection = "none")##, resetPaging = FALSE)
        ## and scroll to selected row
        ##dojs(sprintf("$('#playslist').find('.dataTable').DataTable().row(%s).scrollTo();", max(0, things$plays_row_to_select-1)))
        scrollto <- if (!is.null(things$plays_row_to_select) && !is.na(things$plays_row_to_select)) max(things$plays_row_to_select-1, 0) else 0
        scrollto <- max(0, scrollto - 5)
        dojs(sprintf("$('#playslist').find('.dataTable').DataTable().scroller.toPosition(%s);", scrollto))
        ##dojs(sprintf("$('#playslist').find('.dataTable').DataTable().row(%s).node().scrollIntoView();", max(0, things$plays_row_to_select-1)))
        ##dojs(sprintf("console.dir($('#playslist').find('.dataTable').DataTable().row(%s).node())", max(0, things$plays_row_to_select-1)))
        ##dojs(sprintf("$('#playslist').find('.dataTables_scroll').animate({ scrollTop: $('#playslist').find('.dataTable').DataTable().row(%s).node().offsetTop }, 2000);", max(0, things$plays_row_to_select-1)))
    })

    find_next_skill_row <- function(current_row_idx = NULL) {
        if (is.null(current_row_idx)) current_row_idx <- input$playslist_rows_selected
        next_skill_row <- which(!is.na(things$dvw$plays$skill))
        head(next_skill_row[next_skill_row > current_row_idx], 1)
    }

    find_prev_skill_row <- function(current_row_idx = NULL) {
        if (is.null(current_row_idx)) current_row_idx <- input$playslist_rows_selected
        next_skill_row <- which(!is.na(things$dvw$plays$skill))
        tail(next_skill_row[next_skill_row < current_row_idx], 1)
    }


    observeEvent(input$playback_rate, {
        if (!is.null(input$playback_rate)) do_video("playback_rate", input$playback_rate)
    })

    observeEvent(input$cmd, {
        mycmd <- sub("@.*", "", input$cmd)
        if (!is.null(mycmd)) {
            ## mycmd comes in as a character representation of the ascii code like "65" or "32"
            mykey <- intToUtf8(as.numeric(mycmd))
            ## note that if cmdbox is an INPUT and focus is cmdbox then the document$keypress event doesn't get fired, because it gets grabbed by the cmdbox event handler
            ignore_keys <- NULL ## placeholder for keys handled elsewhere in code (e.g. 37, 39 might not trigger here, may depend on browser)
            if (debug > 1) cat("input: ", mycmd, "\n")
            if (mycmd %in% ignore_keys) {
                if (debug > 1) cat(" (ignored)")
            } else if (mycmd %eq% "38") {
                ## 38 (up arrow)
            } else if (mycmd %eq% "40") {
                ## 40 (down arrow)
            } else if (mycmd %eq% "8") {
                ## backspace
            } else if (mycmd %eq% "46") {
                ## delete key
            } else if (mycmd %in% utf8ToInt("8")) {
                ## prev skill row
                psr <- find_prev_skill_row()
                if (length(psr) > 0) things$plays_row_to_select <- psr
            } else if (mycmd %in% utf8ToInt("2")) {
                ## next skill row
                nsr <- find_next_skill_row()
                if (length(nsr) > 0) things$plays_row_to_select <- nsr
            } else if (mycmd %in% utf8ToInt("qQ0")) {
                do_video("toggle_pause")
            } else if (mycmd %in% utf8ToInt("gG#")) {
                ## video go to currently-selected event
                ev <- selected_event()
                if (!is.null(ev)) do_video("set_time", ev$video_time)
            } else if (mycmd %in% utf8ToInt("wWeE46$^")) {
                ## video forward/backward nav
                vidcmd <- if (tolower(mykey) %in% c("w", "4", "$")) "rew" else "ff"
                dur <- if (mykey %in% c("E", "$", "W", "^")) 10 else 2
                do_video(vidcmd, dur)
            ##} else if (mycmd %in% as.character(33:126)) {
            ##    cat("queued: ", mycmd, "\n")
            ##    ## add to cmd queue
            ##    things$cmd <- paste0(things$cmd, intToUtf8(mycmd))
                ##    output$cmdbox <- renderText(things$cmd)
            } else if (mykey %in% c("r", "R", "5")) {
                ## set the video time of the current event
                sync_single_video_time()
            }
            if (debug > 1) cat("\n")
        }
    })

    ## video helper functions
    do_video <- function(what, ..., id = "main_video") {
        getel <- paste0("document.getElementById('", id, "')")
        myargs <- list(...)
        if (what == "pause") {
            if (video_state$paused) {
                dojs(paste0(getel, ".play();"))
                video_state$paused <- FALSE
            } else {
                dojs(paste0(getel, ".pause();"))
                video_state$paused <- TRUE
            }
            NULL
        } else if (what == "toggle_pause") {
            dojs(paste0("if (", getel, ".paused == true) { ", getel, ".play(); } else { ", getel, ".pause(); }"))
        } else if (what == "get_time") {
            dojs(paste0("Shiny.onInputChange('video_time', ", getel, ".currentTime)"))
        } else if (what == "get_time_fid") {
            dojs(paste0("Shiny.onInputChange('video_time', ", getel, ".currentTime + '&", myargs[[1]], "')"))
        } else if (what == "set_time") {
            dojs(paste0(getel, ".currentTime='", myargs[[1]], "';"))
        } else if (what == "rew") {
            dojs(paste0(getel, ".currentTime=", getel, ".currentTime - ", myargs[[1]], ";"))
        } else if (what == "ff") {
            dojs(paste0(getel, ".currentTime=", getel, ".currentTime + ", myargs[[1]], ";"))
        } else if (what == "playback_rate") {
            dojs(paste0(getel, ".playbackRate=", myargs[[1]], ";"))
        } else {
            NULL
        }
    }

    ## save file
    output$save_file_ui <- renderUI({
    if (is.null(things$dvw)) {
        NULL
    } else {
        downloadButton("save_file_button", "Save file")
    }
    })
    output$save_file_button <- downloadHandler(
        filename = reactive(
            if (!is.null(things$dvw$meta$filename) && !is.na(things$dvw$meta$filename)) basename(things$dvw$meta$filename) else "myfile.dvw"
        ),
        content = function(file) dv_write(things$dvw, file = file)##, contentType = "text/csv")
    )

}

dojs <- function(jscmd) {
    ## cat("js: ", jscmd, "\n")
    shinyjs::runjs(jscmd)
}

names_first_to_capital <- function(x, fun) {
    setNames(x, var2fc(if (missing(fun)) names(x) else vapply(names(x), fun, FUN.VALUE = "", USE.NAMES = FALSE)))
}
var2fc <- function(x) {
    vapply(x, function(z) gsub("_", " ", paste0(toupper(substr(z, 1, 1)), substr(z, 2, nchar(z)))), FUN.VALUE = "", USE.NAMES = FALSE)
}
