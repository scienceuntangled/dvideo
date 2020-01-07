context("Video functions")
test_that("player functions work", {
    expect_true(file.exists(system.file("extdata/js/vid.js", package = "dvideo")))
    expect_s3_class(dv_video_js(), "shiny.tag.list")
    expect_s3_class(dv_video_player(id = "blah", type = "youtube"), "shiny.tag")
})
