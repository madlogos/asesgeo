context("Test geohost")
if (! ".asesEnv" %in% ls(all.names=TRUE, envir=globalenv()))
    .asesEnv <<- new.env(parent=globalenv())

test_that("check my ip", {
    asesgeo:::check_my_ip()
    expect_true(is.character(getOption('my_ip')))
    expect_true(inherits(attr(getOption('my_ip'), "updated"), "POSIXt"))
})


test_that("geohost with ipify.org", {
    app <- "ipify"
    if (is.null(.asesEnv$API_KEY[[app]]))
        skip("You are recommended to test it separately.")

    in1 <- c("61.135.169.81", "12.215.42.19")
    res1 <- suppressWarnings(geohost(in1, api='ipify', output='all'))

    expect_equal(res1[["country"]], c("CN", "US"))
    expect_equal(names(res1), c("ip", "country", "region", "city", "lat", "lng",
                                "postalCode", "timezone", "geonameId"))
})

test_that("geohost with ipinfo.io", {
    app <- "ipinfo"
    if (is.null(.asesEnv$API_KEY[[app]]))
        skip("You are recommended to test it separately.")

    in1 <- c("61.135.169.81", "12.215.42.19")
    res1 <- suppressWarnings(geohost(in1, api='ipinfo', output='all'))

    expect_equal(res1[["country"]], c("CN", "US"))
    expect_equal(names(res1), c("ip", "city", "region", "country", "lat", "lng",
                                "org", "hostname", "postal"))
})

test_that("geohost with ipstack.com", {
    app <- "ipstack"
    if (is.null(.asesEnv$API_KEY[[app]]))
        skip("You are recommended to test it separately.")

    in1 <- c("61.135.169.81", "12.215.42.19")
    res1 <- geohost(in1, api='ipstack', output='all')

    expect_equal(res1[["country"]], c("CN", "US"))
    expect_equal(names(res1), c("ip", "type", "continent_code", "continent_name",
                                "country", "country_name", "region", "region_name",
                                "city", "zip", "lat", "lng", "capital", "language",
                                "language_name", "language_native", "country_flag"))
})
