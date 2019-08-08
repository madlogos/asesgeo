context("Test utils")

test_that("out of china", {
    expect_equal(suppressWarnings(unname(isOutOfChina(c(190, 90), c(21, 27)))),
                 c(NA, TRUE))
    expect_true(isOutOfChina(-50, -90))
    expect_false(isOutOfChina(30, 105))
    expect_equal(unname(isOutOfChina(NA, 9)), NA)
})

test_that("transform lat and lon", {
    expect_equal(asesgeo:::transformLat(c(0, 30), c(0, 105)), 
                 c(-100, 2658.186835108))
    expect_equal(asesgeo:::transformLon(c(0, 30), c(0, 105)), 
                 c(300, 1045.54772256))
})

test_that("format coord args", {
    in1 <- matrix(c(0, 30, 0, 105), ncol=2)
    out1 <- data.frame(lat=c(0, 30), lon=c(0, 105))
    in2 <- matrix(c(0, 105, 0, 30), ncol=2)
    in3 <- matrix(c(0, 30, NA, 105), ncol=2)
    out3 <- data.frame(lat=c(NA, 30), lon=c(NA, 105))
    
    expect_equal(asesgeo:::formatCoordArgs(in1), out1)
    expect_warning(asesgeo:::formatCoordArgs(in2), "within \\[-180, 180\\]")
    expect_equal(asesgeo:::formatCoordArgs(in3), out3)
})

test_that("getCoordArgs vector", {
    out1 <- data.frame(lat=1, lon=2)
    in1 <- 1:2
    in2 <- 1:3
    out21 <- data.frame(lat=1:3, lon=1:3)
    out22 <- data.frame(lat=c(1:2, NA), lon=c(1:2, NA))

    expect_equal(asesgeo:::getCoordArgs(1, 2), out1)
    expect_equal(asesgeo:::getCoordArgs(1, 2, 3), out1)
    expect_equal(asesgeo:::getCoordArgs(in2, in2), out21)
    expect_equal(asesgeo:::getCoordArgs(in2, in1), out22)
    expect_error(asesgeo:::getCoordArgs(1), "how to pair")
})

test_that("getCoordArgs list", {
    in1 <- 1:2
    out1 <- data.frame(lat=1, lon=2)
    out2 <- data.frame(lat=seq(1, 7, 2), lon=seq(2, 8, 2))

    expect_equal(asesgeo:::getCoordArgs(as.list(in1)), out1)
    expect_equal(asesgeo:::getCoordArgs(list(1, 2), list(3, 4), list(5, 6), list(7, 8)), 
                 out2)
})

test_that("getCoordArgs matrix", {
    out1 <- data.frame(lat=70, lon=120)
    in1 <- matrix(c(70, 120), nrow=1)
    in2 <- matrix(c(120, 70), nrow=1)
    
    expect_equal(asesgeo:::getCoordArgs(in1), out1)
    expect_warning(asesgeo:::getCoordArgs(in2), "do it for you")
    expect_equal(suppressWarnings(asesgeo:::getCoordArgs(in1)), out1)
})
