context("Test utils")

test_that("out of china", {
    expect_equal(isOutOfChina(c(190, 90), c(21, 27)), c(NA, TRUE))
    expect_true(isOutOfChina(-50, -90))
    expect_false(isOutOfChina(30, 105))
    expect_equal(isOutOfChina(NA, 9), NA)
})

test_that("transform lat and lon", {
    expect_equal(transformLat(c(0, 30), c(0, 105)), c(-100, 2658.186835108))
    expect_equal(transformLon(c(0, 30), c(0, 105)), c(300, 1045.54772256))
})

test_that("format coord args", {
    in1 <- matrix(c(0, 30, 0, 105), ncol=2)
    out1 <- data.frame(lat=c(0, 30), lon=c(0, 105))
    in2 <- matrix(c(0, 105, 0, 30), ncol=2)
    in3 <- matrix(c(0, 30, NA, 105), ncol=2)
    out3 <- data.frame(lat=c(NA, 30), lon=c(NA, 105))
    
    expect_equal(formatCoordArgs(in1), out1)
    expect_error(formatCoordArgs(in2), "within \\[-180, 180\\]")
    expect_equal(formatCoordArgs(in3), out3)
})

test_that("getCoordArgs vector", {
    out1 <- data.frame(lat=1, lon=2)
    in1 <- 1:2
    in2 <- 1:3
    out21 <- data.frame(lat=1:3, lon=1:3)
    out22 <- data.frame(lat=c(1:2, NA), lon=c(1:2, NA))
    
    expect_equal(getCoordArgs(1, 2), out1)
    expect_equal(getCoordArgs(1, 2, 3), out1)
    expect_equal(getCoordArgs(in2, in2), out21)
    expect_equal(getCoordArgs(in2, in1), out22)
    expect_error(getCoordArgs(in2), "how to pair")
})

test_that("getCoordArgs list", {
    in1 <- 1:2
    out1 <- data.frame(lat=1, lon=2)
    out2 <- data.frame(lat=seq(1, 7, 2), lon=seq(2, 8, 2))

    expect_error(getCoordArgs(list(c(100, 120), c(-80, -180))), "Cannot distinguish")
    expect_equal(getCoordArgs(list(in1)), out1)
    expect_equal(getCoordArgs(list(1:2, 3:4, 5:6, 7:8)), out2)
})


