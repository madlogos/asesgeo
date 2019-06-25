context("Test transform_coord")

test_that("wgs 2 gcj", {
    in1 <- c(39.90734, 116.39089)
    out1 <- data.frame(lat=39.90874107, lng=116.39713094)
    out1_ <- data.frame(lat=39.90874539, lng=116.39713623)  # api
    in2 <- matrix(rep(c(39.90734, 116.39089), 3), nrow=2)
    out2 <- data.frame(lat=rep(39.90874107, 3), lng=rep(116.39713094, 3))
    in3 <- data.frame(lat=c(rep(39.90734, 3), NA), lon=rep(116.39089, 4))
    out3 <- rbind(out2, c(NA, NA))
    
    in4 <- c(0, 0)  # outside China
    out4 <- data.frame(lat=0, lng=0)
    out5 <- data.frame(lat=0.0027284583 , lng=0.0154984971)
    
    expect_equal(wgs_to_gcj(as.list(in1)), out1)
    expect_equal(wgs_to_gcj(in1[1], in1[2]), out1)
    expect_equal(wgs_to_gcj(rep(in1[1], 2), rep(in1[2], 2)), rbind(out1, out1))
    expect_equal(wgs_to_gcj(in2), out2)
    expect_equal(wgs_to_gcj(in3), out3)
    
    expect_equal(suppressWarnings(wgs_to_gcj(in4)), out4)
    expect_equal(suppressWarnings(wgs_to_gcj(in4, force=TRUE)), out5)
})

test_that("gcj 2 wgs", {
    in1 <- c(39.908746, 116.397131)
    out1 <- data.frame(lat=39.90734493, lng=116.39089006)
    in2 <- matrix(rep(c(39.908746, 116.397131), 3), nrow=2)
    out2 <- data.frame(lat=rep(39.90734493, 3), lng=rep(116.39089006, 3))
    in3 <- data.frame(lat=c(rep(39.908746, 3), NA), lon=rep(116.397131, 4))
    out3 <- rbind(out2, c(NA, NA))
    
    in4 <- c(40.366518, 124.775643)  # North Korea, near China
    out4 <- data.frame(lat=40.366518, lng=124.775643)
    out5 <- data.frame(lat=40.366518, lng=124.775643)
    
    expect_equal(gcj_to_wgs(as.list(in1)), out1)
    expect_equal(gcj_to_wgs(in1[1], in1[2]), out1)
    expect_equal(gcj_to_wgs(rep(in1[1], 2), rep(in1[2], 2)), rbind(out1, out1))
    expect_equal(gcj_to_wgs(in2), out2)
    expect_equal(gcj_to_wgs(in3), out3)
    
    expect_equal(suppressWarnings(gcj_to_wgs(in4)), out4)
    expect_equal(suppressWarnings(gcj_to_wgs(in4, force=TRUE)), out5)
})

test_that("gcj 2 bd", {
    in1 <- c(39.908746, 116.397131)
    out1 <- data.frame(lat=39.91508839, lng=116.40350426)
    in2 <- matrix(rep(c(39.908746, 116.397131), 3), nrow=2)
    out2 <- data.frame(lat=rep(39.91508839, 3), lng=rep(116.40350426, 3))
    in3 <- data.frame(lat=c(rep(39.908746, 3), NA), lon=rep(116.397131, 4))
    out3 <- rbind(out2, c(NA, NA))
    
    expect_equal(gcj_to_bd(as.list(in1)), out1)
    expect_equal(gcj_to_bd(in1[1], in1[2]), out1)
    expect_equal(gcj_to_bd(rep(in1[1], 2), rep(in1[2], 2)), rbind(out1, out1))
    expect_equal(gcj_to_bd(in2), out2)
    expect_equal(gcj_to_bd(in3), out3)
})

test_that("bd 2 gcj", {
    in1 <- c(39.91509, 116.40350)
    out1 <- data.frame(lat=39.90874808, lng=116.39712692)
    in2 <- matrix(rep(c(39.91509, 116.40350), 3), nrow=2)
    out2 <- data.frame(lat=rep(39.90874808, 3), lng=rep(116.39712692, 3))
    in3 <- data.frame(lat=c(rep(39.91509, 3), NA), lon=rep(116.40350, 4))
    out3 <- rbind(out2, c(NA, NA))
    
    expect_equal(bd_to_gcj(as.list(in1)), out1)
    expect_equal(bd_to_gcj(in1[1], in1[2]), out1)
    expect_equal(bd_to_gcj(rep(in1[1], 2), rep(in1[2], 2)), rbind(out1, out1))
    expect_equal(bd_to_gcj(in2), out2)
    expect_equal(bd_to_gcj(in3), out3)
})

test_that("wgs 2 bd", {
    skip("not needed.")
})

test_that("bd 2 wgs", {
    skip("not needed.")
})
