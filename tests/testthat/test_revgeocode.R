context("Test revgeocode")
if (! ".asesEnv" %in% ls(all.names=TRUE, envir=globalenv())) 
    .asesEnv <<- new.env(parent=globalenv())

test_that("revgeocode with google map api", {
    if (is.null(.asesEnv$API_KEY$google)) 
        skip("You are recommended to test this file separately")
    in1 <- data.frame(lat=39.9035897, lng=116.4209927)
    
    out1 <- revgeocode(in1, api="google", output="all", location_type="ROOFTOP")
    
    expect_equal(nrow(out1), 2)
    expect_equal(out1[['street_no']][1], 18)
})


test_that("revgeocode with baidu map api", {
    if (is.null(.asesEnv$API_KEY$baidu))
        skip("You are recommended to test this file separately")
    in1 <- data.frame(lat=39.9023994, lng=116.4209460)
    
    out1 <- revgeocode(in1, api="baidu", output="all", use_curl=FALSE)
    
    expect_equal(nrow(out1), 1)
    expect_equal(out1[['city']][1], '\u5317\u4eac\u5e02')
    expect_equal(out1[['district']][1], '\u4e1c\u57ce\u533a')
})

test_that("revgeocode with gaode map api", {
    if (is.null(.asesEnv$API_KEY$gaode)) 
        skip("You are recommended to test this file separately")
    
    in1 <- data.frame(lat=c(31.2371774, NA), lng=c(121.4585095, NA))
    
    out1 <- suppressWarnings(revgeocode(in1, api='gaode', output='all', use_curl=FALSE))
        
    expect_equal(nrow(out1), 2)
    expect_equal(out1[['province']][1], '\u4e0a\u6d77\u5e02')
    expect_equal(out1[['citycode']][1], '021')
})