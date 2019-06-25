context("Test geocode")

test_that("geocode with google map api", {
    if (is.null(asesgeo_env$API_KEY$google)) 
        skip("You are recommended to test this file separately")
    
    in1 <- "Beijing railway station"
    out1 <- data.frame(idf=gsub('\\s+', '+', in1), lat=39.9035897, lng=116.4209927)
    
    expect_equal(as.data.frame(geocode(in1, api="google")), out1)
    expect_equal(geocode(in1, output="all", api="google")[['loctype']][1],
                 "GEOMETRIC_CENTER")
})


test_that("geocode with baidu map api", {
    if (is.null(asesgeo_env$API_KEY$baidu)) 
        skip("You are recommended to test this file separately")
    
    in1 <- "\u5317\u4eac\u706b\u8f66\u7ad9"
    out1 <- data.frame(idf=in1, lat=39.9023994, lng=116.4209460)
    
    expect_equal(as.data.frame(geocode(in1, api="baidu", use_curl=FALSE)), out1)
    expect_equal(geocode(in1, output='all', api="baidu", use_curl=FALSE)[['loctype']], 
                 "\u706b\u8f66\u7ad9")
})

test_that("geocode with gaode map api", {
    if (is.null(asesgeo_env$API_KEY$gaode)) 
        skip("You are recommended to test this file separately")
    
    in1 <- c("\u4e0a\u6d77\u81ea\u7136\u535a\u7269\u9986",
             "\u8499\u81ea\u8def\u4e2d\u5c71\u5357\u4e8c\u8def\u8def\u53e3")
    out1 <- data.frame(idf=in1, lat=c(31.2371774, NA_real_), lng=c(121.4585095, NA_real_))
        
    expect_equal(as.data.frame(geocode(in1, api="gaode", use_curl=FALSE)), out1)
    expect_equal(geocode(in1, output="all", api="gaode", use_curl=FALSE)[["citycode"]],
                 c("021", NA))
})