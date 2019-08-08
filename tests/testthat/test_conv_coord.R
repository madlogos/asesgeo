context("Test conv_coord")
if (! ".asesEnv" %in% ls(all.names=TRUE, envir=globalenv())) 
    .asesEnv <<- new.env(parent=globalenv())

test_that("convCoord with baidu api", {
    # ak <- "0QxKBNPPD2BrnnRkNtkoG3XI"
    if (is.null(.asesEnv$API_KEY$baidumap)) 
        skip("You are recommended to test this file separately")
    
    out1 <- data.frame(lat= 39.90245063, lng=116.42702745)
    
    expect_message(conv_coord(39.90105, 116.42079, from='WGS-84', to='GCJ-02', 
                              api='baidu', messaging=TRUE), 
                   "calling 1 APIs: http://api.map.baidu.com/geoconv/v1")
    expect_equal(conv_coord(39.90105, 116.42079, from='WGS-84', to='GCJ-02', 
                            api='baidu', idf=FALSE), out1)
    expect_warning(conv_coord(39.90105, 116.42079, from='GCJ-02', to='WGS-84', 
                              api='baidu', idf=FALSE),
                   "convert failed with error 22")
    expect_equal(suppressWarnings(
        convCoord(39.90105, 116.42079, from='GCJ-02', to='WGS-84', api='baidu', 
                  idf=FALSE)),
                 data.frame(lat=NA_real_, lng=NA_real_))
})

test_that("convCoord with gaode api", {
    if (is.null(.asesEnv$API_KEY$gaodemap)) 
        skip("You are recommended to test this file separately")
    
    out1 <- data.frame(lat= 39.902445203994, lng=116.427020941841)
    
    expect_message(conv_coord(39.90105, 116.42079, from='WGS-84', to='GCJ-02', 
                              api='gaode', messaging=TRUE), 
                   "calling 1 APIs: https://restapi.amap.com/v3/assistant")
    expect_equal(conv_coord(39.90105, 116.42079, from='WGS-84', to='GCJ-02', 
                            api='gaode', idf=FALSE), out1)
    expect_error(convCoord(39.90105, 116.42079, from='GCJ-02', to='WGS-84', 
                             api='gaode', idf=FALSE),
                   "'arg' should be one of “GCJ-02”")
    expect_equal(suppressWarnings(
        convCoord(39.90105, 116.42079, from='GCJ-02', to='WGS-84', api='gaode', 
                  idf=FALSE)),
        data.frame(lat=NA_real_, lng=NA_real_))
})


test_that("convCoord without api", {
    wgs <- data.frame(lat=39.90105589, lng=116.42079784)
    gcj <- data.frame(lat=39.90245219, lng=116.42702991)
    wgs_out <- data.frame(lat=39.9010558944, lng=116.4207978498)
    bd_out <- data.frame(lat=39.90850546, lng=116.43350894)
    
    expect_equal(convCoord(wgs$lat, wgs$lng, from="WGS", to="GCJ")[, c("lat", "lng")], gcj)
    expect_equal(convCoord(gcj$lat, gcj$lng, from="GCJ", to="WGS")[, c("lat", "lng")], wgs_out)
    expect_equal(convCoord(wgs$lat, wgs$lng, from="WGS", to="BD")[, c("lat", "lng")], bd_out)
})