context("Test APIs")


test_that("API class", {
    in1 <- synthesize_baidumap_api(url_body="<Req>", key="<Key>")
    in2 <- synthesize_googlemap_api(url_body="<Req>", key="<Key>")
    in3 <- synthesize_gaodemap_api(url_body="<Req>", key="<Key>")
    expect_equal(class(in1[[1]]), c("baidu_api_geocode", "url"))
    expect_equal(class(in2[[1]]), c("google_api_geocode", "url"))
    expect_equal(class(in3[[1]]), c("gaode_api_geocode", "url"))
})

test_that("Google map API", {
    in1 <- synthesize_google_api_geocode("<Req>", region="uk", key="<Key>")
    in2 <- synthesize_google_api_revgeocode(c("1.1,2.2", "3.3,4.4"), key="<Key>")
    out1 <- "https://maps.googleapis.com/maps/api/geocode/json?address=<Req>&region=uk&key=<Key>"
    out21 <- "https://maps.googleapis.com/maps/api/geocode/json?latlng=1.1,2.2&key=<Key>"
    out22 <- "https://maps.googleapis.com/maps/api/geocode/json?latlng=3.3,4.4&key=<Key>"
    
    expect_equal(unlist(in1)[[1]], out1)
    expect_equal(unlist(in2)[[1]], out21)
    expect_equal(unlist(in2)[[2]], out22)
})

test_that("Baidu map API", {
    in1 <- synthesize_baidu_api_geocode("<Req>", ics="GCJ-02", key="<Key>")
    in2 <- synthesize_baidu_api_revgeocode("1.1,2.2", key="<Key>")
    in3 <- synthesize_baidu_api_convcoord("1.1,2.2;3.3,4.4", coord_to="WGS-84", key="<Key>")
    out1 <- "http://api.map.baidu.com/geocoder/v2/?address=<Req>&output=json&ak=<Key>"
    out2 <- "http://api.map.baidu.com/geocoder/v2/?location=1.1,2.2&output=json&ak=<Key>"
    out3 <- "http://api.map.baidu.com/geoconv/v1/?coords=1.1,2.2;3.3,4.4&from=5&to=1&ak=<Key>"
    
    expect_equal(unlist(in1)[[1]], out1)
    expect_equal(unlist(in2)[[1]], out2)
    expect_equal(unlist(in3)[[1]], out3)
})

test_that("Gaode map API", {
    in1 <- synthesize_gaode_api_geocode("<Req>", ics="GCJ-02", key="<Key>")
    in2 <- synthesize_gaode_api_revgeocode("1.1,2.2", key="<Key>")
    in3 <- synthesize_gaode_api_convcoord("1.1,2.2;3.3,4.4", coord_from="BD-09", key="<Key>")
    out1 <- "https://restapi.amap.com/v3/geocode/geo?address=<Req>&output=JSON&key=<Key>"
    out2 <- "https://restapi.amap.com/v3/geocode/regeo?location=1.1,2.2&output=JSON&extensions=all&key=<Key>"
    out3 <- paste0("https://restapi.amap.com/v3/assistant/coordinate/convert?", 
                   "locations=1.1,2.2;3.3,4.4&coordsys=baidu&output=json&key=<Key>")
    
    expect_equal(unlist(in1)[[1]], out1)
    expect_equal(unlist(in2)[[1]], out2)
    expect_equal(unlist(in3)[[1]], out3)
})

test_that("ipify API", {
    in1 <- synthesize_ipify_api_geohost("<Req>", key="<Key>")
    in2 <- synthesize_ipify_api_getip("<Req>", key="<Key>")
    out1 <- "https://geo.ipify.org/api/v1?apiKey=<Key>&ipAddress=<Req>"
    out2 <- "https://api.ipify.org?format=json"
    
    expect_equal(unlist(in1)[[1]], out1)
    expect_equal(unlist(in2)[[1]], out2)
})

test_that("ipinfo API", {
    in1 <- synthesize_ipinfo_api_geohost("<Req>", key="<Key>")
    out1 <- "https://ipinfo.io/<Req>/json?token=<Key>"
    
    expect_equal(unlist(in1)[[1]], out1)
})

test_that("ipstack API", {
    in1 <- synthesize_ipstack_api_geohost("<Req>", key="<Key>")
    out1 <- "http://api.ipstack.com/<Req>?access_key=<Key>"
    
    expect_equal(unlist(in1)[[1]], out1)
})

