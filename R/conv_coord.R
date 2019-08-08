#' Generic function to convert geographic coordinates
#'
#' The general function that converts lat/lon coordintes from one GCS to another
#' GCS including WGS-84, GCJ-02 and BD-09 either locally or by calling Baidu
#' Maps API. 
#' 
#' @param lat numeric vector, the latitudes
#' @param lon numeric vector, the longitudes
#' @param from character, the inputting GCS, "WGS-84", "GCJ-02", or "BD-09"
#' @param to character, the outputting GCS, "WGS-84", "GCJ-02", or "BD-09"
#' @param output character, the output format, "all" or "raw". Default "all".
#' \itemize{
#'  \item 'all": the structured data.frame of results comprising of 'lat' and 'lng'. \cr
#'  \item 'raw': the raw JSON data. You can then parse the results using 
#'    \code{\link{parse_convcoords}()}. It is only effective when setting a specific
#'    \code{api} (api mode conversion).
#' }
#' @param api character, call 'baidu' or 'gaode' maps api. Note that baidu API only supports 
#' the transformations from WGS-84 or GCJ-02 to BD-09, and gaode API only supports
#' the tranformations from WGS-84 or BD-09 to GCJ-02. Other coodinate conversions
#' must be done locally. As the conversion result is the same, it's recommended
#' to perform conversions locally. Default NULL, indicating that local algorithm
#' is applied.
#' @param force logical, whether convert the coordinates regardless if they
#' locate inside China. Default FALSE, indicating that only coordinates inside China
#' will be converted. It is only effective when \code{api} is NULL (local mode).
#' @param accurate_cn_bou logical, whether use accurate China boundary to convert
#' the coordinates. It is only effective when \code{api} is NULL (local mode). 
#' Default TRUE. The WGS-84 ==> GCJ-02 conversion will only be
#' conducted in coordinates inside China. When using the accurate China boundary,
#' the function will check if the coordinates are actually inside China's territory.
#' When set FALSE, the function will use a very vague rectangle to reprensent
#' China boundary.
#' @param key character. When \code{api} is TRUE, a web api will be called.
#' The key is thus mandatory. If key is not properly provided, the api
#' will report errors. Default NULL, which indicates that \code{\link{get_api_key}()}
#' will be called to set the api key.
#' @param time numeric, time interval when calling the APIs. This is used to avoid
#' the overuse. Default 0.
#' @param idf logical, whether add an identifier column to the result. Default 
#' TRUE.
#' @param messaging logical, whether print message during processing. Default FALSE.
#' @param ... other arguments to pass to the function. 
#' 
#' @return a 2-col data.frame ([lng, lat]) of transformed coordinates.
#' @author Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), PhD student from
#' Center for Earth System Science, Tsinghua University \cr
#' Yiying Wang (\email{wangy@@aetna.com})
#' 
#' @details The original function turns on \code{api}, which calls Google or Baidu APIs and
#' may thus be slow. This version by default turns off \code{api} and applies 
#' built-in conversion functions to do the trick and thus performs faster. \cr
#' Note that the APIs have usage limits (baidu: 100 lat/lon coordinates per query,
#' gaode: 40 lon/lat coordinates per query). \cr
#' Since the results of coordinate conversion using API method and local algorithms
#' are basically the same, it is recommended to use local algorithms. \cr
#' 
#' @seealso \itemize{
#' \item local transforming functions: \code{\link{wgs_to_gcj}()}, \code{\link{gcj_to_bd}()},
#'   \code{\link{gcj_to_wgs}()}, \code{\link{wgs_to_bd}()}, \code{\link{bd_to_wgs}()}, 
#'   \code{\link{bd_to_gcj}()} and \code{\link{get_api_key}()}. \cr
#' \item \code{asesgeo:::get_cn_bou}(): a list of the 'accurate China boundary' in
#'   'WGS-84', 'GCJ-02' and 'BD-09', which are all \code{sp::SpatialPolygons} objects. \cr
#' \item \code{\link{is_out_of_china}()}: see how points aer distinguished as in
#'   or out of China.
#' }
#' @references Baidu web API for coordinate conversion at
#' \url{http://lbsyun.baidu.com/index.php?title=webapi/guide/changeposition} \cr
#' Gaode web API for coordinate conversion at 
#' \url{https://lbs.amap.com/api/webservice/guide/api/convert}
#' @export
#' @importFrom glue glue
#' @aliases conv_coord
#' @rdname conv_coord
#' @examples
#' \dontrun{
#' # latitude/longitude coordinates of Beijing railway station
#' ## WGS-84: (39.90105, 116.42079)
#' ## GCJ-02: (39.90245, 116.42703)
#' ## BD-09:  (39.90851, 116.43351)
#' ak <- <Your baidu map api key>
#' conv_coord(39.90105, 116.42079, from='WGS-84', to='GCJ-02')
#' conv_coord(39.90105, 116.42079, from='WGS-84', to='GCJ-02', api='baidu', 
#'            key=ak)
#' conv_coord(39.90105, 116.42079, from='WGS-84', to='BD-09')
#' conv_coord(39.90105, 116.42079, from='WGS-84', to='BD-09', api='baidu', 
#'            key=ak)
#' conv_coord(39.90245, 116.42703, from='GCJ-02', to='WGS-84')
#' 
#' # not supported by baidu or gaode map api, return NAs
#' conv_coord(39.90245, 116.42703, from='GCJ-02', to='WGS-84', api='baidu', 
#'            key=ak)
#' conv_coord(39.90245, 116.42703, from='GCJ-02', to='BD-09')
#' conv_coord(39.90245, 116.42703, from='GCJ-02', to='BD-09', api='baidu', 
#'            key=ak)
#' conv_coord(39.90851, 116.43351, from='BD-09', to='GCJ-02')
#'
#' # not supported by baidu or gaode map api, return NAs
#' conv_coord(39.90851, 116.43351, from='BD-09', to='GCJ-02', api='baidu', 
#'            key=ak)
#' conv_coord(39.90851, 116.43351, from='BD-09', to='WGS-84')
#'
#' # not supported by baidu or gaode map api, return NAs
#' conv_coord(39.90851, 116.43351, from='BD-09', to='WGS-84', api='baidu', 
#'            key=ak)
#'
#' # convert multiple coordinates
#' lat <- c(39.99837, 39.98565)
#' lng <- c(116.3203, 116.2998)
#' conv_coord(lat, lng, from='WGS-84', to='GCJ-02')
#' }
convCoord <- function(lat, lon, from = c('WGS-84', 'GCJ-02', 'BD-09'),
                 to = c('WGS-84', 'GCJ-02', 'BD-09'), output = c('all', 'raw'),
                 api = NULL, force = FALSE, accurate_cn_bou = TRUE, key=NULL, 
                 time=0, idf=TRUE, messaging=FALSE, ...){
    # check parameters
    stopifnot(is.numeric(lat))
    stopifnot(is.numeric(lon))
    from <- match.arg(from)
    to <- match.arg(to)
    output <- match.arg(output)
    stopifnot(length(lat) == length(lon))
    if (!is.null(api)) api <- match.arg(api, c('baidu', 'gaode'))
    stopifnot(is.logical(accurate_cn_bou))
    
    # map the local conversion function using a dictionary
    fun_dict <- structure(
        list(wgs_to_gcj, wgs_to_bd, gcj_to_wgs, gcj_to_bd, bd_to_wgs, bd_to_gcj), 
        names=c("WGS-84 GCJ-02", "WGS-84 BD-09", "GCJ-02 WGS-84", 
                "GCJ-02 BD-09", "BD-09 WGS-84", "BD-09 GCJ-02"))
    fun <- fun_dict[[paste(from, to)]]
    
    if (from == to){
        return(data.frame(lat = lat, lng = lon))
    }else{
        if (! is.null(api)){  # api mode
            out <- conv_coord_api(
                lat=lat, lon=lon, from=from, to=to, output=output, api=api, 
                key=key, time=time, idf=idf, messaging=messaging, ...)
        }else{  # local mode
            out <- fun(lat, lon, accurate_cn_bou=accurate_cn_bou)
            if (idf) out <- data.frame(
                idf=glue('{sprintf("%.3f", lat)},{sprintf("%.3f", lon)}'), out)
        }
        return(out)
    }
}

#' @export
conv_coord <- convCoord

conv_coord_api <- function(lat, lon, from=c('WGS-84', 'GCJ-02', 'BD-09'),
                           to=c('WGS-84', 'GCJ-02', 'BD-09'), output=c('all', 'raw'),
                           api=c('baidu', 'gaode'), time=0, 
                           idf=TRUE, key=NULL, messaging=FALSE, ...){
    from <- match.arg(from)
    to <- match.arg(to)
    api <- match.arg(api)
    stopifnot(is.logical(idf))
    stopifnot(is.logical(messaging))
    
    fun_dict <- list(baidu=conv_coord_api_baidu,
                     gaode=conv_coord_api_gaode)
    fun <- fun_dict[[api]]
    fun(lat=lat, lon=lon, from=from, to=to, output=output, time=time, idf=idf, 
        key=key, messaging=messaging, ...)
}

#' @importFrom dplyr bind_rows
parse_convcoords <- function(cvlst, output=c('all'), ...){
    output <- match.arg(output)
    cvname <- names(cvlst)
    cvdf <- mapply(parse_convcoord_result, cv=cvlst, cvname=cvname,
                   MoreArgs=list(output=output, ...), SIMPLIFY=FALSE) %>% 
        bind_rows
    
    return(switch(
        output,
        all=cvdf))
}

parse_convcoord_result <- function(cv, ...){
    # work function for parse_convcoords
    stopifnot(inherits(cv, 'api_data'))
    UseMethod(".parse_convcoord_result", cv)
}

#' @export
#' @importFrom aseskit iif ifnull ifna
.parse_convcoord_result.baidu_convcoord <- function(
    cv, output='all', cvname=NULL, idf=TRUE, ...){
    
    cvname <- aseskit::ifnull(cvname, 'unknown')
    idf_col <- cvname %>% as.factor %>% levels %>% strsplit(';') %>% unlist
    
    if (as.numeric(cv$status) == 0 || length(cv) == 0){  # success
        # cvdf <- with(cv, {
        #     data.frame(lat = as.numeric(base64_dec(cv$result[, 'y'])),
        #                lng = as.numeric(base64_dec(cv$result[, 'x'])),
        #                row.names = NULL)})
        cvdf <- cv$result[, c("y", "x")]
        colnames(cvdf) <- c("lat", "lng")
    }else{
        warning(paste0('convert failed with error ', cv$status, ' (', 
                       cv$message, ')'),
                call. = FALSE)
        len <- strsplit(cvname, ';') %>% unlist %>% length
        cvdf <- data.frame(lat = rep(NA_real_, len), 
                           lng = rep(NA_real_, len))
    }
    invalid_latlng <- aseskit::ifna(abs(cvdf$lat) > 90 & abs(cvdf$lng) > 180, FALSE)
    if (any(invalid_latlng)) cvdf[invalid_latlng, ] <- NA
    if (idf) cvdf <- data.frame(idf=idf_col, cvdf, stringsAsFactors=FALSE)
    
    return(cvdf)
}

#' @export
#' @importFrom aseskit iif ifnull
.parse_convcoord_result.gaode_convcoord <- function(
    cv, output='all', cvname=NULL, idf=TRUE, ...){
    
    cvname <- aseskit::ifnull(cvname, 'unknown')
    idf_col <- cvname %>% as.factor %>% levels %>% strsplit(';') %>% unlist
    
    if (as.numeric(cv$status) == 1 || length(cv) == 0){  # success
        # get character matrix
        cvdf <- cv$locations %>% strsplit(';') %>% unlist %>% strsplit(',') %>% 
            do.call('rbind', .) %>% as.data.frame(stringsAsFactors=FALSE)
        # convert
        cvdf <- data.frame(lat=as.numeric(cvdf[, 2]),
                           lng=as.numeric(cvdf[, 1]))
        if (idf) cvdf <- data.frame(idf=idf_col, cvdf, stringsAsFactors=FALSE)
    }else{
        warning(paste0('convert failed with error ', cv$status, ' (', 
                       cv$infocode, ': ', cv$info, ')'),
                call. = FALSE)
        len <- strsplit(cvname, ';') %>% unlist %>% length
        cvdf <- data.frame(lat = rep(NA_real_, len), 
                           lng = rep(NA_real_, len))
        if (idf) cvdf <- data.frame(idf=idf_col, cvdf, stringsAsFactors=FALSE)
    }
    return(cvdf)
}

#' @export
.parse_convcoord_reuslt.default <- function(
    cv, output=c('all'), cvname=NULL, idf=TRUE,
    ...){
    if (! inherits(cv, 'api_data'))
        warning('cv must be of "api_data" class.')
    stop('cv must be of subclasses of "api_data", ',
         'typically yielded using aseskit::get_api_data().')
}

# -----------Work functions------------

#' @importFrom aseskit get_api_data synthesize_api
conv_coord_api_baidu <- function(lat, lon, from=c('WGS-84', 'GCJ-02', 'BD-09'),
                                 to=c('WGS-84', 'GCJ-02', 'BD-09'), 
                                 output=c('all', 'raw'), time=0, idf=TRUE, 
                                 key=NULL, messaging=FALSE, ...){
    # check args
    output <- match.arg(output)
    from <- match.arg(from)
    to <- match.arg(to)
    
    # coordinate system lookup table, 100 coordinates/batch at most
    latlng <- paste(lon, lat, sep=',')  # sequence: lon,lat
    split_by <- as.vector(mapply(
        rep, 1:ceiling(length(latlng)/100), 100))[seq_along(latlng)]
    latlngs <- vapply(split(latlng, split_by), function(coord){
        paste(coord, collapse=';')
    }, FUN.VALUE=character(1L))
    
    # synthesize urls
    urls <- aseskit::synthesize_api(
        url_body=latlngs, provider='baidumap', api='convcoord', 
        coord_from=from, coord_to=to, key=key)
    
    # result list
    cv <- aseskit::get_api_data(
        urls, use_curl=FALSE, messaging=messaging, name_out=latlngs)
    
    # parse
    if (output == 'raw') return(cv)
    return(parse_convcoords(cv, output=output, idf=idf))
}

#' @importFrom aseskit get_api_data synthesize_api
conv_coord_api_gaode <- function(lat, lon, from=c('WGS-84', 'GCJ-02', 'BD-09'),
                                 to=c('GCJ-02'), 
                                 output=c('all', 'raw'), time=0,
                                 idf=TRUE, key=NULL, messaging=FALSE, ...){
    # check args
    from <- match.arg(from)
    to <- match.arg(to)
    output <- match.arg(output)
    
    # coordinate system lookup table, 40 coordinates/batch at most
    latlng <- paste(sprintf("%.6f", lon), sprintf("%.6f", lat), 
                    sep=",")  # sequence: lon,lat
    split_by <- as.vector(mapply(
        rep, 1:ceiling(length(latlng)/40), 40))[seq_along(latlng)]
    latlngs <- vapply(split(latlng, split_by), function(coord){
        paste(coord, collapse=';')
    }, FUN.VALUE=character(1L))
    
    # synthesize urls
    urls <- aseskit::synthesize_api(
        url_body=latlngs, provider='gaodemap', api='convcoord', 
        coord_from=from, coord_to=to, key=key)
    
    # result list
    cv <- aseskit::get_api_data(
        urls, use_curl=FALSE, messaging=messaging, name_out=latlngs)
    
    # parse
    if (output == 'raw') return(cv)
    return(parse_convcoords(cv, idf=idf))
}

