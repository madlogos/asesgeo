#' Transform coordinates from/to WGS-84, GCJ-02 and BD-09 locally
#' 
#' There are three commonly seen coordinate systems: \itemize{
#' \item WGS-84 is the gloabal coordinate system, typcially collected by GPS. \cr
#' \item GCJ-02 is the coordinate system mandated by Chinese Gov't. \cr
#' \item BD-09 is the coordinates system developed by Baidu Inc. that encrypts 
#'  GCJ-02 further more.}
#' The functions in this family encrypts coordinates from 'WGS-84' to 'GCJ-02' 
#' and 'BD-09', and decrypts them back vice versa. The built-in algorithm called 
#' by these functions is open source and comes with absolutely no ganrantee. 
#' 
#' @note Latitude is the horizontal line serving as y-axis metric, longitude is
#' the vertical line serving as x-axis metric.
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param y one of \itemize{
#' \item coordinate pair: a list of coordinate vectors \code{list(lat1, lon1)},
#' in which case you can provide other coordiante vectors in \code{...} \cr
#' \item latitude vector: latitude number(s), in which case you need to provice longitute
#'  number(s) in \code{...} \cr
#' \item coordinate matrix: a matrix (row 1-2 or col 1-2). The function will 
#'  choose how to read the data \cr
#' \item coordinate data.frame: a data.frame (col 1-2)
#' }
#' @param ... one of \itemize{
#' \item coordinate pairs: when \code{y} is a list containing a coordinate pair 
#'  (e.g., \code{list(lat1, lon1)}), you can pass other coordinate pairs here 
#'  (e.g., \code{list(lat2, lon2), list(lat3, lon3), ...}). \cr
#' \item longitude vector: when \code{y} is only latitude vector, you can pass 
#'  \code{x} (longitude) here. \cr
#' \item when \code{y} is a coordinate matrix or data.frame, \code{...} is omitted.
#' }
#' @param force logical, whether convert the coordinates regardless if they
#' locate inside China. Default FALSE, indicating that only coordinates inside China
#' will be converted.
#' @param accurate_cn_bou logical, whether to convert the coordinates based on
#' accurate China boundary. Only effective when \code{api} is NULL (local mode). 
#' Default TRUE. The WGS-84 ==> GCJ-02 conversion will only be
#' conducted in coordinates inside China. When using the accurate China boundary,
#' the function will check if the coordinates are actually inside China's territory.
#' When set FALSE, the function will use a very vague rectangle to reprensent
#' China boundary.
#' 
#' @seealso \itemize{
#'  \item \code{\link{conv_coord}()} is a wrapper of these functions (local mode
#'   transforming functions) and \code{conv_coord_api} (api mode transforming 
#'   function). \cr
#'  \item \code{asesgeo:::get_cn_bou()}: a list of the 'accurate China boundary' 
#'   in 'WGS-84', 'GCJ-02' and 'BD-09', which are all \code{sp::SpatialPolygons} objects. \cr
#'  \item \code{\link{is_out_of_china}()}: how points are distinguished as in
#'   or out of China.
#' }
#' @references
#' \url{https://on4wp7.codeplex.com/SourceControl/changeset/view/21483#353936}
#' @return A 2-col data.frame ([lng, lat]) of transformed coordinates. Note that
#' points out of China will not have the coordinates converted.
#' @export
#' @aliases wgs2gcj
#' @rdname transform_coord
#' 
#' @examples
#' \dontrun{
#' 
#' ## ========== wgs_to_gcj ==========
#' 
#' # Tiananmen square's WGS-84 coordinate is c(39.90734, 116.39089)
#' # http://www.google.cn/maps/place/Tiananmen,+Dongcheng,+Beijing/
#' # @39.90874,116.39713,16z?hl=en
#'
#' ## Single point
#' wgs_to_gcj(list(39.90734, 116.39089))  # or
#' wgs_to_gcj(39.90734, 116.39089)  # get
#' #           lat      lng
#' # [1,] 39.90874 116.3971
#'
#' ## Multiple points
#' ### Coordinate pairs or Lat / Lon vectors
#' wgs_to_gcj(list(39.90734, 116.39089), list(39.90734, 116.39089))  # or
#' wgs_to_gcj(c(39.90734, 39.90734), c(116.39089, 116.39089))  # get
#' #           lat      lng
#' # [1,] 39.90874 116.3971
#' # [2,] 39.90874 116.3971
#'
#' ### Matrix
#' m <- matrix(c(39.90734, 116.39089, 39.90734, 116.39089, 39.90734, 
#'               116.39089), nrow=2)
#' m
#' #           [,1]      [,2]      [,3]
#' # [1,]  39.90734  39.90734  39.90734
#' # [2,] 116.39089 116.39089 116.39089
#' 
#' wgs_to_gcj(m)  # get
#' #           lat      lng
#' # [1,] 39.90874 116.3971
#' # [2,] 39.90874 116.3971
#' # [3,] 39.90874 116.3971
#'
#' ### data.frame
#' df <- data.frame(lat=c(39.90734, 39.90734, 39.90734, NA),
#'                  lon=c(116.39089, 116.39089, 116.39089, 116.39089))
#' wgs_to_gcj(df)  # get
#' #           lat      lng
#' # [1,] 39.90874 116.3971
#' # [2,] 39.90874 116.3971
#' # [3,] 39.90874 116.3971
#' # [4,]       NA       NA
#' }
#'
wgs_to_gcj <- function(y, ..., force=FALSE, accurate_cn_bou=TRUE){
    # Coord Global GeoSystem (WGS-84) -> Mars GeoSystem (GCJ-02)
    # coords must be a vector c(lat,lon)
    
    # the vectorization efficiency is much better than the original implementation
    # which is actually recursive calling
    # > system.time(wgs2gcj(seq(30, 40, 0.001), seq(110, 120, 0.001)))
    #  user  system elapse 
    #  5.48  0.08   5.72 
    # > system.time(wgs_to_gcj(seq(30, 40, 0.001), seq(110, 120, 0.001)))
    #  user  system elapse 
    #  0.02  0.00   0.01 
    
    stopifnot(is.logical(force))
    stopifnot(is.logical(accurate_cn_bou))
    
    input <- getCoordArgs(y, ...)
    
    # Deprecated:
    # output <- as.data.frame(t(apply(input, MARGIN=1, .wgs2gcj)))
    if (nrow(input) == 0) return(NULL)
    
    A <- pkgenv$A
    EE <- pkgenv$EE
    # construct place holder
    output <- matrix(rep(NA_real_, nrow(input) * 2), ncol=2)
    
    if (force){  # regard all coords inside China
        insideChina <- seq_along(input$lat)
        outsideChina <- numeric(0L)
    }else{
        outOfChina <- isOutOfChina(input, gcs='WGS-84', accurate=accurate_cn_bou)
        insideChina <- which(! outOfChina)
        outsideChina <- which(outOfChina)
        rm(outOfChina)
    }
    
    # out of china
    if (length(outsideChina) > 0) {
        output[outsideChina, ] <- as.matrix(input[outsideChina, ])
        invisible(warning(
            'coordinates #', paste(outsideChina, collapse=', '), ' are out of China. ',
            'They will not be converted to GCJ-02.'))
    }
    # inside china
    if (length(insideChina) > 0){
        wgLat <- input$lat[insideChina]
        wgLon <- input$lon[insideChina]
        dLat <- transformLat(wgLon - 105.0, wgLat - 35.0)
        dLon <- transformLon(wgLon - 105.0, wgLat - 35.0)
        radLat <- wgLat / 180.0 * pi
        magic <- 1.0 - EE * sin(radLat) ^ 2
        sqrtMagic <- sqrt(magic)
        dLat <- (dLat * 180.0) / ((A * (1.0 - EE)) / (magic * sqrtMagic) * pi)
        dLon <- (dLon * 180.0) / (A / sqrtMagic * cos(radLat) * pi)
        output[insideChina, 1] <- wgLat + dLat
        output[insideChina, 2] <- wgLon + dLon
    }
    output <- as.data.frame(output)
    names(output) <- c("lat", "lng")
    return(output)
}

#' @export
wgs2gcj <- wgs_to_gcj


#' @export
#' @importFrom aseskit iif ifna
#' @aliases gcj2wgs
#' @rdname transform_coord
#' @examples
#' \dontrun{
#' 
#' ## ========== gcj_to_wgs ==========
#' 
#' # Tiananmen square's GCJ-02 coordinate is c(39.908746, 116.397131)
#' # http://www.openstreetmap.org/#map=19/39.90734/116.39089
#'
#' ## Single point
#' gcj_to_wgs(list(39.908746, 116.397131))  # or
#' gcj_to_wgs(39.908746, 116.397131)  # get
#' #           lat       lng
#' # [1,] 39.90734  116.3909
#'
#' ## Multiple points
#' ### Coordinate pairs or lat / lon vectors
#' gcj_to_wgs(list(39.908746, 116.397131), list(39.908746, 116.397131))  # or
#' gcj_to_wgs(c(39.908746, 39.908746), c(116.397131, 116.397131))  # get
#' #           lat       lng
#' # [1,] 39.90734  116.3909
#' # [2,] 39.90734  116.3909
#'
#' ### Matrix
#' m <- matrix(c(39.908746, 116.397131, 39.908746, 116.397131, 39.908746, 
#'               116.397131), nrow=2)
#' m
#' #           [,1]       [,2]       [,3]
#' # [1,]  39.90875   39.90875   39.90875
#' # [2,] 116.39713  116.39713  116.39713
#' gcj_to_wgs(m)  # get
#' #           lat       lng
#' # [1,] 39.90734  116.3909
#' # [2,] 39.90734  116.3909
#' # [3,] 39.90734  116.3909
#'
#' ### data.frame
#' df <- data.frame(lat=c(39.908746, 39.908746, 39.908746, NA),
#'                  lon=c(116.397131, 116.397131, 116.397131, 116.397131))
#' gcj_to_wgs(df)  # get
#' #           lat       lng
#' # [1,] 39.90734  116.3909
#' # [2,] 39.90734  116.3909
#' # [3,] 39.90734  116.3909
#' # [4,]       NA        NA
#' }
#'
gcj_to_wgs <- function(y, ..., force=FALSE, accurate_cn_bou=TRUE){
    # Coord  Mars GeoSystem (GCJ-02) -> Global GeoSystem (WGS-84)
    
    # cited from cran/geoChina:
    # offset dV = V' - V
    # question: gcj => wgs, namely V = V' - dV'
    # V' is known, while dV' is unknown.
    # since dV is very close to dV', using dV to estimate dV'; however, to calculate
    # dV, V must be known. since V and V' are very close to each other, initially 
    # using V' to approximate V.
    
    stopifnot(is.logical(force))
    stopifnot(is.logical(accurate_cn_bou))
    
    input <- getCoordArgs(y, ...)
    
    # convert all to WGS-84
    # -----------deprecated----------
    # gcLat <- input$lat
    # gcLon <- input$lon
    # dfCoord <- wgs_to_gcj(gcLat, gcLon)  # convert back
    # dLat <- dfCoord[, 'lat']
    # dLon <- dfCoord[, 'lng']
    # output <- cbind(2.0 * gcLat - dLat, 2.0 * gcLon - dLon)  # matrix
    # -----------------------------
    
    in0 <- as.matrix(input[c('lat', 'lon')])
    in1 <- in0
    # convert forcefully anyway
    out1 <- suppressWarnings(
        as.matrix(wgs_to_gcj(in1[, c('lat', 'lon')], force=TRUE)))
    output <- in1 - (out1 - in0)
    
    max_row_dif <- ifna(apply(abs(output - in1), 1, max), 0)
    need_adj <- max_row_dif >= 1e-6
    if (any(need_adj)){
        in1[need_adj, ] <- output[need_adj, ]
        out1[need_adj, ] <- as.matrix(wgs_to_gcj(in1[need_adj, 1], in1[need_adj, 2]))
        output[need_adj, ] <- in1[need_adj, ] - (out1[need_adj, ] - in0[need_adj, ])
    }
    
    # any points out of China? the points outside China after conversion will be
    # converted back
    if (force){  # regard all as inside China
        outsideChina <- numeric(0L)
    }else{
        outsideChina <- which(
            isOutOfChina(output[, 1:2], gcs="GCJ-02", accurate=accurate_cn_bou))
    }
    
    # out of china, no conversion (convert back to original)
    if (length(outsideChina) > 0) {
        output[outsideChina, ] <- as.matrix(input[outsideChina, ])
        invisible(warning(
            'coordinates #', paste(outsideChina, collapse=', '), ' are out of China ',
            'after conversion. They will not be converted to WGS-84.'))
    }
    output <- as.data.frame(output, stringsAsFactors=FALSE)
    colnames(output) <- c("lat", "lng")
    return(output)
}

#' @export
gcj2wgs <- gcj_to_wgs


#' @export
#' @aliases gcj2bd
#' @rdname transform_coord
#' @examples
#' \dontrun{
#' 
#' ## ========== gcj_to_bd ==========
#' 
#' # Tiananmen square's GCJ-02 coordinate is c(39.908746, 116.397131)
#' #http://api.map.baidu.com/marker?location=39.91509,116.40350&title=Tiananmen&
#'  content=Tiananmen%20square&output=html
#'
#' ## Single point
#' gcj_to_bd(list(39.908746, 116.397131))  # or
#' gcj_to_bd(39.908746, 116.397131)  # get
#' #           lat       lng
#' # [1,] 39.91509  116.4035
#'
#' ## Multiple points
#' ### Coordiante pairs or lat / lon vectors
#' gcj_to_bd(list(39.908746, 116.397131), list(39.908746, 116.397131))  # or
#' gcj_to_bd(c(39.908746, 39.908746), c(116.397131, 116.397131))  # get
#' #           lat       lng
#' # [1,] 39.91509  116.4035
#' # [2,] 39.91509  116.4035
#'
#' ### Matrix
#' m <- matrix(c(39.908746, 116.397131, 39.908746, 116.397131, 39.908746, 
#'               116.397131), nrow=2)
#' m
#' #           [,1]       [,2]       [,3]
#' # [1,]  39.90875   39.90875   39.90875
#' # [2,] 116.39713  116.39713  116.39713
#' gcj_to_bd(m)  # get
#' #           lat       lng
#' # [1,] 39.91509  116.4035
#' # [2,] 39.91509  116.4035
#' # [3,] 39.91509  116.4035
#'
#' ### data.frame
#' df <- data.frame(lat=c(39.908746, 39.908746, 39.908746, NA),
#'                  lon=c(116.397131, 116.397131, 116.397131, 116.397131))
#' gcj_to_bd(df)  # get
#' #           lat       lng
#' # [1,] 39.91509  116.4035
#' # [2,] 39.91509  116.4035
#' # [3,] 39.91509  116.4035
#' # [4,]       NA        NA
#' }
#'
gcj_to_bd <- function(y, ..., force=FALSE, accurate_cn_bou=TRUE){
    # Coord Mars GeoSystem (GCJ-02) -> Baidu GeoSystem (BD-09)
    stopifnot(is.logical(force))
    stopifnot(is.logical(accurate_cn_bou))
    
    input <- getCoordArgs(y, ...)
    
    # Deprecated:
    # output <- as.data.frame(t(apply(input, MARGIN=1, .gcj2bd)))
    XM_PI <- pkgenv$XM_PI
    output <- matrix(rep(NA_real_, nrow(input) * 2), ncol=2)
    
    na_rows <- is.na(input$lat) | is.na(input$lon)
    y <- input$lat[! na_rows]  # ie gcLat
    x <- input$lon[! na_rows]  # ie gcLon
    z <- sqrt(x ^ 2 + y ^ 2) + 0.00002 * sin(y * XM_PI)
    theta <- atan2(y, x) + 0.000003 * cos(x * XM_PI)
    output[! na_rows, ] <- cbind(z * sin(theta) + 0.006, z * cos(theta) + 0.0065)
    
    # check if outside China
    if (! force){
        wgs <- gcj_to_wgs(input, force=TRUE)
        outsideChina <- which(
            isOutOfChina(wgs, gcs="GCJ-02", accurate=accurate_cn_bou))
        if (length(outsideChina) > 0){
            invisible(warning(
                'coordinates #', paste(outsideChina, collapse=', '), ' are out of China ',
                'after conversion to WGS-84. They will not be converted to BD-09.'))
        }
        output[outsideChina, ] <- as.matrix(input[outsideChina, ])
    }
    
    output <- as.data.frame(output, stringsAsFactors=FALSE)
    colnames(output) <- c("lat", "lng")
    return(output)
}

#' @export
gcj2bd <- gcj_to_bd

#' @export
#' @aliases bd2gcj
#' @rdname transform_coord
#' @examples
#' \dontrun{
#' 
#' ## ========== bd_to_gcj ==========
#' 
#' # Tiananmen square's BD-06 coordinate is c(39.91509, 116.40350)
#' # http://www.google.cn/maps/place/Tiananmen,+Dongcheng,+Beijing/
#' # @39.90875,116.39713,16z?hl=en
#'
#' ## Single point
#' bd_to_gcj(list(39.91509, 116.40350))  # or
#' bd_to_gcj(39.91509, 116.40350)  # get
#' #           lat       lng
#' # [1,] 39.90875  116.3971
#'
#' ## Multiple points
#' ### coordinate pairs or lat / lon vectors
#' bd_to_gcj(list(39.91509, 116.40350), list(39.91509, 116.40350))  # or
#' bd_to_gcj(c(39.91509, 39.91509), c(116.40350, 116.40350))  # get
#' #           lat       lng
#' # [1,] 39.90875  116.3971
#' # [2,] 39.90875  116.3971
#'
#' ### Matrix
#' m <- matrix(c(39.91509, 116.40350, 39.91509, 116.40350, 39.91509, 
#'               116.40350), nrow=2)
#' m
#' #           [,1]       [,2]       [,3]
#' # [1,]  39.91509   39.91509   39.91509
#' # [2,] 116.40350  116.40350  116.40350
#' bd_to_gcj(m)  # get
#' #           lat       lng
#' # [1,] 39.90875  116.3971
#' # [2,] 39.90875  116.3971
#' # [3,] 39.90875  116.3971
#'
#' ### data.frame
#' df <- data.frame(lat=c(39.91509, 39.91509, 39.91509, NA),
#'                  lon=c(116.40350, 116.40350, 116.40350, 116.40350))
#' bd_to_gcj(df)  # get
#' #           lat       lng
#' # [1,] 39.90875  116.3971
#' # [2,] 39.90875  116.3971
#' # [3,] 39.90875  116.3971
#' # [4,]       NA        NA
#' }
#'
bd_to_gcj <- function(y, ..., force=FALSE, accurate_cn_bou=TRUE){
    # Coord Baidu GeoSystem (BD-09) -> Mars GeoSystem (GCJ-02)
    stopifnot(is.logical(force))
    stopifnot(is.logical(accurate_cn_bou))
    
    input <- getCoordArgs(y, ...)
    
    # Deprecated:
    # output <- as.data.frame(t(apply(input, MARGIN=1, .bd2gcj)))
    output <- matrix(rep(NA_real_, nrow(input) * 2), ncol=2)
    XM_PI <- pkgenv$XM_PI
    na_rows <- is.na(input$lat) | is.na(input$lon)
    
    bdLon <- input$lon[! na_rows]
    bdLat <- input$lat[! na_rows]
    x <- bdLon - 0.0065
    y <- bdLat - 0.006
    z <- sqrt(x ^ 2 + y ^ 2) - 0.00002 * sin(y * XM_PI)
    theta <- atan2(y, x) - 0.000003 * cos(x * XM_PI)
    
    output[! na_rows, ] <- cbind(z * sin(theta), z * cos(theta))
    
    # check if outside China
    if (! force){
        wgs <- gcj_to_wgs(output, force=TRUE)
        outsideChina <- which(
            isOutOfChina(wgs, gcs="BD-09", accurate=accurate_cn_bou))
        if (length(outsideChina) > 0){
            invisible(warning(
                'coordinates #', paste(outsideChina, collapse=', '), ' are out of China ',
                'after conversion to WGS-84. They will not be converted to GCJ-02.'))
        }
        output[outsideChina, ] <- as.matrix(input[outsideChina, ])
    }
    
    output <- as.data.frame(output, stringsAsFactors=FALSE)
    colnames(output) <- c("lat", "lng")
    return(output)
}

#' @export
bd2gcj <- bd_to_gcj


#' @export
#' @aliases wgs2bd
#' @rdname transform_coord
#' @examples
#' \dontrun{
#' 
#' ## ========== wgs_to_bd ==========
#' 
#' # Tiananmen square's WGS-84 coordinate is c(39.90734, 116.39089)
#' # http://api.map.baidu.com/marker?location=39.91509,116.40350&title=
#' # Tiananmen&content=Tiananmen%20square&output=html
#'
#' ## Single point
#' wgs_to_bd(list(39.90734, 116.39089))  # or
#' wgs_to_bd(39.90734, 116.39089)  # get
#' #           lat       lng
#' # [1,] 39.91508  116.4035
#'
#' ## Multiple points
#' ### Coordinate pairs or lat / lon vectors
#' wgs_to_bd(list(c(39.90734, 116.39089), c(39.90734, 116.39089)))  # or
#' wgs_to_bd(c(39.90734, 39.90734), c(116.39089, 116.39089))  # get
#' #           lat       lng
#' # [1,] 39.91508  116.4035
#' # [2,] 39.91508  116.4035
#'
#' ### Matrix
#' m <- matrix(c(39.90734, 116.39089, 39.90734, 116.39089, 39.90734, 
#'               116.39089), nrow=2)
#' m
#' #           [,1]       [,2]       [,3]
#' # [1,]  39.90734   39.90734   39.90734
#' # [2,] 116.39089  116.39089  116.39089
#' wgs_to_bd(m)  # get
#' #           lat       lng
#' # [1,] 39.91508  116.4035
#' # [2,] 39.91508  116.4035
#' # [3,] 39.91508  116.4035
#'
#' ### data.frame
#' df <- data.frame(lat=c(39.90734, 39.90734, 39.90734, NA),
#'                  lon=c(116.39089, 116.39089, 116.39089, 116.39089))
#' wgs_to_bd(df)  # get
#' #           lat       lng
#' # [1,] 39.91508  116.4035
#' # [2,] 39.91508  116.4035
#' # [3,] 39.91508  116.4035
#' # [4,]       NA        NA
#' }
#'
wgs_to_bd <- function(y, ..., force=FALSE, accurate_cn_bou=TRUE){
    # Global coord (WGS-84) -> Baidu GeoSystem (BD-09)
    intermed <- suppressWarnings(
        wgs_to_gcj(y, ..., force=force, accurate_cn_bou=accurate_cn_bou))
    
    return(gcj_to_bd(intermed, force=force, accurate_cn_bou=accurate_cn_bou))
}

#' @export
wgs2bd <- wgs_to_bd


#' @export
#' @aliases bd2wgs
#' @rdname transform_coord
#' @examples
#' \dontrun{
#' 
#' ## ========== bd_to_wgs ==========
#' 
#' # Tiananmen square's BD-06 coordinate is c(39.91509, 116.40350)
#' # http://www.openstreetmap.org/#map=19/39.90734/116.39089
#'
#' ## Single point
#' bd_to_wgs(list(39.91509, 116.40350))  # or
#' bd_to_wgs(39.91509, 116.40350)  # get
#' #           lat       lng
#' # [1,] 39.90734  116.3909
#'
#' ## Multiple points
#' ### Coordinate pairs or lat / lon vectors
#' bd_to_wgs(list(39.91509, 116.40350), list(39.91509, 116.40350))  # or
#' bd_to_wgs(c(39.91509, 39.91509), c(116.40350, 116.40350))  # get
#' #           lat       lng
#' # [1,] 39.90734  116.3909
#' # [2,] 39.90734  116.3909
#'
#' ### Matrix
#' m <- matrix(c(39.91509, 116.40350, 39.91509, 116.40350, 39.91509, 
#'               116.40350), nrow=2)
#' m
#' #           [,1]       [,2]       [,3]
#' # [1,]  39.90734   39.90734   39.90734
#' # [2,] 116.39089  116.39089  116.39089
#' bd_to_wgs(m)  # get
#' #           lat       lng
#' # [1,] 39.90734  116.3909
#' # [2,] 39.90734  116.3909
#' # [3,] 39.90734  116.3909
#'
#' ### data.frame
#' df <- data.frame(lat=c(39.91509, 39.91509, 39.91509, NA),
#'                  lon=c(116.40350, 116.40350, 116.40350, 116.40350))
#' bd_to_wgs(df)  # get
#' #           lat       lng
#' # [1,] 39.90734  116.3909
#' # [2,] 39.90734  116.3909
#' # [3,] 39.90734  116.3909
#' # [4,]       NA        NA
#' }
#'
bd_to_wgs <- function(y, ..., force=FALSE, accurate_cn_bou=TRUE){
    # Coord Baidu GeoSystem (BD-09) -> Global GeoSystem (WGS-84)
    intermed <- suppressWarnings(
        bd_to_gcj(y, ..., force=force, accurate_cn_bou=accurate_cn_bou))
    return(gcj_to_wgs(intermed, force=force, accurate_cn_bou=accurate_cn_bou))
}

#' @export
bd2wgs <- bd_to_wgs
