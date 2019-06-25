# --------------Work functions for coord conversion----------------

#' @importFrom sp coordinates<- proj4string<- CRS proj4string
xy2SpatialPoints <- function(y, ..., proj4string=NULL){
    coord <- getCoordArgs(y, ...)
    # note: does not accept NAs
    coordinates(coord) <- ~lon+lat
    if (! 'cn_bou' %in% names(pkgenv)){
        pkgenv$cn_bou <- lapply(c('WGS-84', 'GCJ-02', 'BD-09'), get_cn_bou)
        names(pkgenv$cn_bou) <- c('WGS-84', 'GCJ-02', 'BD-09')
    }
    proj4string(coord) <- if (is.null(proj4string))
        CRS(proj4string(pkgenv$cn_bou[['WGS-84']])) else CRS(proj4string)
    return(coord)
}

#' @importFrom sp over
#' @importFrom glue glue
is_point_in_polygon <- function(pts, polygon){
    stopifnot(inherits(pts, 'SpatialPoints'))
    stopifnot(inherits(polygon, 'SpatialPolygons'))
    
    o <- over(pts, polygon)
    coords <- pts@coords
    nms <- glue('{sprintf("%.3f", coords[, "lon"])},',
                '{sprintf("%.3f", coords[, "lat"])}')
    names(o) <- nms
    return(! is.na(o))
}


#' Geographic coordinate number formatters
#'
#' \code{lat_coord} and \code{lon_coord} are wrappers of \code{\link[scales]{number}} 
#' to format the geographic coordinate values. \code{coord_format} is a function
#' factory to produce formatter functions. 
#' 
#' @param x numeric vector to format
#' @param accuracy Number to round to, NULL for automatic guess.
#' @param scale A scaling factor: \code{x} will be multiply by scale before formating 
#' (useful if the underlying data is on another scale, e.g. for computing 
#' percentages or thousands).
#' @param prefix Symbols to display before and after value. By default, it is
#' a named character vector \code{c(pos="", zero="", neg="")}. If not in such format,
#' the function will automatically format the prefix.
#' @param unit The units to append.
#' @param sep The separator between the number and the unit label.
#' @param suffix Symbols to display before and after value. By default, it is
#' a named character vector \code{c(pos="N", zero="", neg="S")} for 'lat' and
#' \code{c(pos="E", zero="", neg="W")} for 'lon'. If not in such format, 
#' the function will automatically format the suffix.
#' @param big.mark Character used between every 3 digits to separate thousands.
#' @param decimal.mark The character to be used to indicate the numeric decimal point.
#' @param trim Logical, if FALSE, values are right-justified to a common width 
#' (see \code{\link{format}}()).
#' @param ... Other arguments passed on to \code{\link{format}}().
#'
#' @details \code{coord_format} is especially useful when plotting maps with \pkg{ggplot2}.
#' The \code{\link{scale_x_continuous}()} and \code{\link{scale_y_continuous}} 
#' function accepts functions as arguments \code{breaks} and \code{labels}. You 
#' can contruct your own formatter function using \code{coord_format}.
#'
#' @return a character vector
#' @export
#' 
#' @seealso \code{\link{number_format}()} is a function factory to produce functions.
#' 
#' @rdname geocoord_format
#' @examples
#' \dontrun{
#' lat_coord(c(-10, 0, 20), sep=' ', unit='deg')
#' # [1] "10 degS" "0 deg"   "20 degN"
#' 
#' lat_coord(c(-10, 0, 20), sep=' ', prefix=c(
#'     pos='S.lat', zero='', neg='N.lat'), suffix='', unit='deg')
#' # [1] "N.lat10 deg" "0 deg"       "S.lat20 deg"
#' }
lat_coord <- function(
    x, accuracy = 1, scale = 1, prefix = c(pos="", zero="", neg=""), 
    unit = "\u00b0", sep = "", suffix = c(pos="N", zero="", neg="S"), 
    big.mark = " ", decimal.mark = ".", trim = TRUE, ...){
    # use unit_format() to gen functions

    stopifnot(all(abs(x) <= 180, na.rm=TRUE))
    
    x[!is.na(x) & x > 90] <- x[!is.na(x) & x > 90] - 180
    x[!is.na(x) & x < -90] <- x[!is.na(x) & x < -90] + 180
    coord_tick(x, accuracy = accuracy, scale = scale, prefix = prefix, 
               suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark, 
               trim = trim, unit = unit, sep = sep, ...)
}

#' @export
#' 
#' @rdname geocoord_format
#' @examples
#' \dontrun{
#' lon_coord(c(-10, 0, 20), sep=' ', unit='deg')
#' # [1] "10 degW" "0°"   "20 degE"
#' 
#' lon_coord(c(-10, 0, 20), sep=' ', prefix=c(
#'     pos='E.lon', zero='', neg='W.lon'), suffix='', unit='deg')
#' # [1] "W.lon10 deg" "0 deg"       "E.lon20 deg"
#' }
lon_coord <- function(
    x, accuracy = 1, scale = 1, prefix = c(pos="", zero="", neg=""), 
    unit = "\u00b0", sep = "", suffix = c(pos="E", zero="", neg="W"), 
    big.mark = " ", decimal.mark = ".", trim = TRUE, ...){

    stopifnot(all(abs(x) <= 360, na.rm=TRUE))
    
    x[!is.na(x) & x > 180] <- x[!is.na(x) & x > 180] - 360
    x[!is.na(x) & x < -180] <- x[!is.na(x) & x < -180] + 360
    coord_tick(x, accuracy = accuracy, scale = scale, prefix = prefix, 
               suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark, 
               trim = trim, unit = unit, sep = sep, ...)
}

#' @export
#' @rdname geocoord_format
#' @examples 
#' \dontrun{
#' 
#' # --------construct a function factory------------
#' library(ggplot2)
#' ggplot(aes(long, lat, group=group), data=map_data('usa')) + geom_path() +
#'   scale_x_continuous(labels=lon_coord) + scale_y_continuous(labels=lat_coord)
#' 
#' ## define new formatter functions
#' lon_lbl <- function(x, ...) 
#'    coord_format(prefix=c(pos='East', zero='', neg='West'), 
#'                 unit='deg', sep=' ', suffix='', ...)(x)
#' lat_lbl <- function(x, ...)
#'    coord_format(prefix=c(pos='North', zero='', neg='South'), 
#'                 unit='deg', sep=' ', suffix='', ...)(x)
#' ggplot(aes(long, lat, group=group), data=map_data('usa')) + geom_path() +
#'   scale_x_continuous(labels=lon_lbl) + scale_y_continuous(labels=lat_lbl)
#' }
coord_format <- function(accuracy = 1, scale = 1, 
                         prefix=c(pos="", zero="", neg=""), unit="\u00b0", sep=" ",
                         suffix=c(pos="", zero="", neg=""), big.mark = " ",
                         decimal.mark = ".", trim = TRUE, ...){
    function(x) coord_tick(
        x=x, accuracy = accuracy, scale = scale, prefix = prefix, 
        suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark,
        trim = trim, unit = unit, sep = sep, ...)
}

#' @importFrom aseskit iif ifnull ifnull aline
#' @importFrom dplyr if_else
#' @importFrom scales number
coord_tick <- function(x, accuracy = 1, scale = 1, prefix=c(pos="", zero="", neg=""), 
                       suffix=c(pos="", zero="", neg=""), big.mark = " ",
                       decimal.mark = ".", trim = TRUE, unit = unit, sep = sep, ...){
    stopifnot(is.character(prefix))
    prefix <- aline(prefix, 3, append='')
    prefix <- paste0(prefix, sep)
    if (! all(ifnull(names(prefix), '') %in% c('pos', 'zero', 'neg')))
        names(prefix) <- c('pos', 'zero', 'neg')
    stopifnot(is.character(suffix))
    suffix <- aline(suffix, 3, append='')
    suffix <- paste0(sep, unit, suffix)
    if (! all(ifnull(names(suffix), '') %in% c('pos', 'zero', 'neg')))
        names(suffix) <- c('pos', 'zero', 'neg')
    
    prefix <- if_else(x > 0, prefix['pos'], if_else(x < 0, prefix['neg'], prefix['zero']))
    suffix <- if_else(x > 0, suffix['pos'], if_else(x < 0, suffix['neg'], suffix['zero']))
    number(abs(x), accuracy = accuracy, scale = scale, prefix = prefix, 
           suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark,
           trim = trim, ...)
}


#' Detect whether the points are outside China
#' 
#' Analyzes the coordinates (lat, lon) to check whether they are in or out of China. 
#' You can choose to return a logical vector or a ggplot2 object for quick 
#' visualization. 
#' 
#' @param y one of \itemize{
#' \item coordinate pair: a list of coordinate vectors \code{list(lat1, lon1)},
#' in which case you can provide other coordiante vectors in \code{...} \cr
#' \item latitude vector: latitude number(s), in which case you need to provice 
#'  longitute number(s) in \code{...} \cr
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
#' @param accurate logical, whether use accurate China boundary to analyze
#' the coordinates. Default TRUE. When using the accurate China boundary,
#' the function will check if the coordinates (Lat, Lon) are actually inside 
#' China's territory. When set FALSE, the function will use a very vague rectangle 
#' defined by \code{xlim} and \code{ylim} to reprensent China boundary.
#' @param gcs character, the geo-coordinate system, 'WGS-84', 'GCJ-02' or 'BD-09'.
#' Default 'WGS-84'. The function will analyze the points (Lat, Lon) under the
#' specific gcs.
#' @param plot logical, if generate a plot to show the location of the points.
#' @param canvas character, 'china' or 'world'. It is only effective when 
#' \code{plot} == TRUE. If 'china', the plot is restricted to east Asia, with
#' the projection 'azequalarea', orientation of c(30, 105) with no rotation. If
#' 'world', the plot is the full 'world' map, with the projetion 'mercator'.
#' @param xlim numeric vector of length 2. The longitude limits of the China map.
#' Default c(72.004, 137.8347). when \code{accurate} is FALSE, the rectangle
#' made by xlim and ylim will be used as China.
#' @param ylim numeric vector of length 2. The latitude limits of the China map.
#' Default c(0.8293, 55.8271). when \code{accurate} is FALSE, the rectangle
#' made by xlim and ylim will be used as China boundary.
#' 
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @details The most popular open-source coordinate transformation algorithm 
#' regards China as a rectangle. Points outside this rectangle are treated as 
#' 'outside China', of which case the conversion from WGS-84 ==> GCJ-02 / BD-09
#' is not conducted. This function inherits this algorithm as well. As a step 
#' further, it prepares an 'accurate' method that use the actual China boundary
#' to analyze the input points. Due to the limitation of proj.4 library, it plots
#' the points and China boundary under 'WGS-84' system.
#' 
#' @import ggplot2
#' @importFrom aseskit iif ifna
#' @importFrom glue glue
#' @importFrom maptools map2SpatialPolygons
#' @importFrom sp proj4string
#' @export
#' @rdname is_out_of_china
#' 
#' @return depends on \code{plot}, \itemize{
#'  \item \code{TRUE}: returns a 'ggplot2' object. \cr
#'  \item \code{FALSE}; returns a logical vector with the same length of Lat (or Lon).
#' }
#' @aliases is_out_of_china
#' @seealso 
#'  Refer to \code{\link{transform_coord}} function family for argument \code{y, ...}
#' @examples 
#' \dontrun{
#' is_out_of_china(c(10, 30, 40, 50, 70), c(100, 120, 100, 140, 160), 
#'                 accurate_=TRUE)  
#' # 10.00,100.00 30.00,120.00 40.00,100.00 50.00,140.00 70.00,160.00 
#' # TRUE         FALSE        FALSE        TRUE         TRUE
#' 
#' is_out_of_china(c(10, 30, 40, 50, 70), c(100, 120, 100, 140, 160), 
#'                 accurate=FALSE)
#' # 10.00,100.00 30.00,120.00 40.00,100.00 50.00,140.00 70.00,160.00 
#' # FALSE        FALSE        FALSE        TRUE         TRUE 
#' 
#' ## visualize the positions
#' is_out_of_china(c(10, 30, 40, 50, 70), c(100, 120, 100, 140, 160), 
#'                 plot=TRUE, canvas='china')
#' is_out_of_china(c(10, 30, 40, 50, 70), c(100, 120, 100, 140, 160), 
#'                 plot=TRUE, canvas='world')              
#' }
isOutOfChina <- function(y, ..., accurate=TRUE, gcs=c('WGS-84', 'GCJ-02', 'BD-09'), 
                         plot=FALSE, canvas=c('china', 'world'), 
                         xlim=c(72.004, 137.8347), ylim=c(0.8293, 55.8271)){
    # to check if the places are out of China
    latlng <- getCoordArgs(y, ...)
    stopifnot(is.logical(accurate))
    gcs <- match.arg(gcs)
    stopifnot(is.logical(plot))
    canvas <- match.arg(canvas)
    stopifnot(is.numeric(xlim) && length(xlim) >= 2)
    stopifnot(is.numeric(ylim) && length(ylim) >= 2)
    xlim <- range(xlim, na.rm=TRUE)
    ylim <- range(ylim, na.rm=TRUE)
    
    # initialize accurate cn_bou
    if (! 'cn_bou' %in% names(pkgenv)){
        pkgenv$cn_bou <- lapply(c('WGS-84', 'GCJ-02', 'BD-09'), get_cn_bou)
        names(pkgenv$cn_bou) <- c('WGS-84', 'GCJ-02', 'BD-09')
    }
    proj4str <- proj4string(pkgenv$cn_bou[['WGS-84']])
    
    na_coord <- is.na(latlng$lat) | is.na(latlng$lon)
    
    if (all(na_coord)){
        o <- rep(FALSE, nrow(latlng))
    }else{
        coord <- xy2SpatialPoints(latlng[! na_coord, ])
        o <- rep(NA, nrow(latlng))
        if (accurate){
            o[! na_coord] <- ! is_point_in_polygon(coord, pkgenv$cn_bou[[gcs]])
        }else{
            xy <- data.frame(lat=ylim[c(1, 2, 2, 1, 1)], lng=xlim[c(1, 1, 2, 2, 1)])
            if (gcs == 'GCJ-02') xy <- wgs_to_gcj(xy, force=TRUE)
            if (gcs == 'BD-09') xy <- wgs_to_bd(xy, force=TRUE)
            cn_bou <- structure(
                list(x=xy$lng, y=xy$lat, names='rectangle'), class='map')
            cn_bou <- map2SpatialPolygons(
                cn_bou, IDs=cn_bou$names, proj4string=CRS(proj4string(proj4str)))
            o[! na_coord] <- ! is_point_in_polygon(coord, cn_bou)
            # o <- ! (Lat >= 0.8293 & Lat <= 55.8271 & Lon >= 72.004 & Lon <= 137.8347)
            # o <- ! (Lon > 73.66 & Lon < 135.05 & Lat > 3.86 & Lat < 53.55)
        }
        outOfRange <- ifna(abs(latlng$lat)>90 | abs(latlng$lon)>180, TRUE)
        if (any(outOfRange)) o[outOfRange] <- NA
    }
    names(o) <- glue('{sprintf("%.3f", latlng$lat)},{sprintf("%.3f", latlng$lon)}')
    
    if (plot){
        # base map
        if (canvas == 'china'){
            gg <- ggplot() + 
                coord_map('azequalarea', orientation=c(30, 105, 0), 
                          xlim=range(c(xlim, latlng$lon), na.rm=TRUE) + c(-0.5, 0.5), 
                          ylim=range(c(ylim, latlng$lat), na.rm=TRUE) + c(-0.5, 0.5)) +
                geom_polygon(aes(long, lat, group=group), data=fortify(
                    map_data('world')), fill='transparent', color='gray85',
                    size=0.1)
        }else if (canvas == 'world'){
            gg <- ggplot() + coord_quickmap(xlim=c(0, 360), ylim=c(-90, 90)) +
                geom_polygon(aes(long, lat, group=group), data=fortify(
                    map_data('world2')), fill='transparent', color='gray90',
                    size=0.1)
        }
        # china map
       
        gg <- gg +
            geom_polygon(aes(long, lat, group=group), data=fortify(
                if (accurate) pkgenv$cn_bou[['WGS-84']] else cn_bou),
                fill='gray90', alpha=0.4, color='gray75', size=0.1) +
            theme_minimal() + 
            scale_x_continuous(labels=lon_coord) +
            scale_y_continuous(labels=lat_coord)
        if (! all(is.na(o))){
            pts <- data.frame(x=latlng$lon, y=latlng$lat, outsideChina=c('Y', 'N')[2-o])
            pts$outsideChina <- factor(pts$outsideChina, levels=c('Y', 'N'))
            gg <- gg + geom_point(aes(x, y, color=outsideChina), data=pts) +
                labs(title=paste0('Are the points outside China? (', gcs, ')'), 
                     x='Longitude', y='Latitude', color='Outside China?')
        }else{
            invisible(warning('All the coordinates (Lat, Lon) are invalid.'))
        }
        gg + theme(legend.position=c(0.8, 1), legend.direction='horizontal',
                 panel.grid=element_line(color='gray95', size=0.01, linetype=1),
                 axis.title=element_text(color='gray50'),
                 axis.text=element_text(color='darkgray')) 
    }else{
        return(o)
    }
}

#' @export
is_out_of_china <- isOutOfChina

transformLat <- function(x, y){
    # China encrpytion of latitudes
    stopifnot(length(x) == length(y))
    ret <- -100.0 + 2.0 * x + 3.0 * y + 0.2 * y ^ 2 + 0.1 * x * y + 0.2 * sqrt(abs(x))
    ret <- ret + (20.0 * sin(6.0 * x * pi) + 20.0 * sin(2.0 * x * pi)) * 2.0 / 3.0
    ret <- ret + (20.0 * sin(y * pi) + 40.0 * sin(y / 3.0 * pi)) * 2.0 / 3.0
    ret <- ret + (160.0 * sin(y / 12.0 * pi) + 320.0 * sin(y * pi / 30.0)) * 2.0 / 3.0
    return(ret)
}

transformLon <- function(x, y){
    # China encryption of longitudes
    stopifnot(length(x) == length(y))
    ret <- 300.0 + x + 2.0 * y + 0.1 * x ^ 2 + 0.1 * x * y + 0.1 * sqrt(abs(x))
    ret <- ret + (20.0 * sin(6.0 * x * pi) + 20.0 * sin(2.0 * x * pi)) * 2.0 / 3.0
    ret <- ret + (20.0 * sin(x * pi) + 40.0 * sin(x / 3.0 * pi)) * 2.0 / 3.0
    ret <- ret + (150.0 * sin(x / 12.0 * pi) + 300.0 * sin(x / 30.0 * pi)) * 2.0 / 3.0
    return(ret)
}

formatCoordArgs <- function(input){
    # clean up the output of getCoordArgs
    # yield data.frame [lat, lon]
    if (! is.null(input)){
        stopifnot(! is.null(dim(input)))
        out <- if (is.matrix(input)) 
            as.data.frame(input, stringsAsFactors=FALSE) else input
        names(out) <- c('lat', 'lon')
        out[is.na(out$lat) | is.na(out$lon), ] <- c(NA_real_, NA_real_)
        if (any(abs(out['lat']) > 90 | abs(out['lon']) > 180, na.rm=TRUE))
            stop("input shoudl be arranged as [lat, lon]. Lat should be within ",
                 "[-90, 90], Lon should be within [-180, 180].\n",
                 "Check if you have mistakenly exchanged the column order.")
        row.names(out) <- NULL
        return(out)
    }
}

getCoordArgs <- function(y, ...){
    ## Coarse params to a 2-col data.frame
    UseMethod(".getCoordArgs")
}

#' @importFrom aseskit aline
.getCoordArgs.vector <- function(y, ...){
    # args
    # y: lon vec
    # ...: lat vec
    #
    y <- as.numeric(y)
    x <- list(...)
    if (length(y) == 0){
        out <- NULL
    }else{
        ## y refers to a vector of lat/lon
        ## return all paired points
        if (length(x) > 0){
            x <- x[[1]]
            x <- aline(unlist(x), length.out=length(y), append=NA_real_)
            out <- data.frame(lat=y, lon=x)
        }else{
            if (length(y) > 1){
                out <- data.frame(lat=y[1], lon=y[2])
            }else{
                stop("Only y is provided with insufficient length. ",
                     "No idea how to pair the points.")
            }
        }
    }
    return(formatCoordArgs(out))
}

#' @importFrom aseskit aline
#' @importFrom dplyr bind_rows
.getCoordArgs.list <- function(y, ...){
    # Args:
    # y: a series of points, e.g., list(c(1, 3), c(5, 6))
    # ...: other (lat, lon) pairs
    if (length(y) < 2) {
        out <- NULL
        invisible(warning('y must at least contain one coordinate pair, e.g., list(lat, lon).'))
    }else{
        ## return all possible points
        extract_pairs <- function(lst){
            if (length(lst) == 0){
                stop('No data found.')
            }else if (length(lst) == 1){
                lat <- lst[[1]][[1]]
                lon <- lst[[1]][[2]]
            }else{
                lat <- unlist(lst[[1]])
                lon <- unlist(lst[[2]])
            }
            
            len <- max(length(lat), length(lon), na.rm=TRUE)
            o <- matrix(c(aline(lat, len, append=NA_real_),
                          aline(lon, len, append=NA_real_)), ncol=2)
            return(o)
        }
        dots <- lapply(list(...), function(ll) if (is.list(ll)) ll else list(ll))
        y <- c(list(y), dots)
        out <- lapply(y, extract_pairs) %>% do.call('rbind', .)
        out <- as.data.frame(out, stringsAsFactors=FALSE)
        names(out) <- c('lat', 'lon')
    }
    return(formatCoordArgs(out))
}

#' @importFrom dplyr if_else
.getCoordArgs.matrix <- function(y, ...){
    if (ncol(y) == 1 || nrow(y) == 1){
        y <- as.vector(y)
        if (length(y) == 1) {
            out <- data.frame(lat=NA_real_, lon=NA_real_)
        }else{
            out <- data.frame(lat=y[1], lon=y[2])
        }
    }else{
        stopifnot(all(abs(as.vector(y)) <= 180, na.rm=TRUE))
        colcat <- rowcat <- vector("character", length=2L)
        colcat <- if_else(apply(y[, 1:2], 2, function(vec) 
            any(abs(vec) > 90, na.rm=TRUE)), 'lon', 'lat')
        rowcat <- if_else(apply(y[1:2, ], 1, function(vec) 
            any(abs(vec) > 90, na.rm=TRUE)), 'lon', 'lat')
        
        fail_msg <- "Cannot distinguish lat and lon in the matrix either col 1-2 or row 1-2."
        
        if (colcat[1] != colcat[2]){
            out <- structure(data.frame(as.numeric(y[, 1]), as.numeric(y[, 2])),
                             names=colcat, concat="col")
        }else if (rowcat[1] != rowcat[2]){
            out <- structure(data.frame(as.numeric(y[1, ]), as.numeric(y[2, ])),
                             names=rowcat, concat="row")
        }else{
            if (colcat[1] == "lon" && rowcat[1] == "lon") stop(fail_msg)
            out <- NA
            if (ncol(y) <= nrow(y)){  # long
                attr(out, "concat") <- if_else(colcat[1] == "lon", "row", "col")
            }else{
                attr(out, "concat") <- if_else(rowcat[1] == "lon", "col", "row")
            }
            if (attr(out, "concat") == "row") 
                out <- data.frame(lat=as.numeric(y[1, ]), lon=as.numeric(y[2, ]))
            else if (attr(out, "concat") == "col")
                out <- data.frame(lat=as.numeric(y[, 1]), lon=as.numeric(y[, 2]))
        }
        
        if (identical(names(out), c("lon", "lat"))) 
            invisible(message("The order of the variables you input has been ",  
                              "reversed to match the valid lat/lon range."))
        out <- out[, c("lat", "lon")]
    }
    return(formatCoordArgs(out))
}

.getCoordArgs.data.frame <- function(y, ...){
    ## get the first two columns
    o <- y[, 1:2]
    out <- vapply(o, function(col) {
            if (! is.numeric(col)) as.numeric(as.character(col)) else col
        }, FUN.VALUE=numeric(nrow(o)))
    dim(out) <- dim(y)
    out <- as.data.frame(out, stringsAsFactors=FALSE)
    return(formatCoordArgs(out))
}

.getCoordArgs.default <- .getCoordArgs.vector

# ================Format gcdf or rgcdf=======================

format_gcdf <- function(gcdf, rownames=TRUE, ics_china=c("GCJ-02", "BD-09", "WGS-84"), 
                        ics_intl=c("WGS-84", "BD-09", "GCJ-02"), ocs=NULL, ...){
    # format a gcdf data.frame yielded by parse_xxxmap_api_results
    # 1. set the rownames
    # 2. transform lat and lng
    
    # check args
    stopifnot(is.data.frame(gcdf))
    stopifnot(all(c('lat', 'lng') %in% names(gcdf)))
    stopifnot(is.numeric(gcdf$lat) && is.numeric(gcdf$lng))
    stopifnot(is.logical(rownames) || is.character(rownames))
    ics_china <- match.arg(ics_china)
    ics_intl <- match.arg(ics_intl)
    if (! is.null(ocs)) ocs <- match.arg(ocs, c("WGS-84", "BD-09", "GCJ-02"))
    dots <- list(...)
    
    if (nrow(gcdf) == 0) return(gcdf)
    
    # rownames
    if (is.character(rownames)){
        row.names(gcdf) <- rownames
    }else if (is.logical(rownames)){
        if (! rownames) row.names(gcdf) <- NULL
    }
    
    # transform coords
    if (! is.null(ocs)){
        insideChina <- which(! isOutOfChina(gcdf$lat, gcdf$lng))
        outsideChina <- which(isOutOfChina(gcdf$lat, gcdf$lng))
        if (length(insideChina) > 0){
            if (!is.null(ics_china) && !is.null(ocs))
                gcdf[insideChina, c('lat', 'lng')] <- conv_coord(
                    gcdf[insideChina, 'lat'], gcdf[insideChina, 'lng'], 
                    from=ics_china, to=ocs)[c('lat', 'lng')]
        }
        if (length(outsideChina) > 0){
            if (ocs != 'WGS-84'){
                message('wrong usage: for address out of China, ',
                        'ocs can only be set to "WGS-84"', appendLF = TRUE)
            }
            if (!is.null(ics_intl))
                gcdf[outsideChina, c('lat', 'lng')] <- conv_coord(
                    gcdf[outsideChina, 'lat'], gcdf[outsideChina, 'lng'], 
                    from=ics_intl, to='WGS-84')[c('lat', 'lng')]
        }
    }
    
    return(gcdf)
}

NULLtoNA <- function(x){
	# fill NULL with NA, used for geocode functions
    # deprecated. equivalent to ifnull(x, NA)
	# author: Cai Jun
	#
    stop('Deprecated')
    if (is.null(x)) return(NA)
    if (is.character(x) & length(x) == 0) return(NA)
    x
}

# ================accurate china boundary=====================

#' @importFrom maps map SpatialPolygons2map
#' @importFrom maptools map2SpatialPolygons unionSpatialPolygons
#' @importFrom sp CRS SpatialPolygonsDataFrame
#' @importFrom raster bind
get_cn_bou <- function(gcs=c('WGS-84', 'GCJ-02', 'BD-09'), 
                       proj4str='+proj=longlat +datum=WGS84'){
    # return a SptialPolygons object
    gcs <- match.arg(gcs)
    stopifnot(is.character(proj4str))
    
    proj4str <- CRS(proj4str)
    cn <- map(database='world', regions=c('china', 'taiwan'), fill=TRUE, 
              plot=FALSE)
    diaoyudao <- structure(list(
        x=c(123.458333, 123.456667, 123.456667, 123.458333, 123.678333,
            123.685, 123.688333, 123.69, 123.69, 123.556667, 123.553333, 
            123.46, 123.4583333),
        y=c(25.735, 25.7366667, 25.74, 25.745, 25.93, 25.93, 25.9266667, 
            25.9233333, 25.9216667, 25.72, 25.72, 25.7333333, 25.735),
        names='china:diaoyu dao'), class='map')
    sea_bou <- structure(list(
        x=c(122.705, 122.705, 122.57, 122.545, 122.518333, 122.263333,
            120.885, 119.9033333, 121.3466667, 121.64, 122.243333, 123.156667,
            123.161667, 122.945, 122.275, 121.916667, 121.911667, 121.13,
            120.506667, 120.405, 119.938333, 119.478333, 118.236667,
            117.688333, 117.248333, 117.231667, 116.495, 115.125, 113.966667,
            112.798333, 112.358333, 111.273333, 111.213333, 110.493333,
            110.485, 110.14, 110.05, 109.701667, 109.696667, 109.573333,
            109.126667, 108.951667, 108.688333, 108.685, 108.676667,
            108.675, 108.621667, 108.6, 108.643333, 108.056371, 114.559386,
            124.148232, 122.705),
        y=c(37.4, 37.395, 36.963333, 36.918333, 36.895, 36.746667, 35.893333, 
            35.003333, 33.363333, 33.015, 31.421667, 30.735, 30.725, 30.168333, 
            28.888333, 28.398333, 28.391667, 27.465, 26.3766667, 26.156667,
            25.43, 24.976667, 24.161667, 23.531667, 23.215, 23.205, 22.935, 
            22.315, 21.808333, 21.568333, 21.461667, 19.975, 19.883333, 18.666667,
            18.656667, 18.435, 18.383333, 18.183333, 18.183333, 18.158333,
            18.243333, 18.321667, 18.503333, 18.506667, 18.516667, 18.518333,
            18.841667, 19.193333, 19.351667, 21.532137, 44.787943, 39.742032,
            37.4),
        names='china:sea boundary'), class='map')
    nine_seg <- structure(list(
        x=c(107.979752, 107.460416, 109.298932, 109.619034, 109.771166, 
            109.846784, 110.323006, 110.313098, 110.272998, 110.212795, 110.067351,
            108.231179, 108.204353, 108.220643, 108.250218, 108.30114, 111.797, 
            112.471684, 112.848128, 115.544731, 116.238771, 118.538125,
            118.890084, 118.981628, 119.060343, 119.066019, 119.474162,
            119.655108, 119.999707, 121.179157, 121.91474, 122.505851,
            122.803898, 123.1617, 107.979752),
        y=c(21.469146, 18.674063, 16.210163, 15.770208, 15.461996, 15.187814, 
            12.24462, 11.93938, 11.693147, 11.5207, 11.254404, 7.072154, 
            6.774731, 6.413845, 6.21582, 6.017912, 3.41197, 3.575659, 3.752836,
            7.171338, 7.990553, 10.910131, 11.666285, 11.978519, 14.830115,
            16.002554, 18.013007, 18.40438, 18.990739, 20.82723, 21.697224,
            23.472637, 24.635875, 30.725, 21.469146),
        names='china:nine segments'), class='map')
    bou_lst <- lapply(list(cn, diaoyudao, sea_bou, nine_seg), function(lst){
        map2SpatialPolygons(lst, IDs=lst$names, proj4string=proj4str)
    })
    
    # merge
    cn_bou <- do.call('bind', bou_lst)  # raster::bind
    cn_bou <- unionSpatialPolygons(cn_bou, IDs=rep('china', length(cn_bou)))
    
    # convert
    if (! gcs == 'WGS-84'){
        cn_bou_map <- SpatialPolygons2map(SpatialPolygonsDataFrame(
            cn_bou, data=data.frame(NAME='China'), match.ID=FALSE))
        if (gcs == 'GCJ-02') {
            xy <- wgs_to_gcj(cn_bou_map$y, cn_bou_map$x, force=TRUE) 
        }else if (gcs == 'BD-09'){
            xy <- wgs_to_bd(cn_bou_map$y, cn_bou_map$x, force=TRUE) 
        }       
        cn_bou_map$x <- xy$lng
        cn_bou_map$y <- xy$lat
        cn_bou <- map2SpatialPolygons(
            cn_bou_map, IDs=cn_bou_map$names, proj4string=proj4str)
    }
    return(cn_bou)
}

# ================Demolish nested list [Not applicable now]==================
demolish_nested_list <- function(obj, elems=1:3, lists=NULL, ...){
    # get rows and cols from df, and flatten them into one row
    # results from gaode api often contains list(), convert to NA
    
    # TO DO: big issues parsing gaode results
    
    UseMethod(".demolish_nested_list", obj)
}

#' @importFrom aseskit iif ifempty
.demolish_nested_list.data.frame <- function(obj, rows=elems, cols=lists, 
                                             elems=1:3, lists=NULL, ...){
    stopifnot(is.null(rows) || is.numeric(rows))
    rows <- if (is.null(rows)) seq_len(nrow(obj)) else
        intersect(seq_len(nrow(obj)), rows)
    stopifnot(is.null(cols) || is.numeric(cols) || is.character(cols))
    if (is.null(cols)) cols <- seq_along(obj)
    if (is.character(cols)) cols <- which(names(obj) %in% cols)
    if (is.numeric(cols)) cols <- intersect(seq_len(ncol(obj)), cols)
    
    df <- obj[rows, cols]
    col_is_list <- which(vapply(df, is.list, FUN.VALUE=logical(1L)))
    
    if (length(col_is_list) > 0){
        lst <- lapply(df, function(l){
            if (is.list(l)){
                sapply(l, function(ll){
                    if (is.data.frame(ll)){
                        flatten_df(ifempty(ll, NA_character_))
                    }else{
                        unlist(ifempty(ll, NA_character_))
                    }
                })
            }else if (is.data.frame(l)){
                as.data.frame(t(ifemtpy(l, NA)), stringsAsFactors=FALSE)
            }else{
                ifempty(l, NA)
            }
        })
        
        o <- NULL
        for (i in seq_along(lst)){
            if (is.matrix(lst[[i]])){
                colnames(lst[[i]]) <- paste(names(lst)[[i]], colnames(lst[[i]]), sep="_")
                o <- cbind(o, lst[[i]])
            }else if (is.list(lst[[i]])){
                lsti_is_df <- vapply(lst[[i]], is.list, FUN.VALUE=logical(1L))
                if (any(lsti_is_df)){
                    o <- cbind(o, do.call("rbind", lst[[i]]))
                }else{
                    o <- cbind(o, unlist(lst[[i]]), recursive=TRUE)
                }
            }else{
                lst[[i]] <- matrix(lst[[i]], ncol=1)
                colnames(lst[[i]]) <- names(lst)[i]
                o <- cbind(o, lst[[i]])
            }
        }
        df <- structure(unlist(o), dim=dim(o)) %>% as.data.frame(stringsAsFactors=FALSE)
        names(df) <- attr(o, 'dimnames')[[2]]
    }
    
    return(df)
}

#' @importFrom aseskit iif ifempty
.demolish_nested_list.list <- function(obj, elems=1:3, lists=NULL, ...){
    stopifnot(is.null(elems) || is.numeric(elems))
    stopifnot(is.null(lists) || is.numeric(lists))
    if (is.null(lists)) lists <- seq_along(obj)
    
    out <- obj[lists]
    out <- unlist(ifempty(out, NA), recursive=FALSE)
    return(out)
}

.demolish_nested_list.matrix <- function(obj, elems=1:3, lists=NULL, ...){
    stopifnot(is.null(elems) || is.numeric(elems))
    stopifnot(is.null(lists) || is.numeric(lists))
    if (is.null(lists)) lists <- seq_len(ncol(obj))
    return(obj[elems, lists])
}

.demolish_nested_list.default <- function(obj, elems=1:3, lists=NULL, ...){
    stopifnot(is.null(elems) || is.numeric(elems))
    return(obj[elems])
}


