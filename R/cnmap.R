#' Load China Map on the Levels of Nation, Province, City or County
#'
#' Load the China map and do tiny modifications. Unlike the China map in packages 
#' \pkg{maps} and \pkg{mapdata}, the dataset in this package is based on Chinese 
#' official data. \code{cnmap0}, \code{cnmap1}, \code{cnmap2}, \code{cnmap3}
#' are convenient wrappers for \code{level} 0 (nation), 1 (province), 2 (city/prefecture), 
#' 3 (county) respectively. \cr
#' \figure{cnmap.png} \figure{bjmap.png}
#' 
#' @param level Character or integer, "nation" (0), "province" (1), "city" (2) or
#' "county" (3). Default 'nation'.
#' @param regions Character strings that fully/partially match ADCODE, NAME, 
#' NAME_LAB or NAME_EN fields. E.g., 'beijing' or '^110' to filter the map to 
#' Beijing area (case insensitive). It supports regular expression. Default NULL, 
#' indicating that all the regions will be shown.
#' @param simplify_level Numeric (0 ~ 1), the proportion of points to retain when
#' applying polygon simplification. See \pkg{rmapshaper}::\code{\link[rmapshaper]{ms_simplify}}.
#' If set 1, then the function will load maps with highest border precision (bigger
#' in size). It is useful to reduce the object size when drawing a city- or 
#' county-level map by 50\%-95\%. Default 1 (no simplification). \cr
#' Be careful to balance the trade-off of \code{simplify_level} and plot rendering.
#' The smaller \code{simplify_level} is, the longer it takes to simplify the polygons,
#' but the shorter it takes to render the plot. 
#' @param drop_fragment Logical, whether drop small fragments in the map (typically
#' small islands). It is useful to reduce the object size by around 10\%. Default FALSE. 
#' \cr When trying to drop fragments, it will \enumerate{
#'   \item at least preserve the largest polygon within a single Polygon slot;
#'   \item at least preserve the largest polygon with duplicated ADCODE.
#' }
#' @param preserve_topo Character ("all", "major", "none"), indicating how to preserve 
#' the topology when applying polygon simplification algorithm (only effective when
#'  \code{simplify_level} < 1). It is highly recommended to set \code{preserve_topo} == 
#'  "all" or "major" to avoid unexpected deformation. Default "all".
#' \itemize{
#'   \item \strong{all}: preserve topology of all the polygons, i.e. each Polygons 
#'     slot and its subordinate Polygon slots are retained.
#'   \item \strong{major}: only preserve topology of the Polygons slot. Some of 
#'     the Polygon slots inside the Polygons may be lost.
#'   \item \strong{none}: do not try to preserve topology, i.e., some of the 
#'     Polygons and/or Polygon slots inside them will be lost.
#' } 
#' @param fragment_area Numeric, the threshold area (in km^2) for "fragment" (only 
#' effective when \code{drop_fragment} = TRUE). It is not recommended to set this 
#' value too high. Default 0.003 (the size of the smallest county in China). 
#' @param output Character, "spdf", "map", "df", "sf". Default "spdf" 
#' (\code{sp::SpatialPolygonsDataFrame}).
#' 
#' @return By default, an \pkg{sp}::\code{\link[sp]{SpatialPolygonsDataFrame}} 
#' object (\code{output} == "spdf"). You can further process it by \describe{
#'  \item{convert to a \pkg{maps}::\code{\link[maps]{map}} object}{
#'    \pkg{maps}::\code{\link[maps]{SpatialPolygons2map}()}}
#'  \item{convert to a data.frame}{\pkg{ggplot2}::\code{\link[ggplot2]{fortify}()}}
#'  \item{convert to an \code{sf} object}{\pkg{sf}::\code{\link{st_as_sf}()}}
#' }
#' @importFrom rmapshaper ms_simplify ms_dissolve
#' @importFrom sf st_as_sf as_Spatial
#' @importFrom sp SpatialPolygonsDataFrame
#' @importFrom maps SpatialPolygons2map
#' @importFrom ggplot2 fortify
#' @export
#'
#' @seealso \itemize{
#'   \item check the vignettes: \code{vignette("drawChinaMap", package="asesgeo")}
#'   \item other useful functions: \code{\link[rmapshaper]{ms_simplify}}, 
#'      \code{\link[rmapshaper]{ms_dissolve}}, \code{\link[sp]{SpatialPolygonsDataFrame}}
#' }
#' @note \describe{
#'  \item{Direct vs indirect loading}{You can also directly load the map datasets 
#'   using \code{data(cnmap0)}, \code{data(cnmap1)}, \code{data(cnmap2)}, or \code{data(cnmap3)}.
#'   But it is more recommended to call \code{cnmap}() to enjoy the benefits by 
#'   its default settings. }
#'  \item{Map meta data & encoding}{Suppose you call \code{cn0 <- data(cnmap0)}, 
#'   the data set will show in .GlobalEnv. You can then check its meta data by 
#'   \code{cn0@@data} (cn0, as a SpatialPolygonsDataFrame object, is of S4 class) 
#'   or \code{slot(cn0, "data")}. 
#'   If you have issues with the character encoding, then try to switch to a Chinese 
#'   locale (\code{Sys.setlocale("LC_CTYPE", "Chs")}). \cr \cr 
#'   Some key fields in map meta dataset: \itemize{
#'     \item \strong{ADCODE}: The six-digit official administrative code
#'     \item \strong{NAME}: Full name of the administrative area
#'     \item \strong{NAME_LAB}: Short name of the administrative area
#'     \item \strong{NAME_EN}: English name of the administrative area
#'   }}
#' \item{Coordinate reference system}{
#'   The embeded map datasets are applying WGS84 coordinate system. You can define 
#'   your own coordinate reference system when plotting a map. China's official 
#'   recommendations:  \itemize{
#'   \item \pkg{maps}::\code{\link{map}}() or \pkg{ggplot2}::\code{\link[ggplot2]{coord_map}}:
#'     set it with \pkg{mapproj}::\code{\link{mapproject}()} \itemize{
#'       \item smaller maps: \code{projection="albers", parameters = c(24, 47)}
#'       \item larger maps: \code{projection="azequalarea", orientation = c(30, 105, 0)}
#'     }
#'   \item \pkg{ggplot2}::\code{\link[ggplot2]{coord_sf}}: set it using CRS
#'     \itemize{
#'       \item smaller maps: \code{coord_sf(crs="+init=epsg:4490 +proj=aea 
#'         +ellps=krass +lon_0=105 +lat_1=25 +lat_2=47")}
#'       \item larger maps: \code{coord_sf(crs="+init=epsg:4490 +proj=laea 
#'         +ellps=GRS80 +lon_0=105 +lat_0=30")}
#'     }
#'   }}
#' }
#' @examples
#' \dontrun{
#' # to yield the 3-level China map as shown on the top
#' library(maps)
#' op <- par(mar=c(0, 0, 0, 0))
#' map(cnmap(3), col="gray90", lwd=0.2, projection="albers", 
#'   parameters = c(24, 47))  # cnmap(3) is equal to cnmap3()
#' map(cnmap(2), col="gray75", lwd=0.5, add=TRUE, projection="albers", 
#'   parameters = c(24, 47))
#' map(cnmap(1), col="gray30", lwd=0.8, add=TRUE, projection="albers", 
#'   parameters = c(24, 47))
#' map(cnmap(0), col="black", lwd=1, add=TRUE, projection="albers", 
#'   parameters = c(24, 47))
#' par(op) 
#' 
#' # to draw the 2-level Beijing map as shown on the top
#' library(ggplot2)
#' library(sf)
#' library(extrafont)
#' bjmap2 <- st_as_sf(cnmap(2, "^110", simplify_level=0.5))
#' bjmap3 <- st_as_sf(cnmap(3, "^110", simplify_level=0.5))
#' ggplot() + 
#'   geom_sf(data=bjmap3, color="gray50", size=0.5) +
#'   geom_sf(data=bjmap2, fill="transparent", color="gray5", size=0.8) +
#'   geom_sf_text(aes(label=NAME_LAB), data=bjmap3, family="Microsoft YaHei") +
#'   coord_sf(crs="+init=epsg:4490 +proj=laea +ellps=GRS80 
#'            +lon_0=105 +lat_0=30") +theme_minimal()
#' }
cnmap <- function(level=c("nation", "province", "city", "county"), regions=NULL, 
                  simplify_level=1, drop_fragment=FALSE, 
                  preserve_topo=c("all", "major", "none"),
                  fragment_area=0.003, output=c("spdf", "map", "df", "sf")){
    # check args
    if (is.character(level)){
        level <- match.arg(level)
    }else if (is.numeric(level)){
        stopifnot(level %in% 0:3)
        level <- c("nation", "province", "city", "county")[level+1]
    }
    preserve_topo <- match.arg(preserve_topo)
    output <- match.arg(output)
    stopifnot(is.numeric(simplify_level) && (simplify_level >=0 && simplify_level <= 1))
    mapobj <- switch(level, nation=asesgeo::chnmap0, province=asesgeo::chnmap1, 
                     city=asesgeo::chnmap2, county=asesgeo::chnmap3)
    meta <- mapobj@data
    
    # filter the map
    if (! is.null(regions)){
        match_region <- function(fld=c("NAME", "NAME_LAB", "NAME_EN", "ADCODE")){
            fld <- match.arg(fld)
            if (! fld %in% names(mapobj@data)){
                o <- rep(NA, nrow(mapobj@data))
            }else{
                o <- vapply(tolower(regions), grepl, x=tolower(mapobj[[fld]]), 
                            FUN.VALUE=logical(nrow(mapobj@data))) %>% 
                    apply(1, any, na.rm=TRUE)
            }
            return(o)
        }
        matched <- vapply(c("NAME", "NAME_LAB", "NAME_EN", "ADCODE"), match_region,
                          FUN.VALUE=logical(nrow(mapobj@data))) %>% 
            apply(1, any, na.rm=TRUE)
        mapobj <- mapobj[matched, ]
    }
    
    # simplify map obj
    if (simplify_level < 1){
        mapobj <- rmapshaper::ms_simplify(
            sf::st_as_sf(mapobj), keep=simplify_level, 
            keep_shapes=preserve_topo %in% c("all", "major"),
            explode=preserve_topo == "all")
        mapobj <- sf::as_Spatial(rmapshaper::ms_dissolve(
            mapobj, field="ADCODE", sum_fields=c("AREA", "PERIMETER"), 
            copy_fields=names(mapobj)[
                ! names(mapobj) %in% c("ADCODE", "AREA", "PERIMETER")]))
    }
    
    # drop fragments
    if (drop_fragment){
        mapobj <- drop_map_fragment(
            mapobj, level=which(c("nation", "province", "city", "county") == level) - 1,
            thres=list(fld="area", fun=`>=`, val=fragment_area))
    }
    
    return(switch(output,
                  spdf = mapobj,
                  map = maps::SpatialPolygons2map(mapobj, namefield="ADCODE"),
                  df = ggplot2::fortify(mapobj),
                  sf = st_as_sf(mapobj)))
}

#' @export
#' @rdname cnmap
cnmap0 <- function(regions=NULL, simplify_level=1, drop_fragment=FALSE, 
                   preserve_topo=c("all", "major", "none"), fragment_area=0.003,
                   output=c("spdf", "map", "df", "sf")){
    cnmap(0, regions=regions, simplify_level=simplify_level, preserve_topo=preserve_topo,
          drop_fragment=drop_fragment, fragment_area=fragment_area, output=output)
}

#' @export
#' @rdname cnmap
cnmap1 <- function(regions=NULL, simplify_level=1, drop_fragment=FALSE, 
                   preserve_topo=c("all", "major", "none"), fragment_area=0.003,
                   output=c("spdf", "map", "df", "sf")){
    cnmap(1, regions=regions, simplify_level=simplify_level, preserve_topo=preserve_topo,
          drop_fragment=drop_fragment, fragment_area=fragment_area, output=output)
}

#' @export
#' @rdname cnmap
cnmap2 <- function(regions=NULL, simplify_level=1, drop_fragment=FALSE, 
                   preserve_topo=c("all", "major", "none"), fragment_area=0.003,
                   output=c("spdf", "map", "df", "sf")){
    cnmap(2, regions=regions, simplify_level=simplify_level, preserve_topo=preserve_topo,
          drop_fragment=drop_fragment, fragment_area=fragment_area, output=output)
}

#' @export
#' @rdname cnmap
cnmap3 <- function(regions=NULL, simplify_level=1, drop_fragment=FALSE, 
                   preserve_topo=c("all", "major", "none"), fragment_area=0.003,
                   output=c("spdf", "map", "df", "sf")){
    cnmap(3, regions=regions, simplify_level=simplify_level, preserve_topo=preserve_topo,
          drop_fragment=drop_fragment, fragment_area=fragment_area, output=output)
}

drop_map_fragment <- function(
    mapobj, level=0, thres=list(fld="area", fun=`>=`, val=0.003)){
    # general caller to drop islands in the map object
    # Args:
    #   mapobj: a sp::SpatialPolygonsDataFrame object
    #   level: 0-3
    #   thres: a list assigning slot field, function, threshold value
    # Return:
    #   a modified sp::SpatialPolygonsDataFrame object
    
    stopifnot(inherits(mapobj, "SpatialPolygonsDataFrame"))
    stopifnot(is.numeric(level) && (level >= 0 && level <= 3))
    filters <- list(
        `0`=unique(chnmap0@data$ADCODE),
        `1`=c("350000", "440000", "450000", "460000", "130000", "320000", "210000",
              "370000", "310000", "710000", "120000", "330000"),
        `2`=c("130200", "210200", "210600", "210800", "211400", "320600", "320700", 
              "330200", "330300", "331000", "350100", "350200", "350300", "350500", 
              "350600", "350900", "370200", "370500", "370600", "371000", "440500",
              "440700", "440800", "440900", "441300", "441500", "441700", "445100",
              "450500", "450600", "450700", "460100", "460200", "460300", "469002",
              "469005", "469006", "710001", "710002", "710003", "710005", "710021",
              "710022", "710023", "710024", "710025", "710027", "710028", "710030",
              "710032", "710033", "710035", "710036", "710102", "810000"),
        `3`=c("130207", "130224", "130225", "210211", "210212", "210213", "210224",
              "210282", "210283", "210604", "210624", "210681", "210711", "210802",
              "211481", "320623", "320703", "330206", "330225", "330226", "330283",
              "330305", "330326", "330327", "330381", "330382", "331002", "331004",
              " 331021", "331022", "331081", "331082", "350105", "350122", "350128",
              "350181", "350182", "350203", "350213", "350305", "350505", "350521",
              "350527", "350622", "350623", "350624", "350626", "350681", "350902",
              "350921", "350981", "350982", "370211", "370212", "370282", "370521",
              "370522", "370602", "370612", "370634", "370681", "370683", "371002",
              "371082", "371083", "440523", "440781", "440803", "440804", "440811",
              "440825", "440881", "440882", "440904", "441303", "441323", "441502",
              "441521", "441581", "441702", "441721", "445122", "450502", "450521",
              "450602", "450603", "450702", "460105", "460108", "460202", "460203",
              "460204", "460300", "469002", "469005", "469006", "710001", "710024",
              "710027", "710028", "710030", "710032", "710033", "710035", "710036", 
              "710202", "810000")
    )
    
    # drop fragment within each polygon
    idx <- which(mapobj@data$ADCODE %in% filters[[as.character(level)]])
    if (length(idx) == 0) return(mapobj)
    o <- mapobj@polygons[idx]
    o <- lapply(o, function(pg){
        larger_than_thres <- vapply(
            pg@Polygons, function(lst) thres$fun(slot(lst, thres$fld), thres$val), 
            FUN.VALUE=logical(1L))
        if (any(larger_than_thres)){
            pg@plotOrder <- as.integer(rank(pg@plotOrder[larger_than_thres]))
            pg@Polygons <- pg@Polygons[larger_than_thres]
        }else{
            pg@plotOrder <- 1L
            pg@Polygons <- pg@Polygons[which.max(vapply(
                pg@Polygons, function(lst) slot(lst, thres$fld),
                FUN.VALUE=numeric(1)))]
        }
        return(pg)
    })
    mapobj@polygons[idx] <- o
    
    # drop polygons whose total area is below thres
    indicator <- unlist(get_sp_elem(mapobj, thres$fld, "polygons"))
    names(indicator) <- mapobj@data$ADCODE
    ind_dup <- duplicated(mapobj@data$ADCODE)
    mapobj <- mapobj[! ind_dup | (ind_dup & thres$fun(indicator, thres$val)), ]
    
    return(mapobj)
}


get_sp_elem <- function(x, ...){
    UseMethod("get_sp_elem", x)
}

get_sp_elem.SpatialPolygonsDataFrame <- function(
    x, attr_name, slot=c("data", "polygons", "plotOrder", "bbox", "proj4string"), 
...){
    slot <- match.arg(slot)
    stopifnot(is.character(attr_name))
    if (slot == "data"){
        o <- x@data[, attr_name]
    }else if (slot == "polygons"){
        if (length(attr_name) > 1){
            attr_name <- attr_name[[1]]
            warning("Too many attr assigned. Only accept the first element.")
        }
        attr_name <- match.arg(attr_name, c("Polygons", "plotOrder", "labpt", "ID", "area"))
        o <- lapply(x@polygons, function(lst) slot(lst, attr_name))
    }else{
        o <- slot(x, slot)
    }
    return(o)
}

get_sp_elem.default <- get_sp_elem.SpatialPolygonsDataFrame
