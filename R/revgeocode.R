#' Reverse geocode
#'
#' Reverse geocodes a lat/lng location using Google or Baidu Maps API.  Note that in most cases by
#' using this function you are agreeing to the Google Maps API Terms of Service
#' at \url{https://cloud.google.com/maps-platform/terms/}, the Baidu Maps API Terms
#' of Use at \url{http://lbsyun.baidu.com/index.php?title=open/question} or the 
#' Gaode Maps API Terms of Use at \url{https://lbs.amap.com/faq/top/notice}. \cr
#' Note that geocoding service may cause charges. Take care of our app key and 
#' check the quota carefully.
#'
#' @param latlng a location in latitude/longitude format, either a vector, list, 
#' matrix or a data.frame. The function will automatically identify the inputs 
#' and convert it to a data.frame [lat, lng] to proceed on. \describe{
#'  \item{vector: only extract the first two elements}{
#'     c(30, 100, ...) ==> data.frame(lat=30, lon=100)}
#'  \item{matrix: automatically identify the structure}{
#'     matrix(c(30, 20, 40, 100, 110, 90), ncol=2) ==> data.frame(lat=c(30, 20, 40), 
#'     lon=c(100, 110, 90))}
#'  \item{list: the lat and lon vectors are nested inside the list}{
#'    list(c(30, 20, 40), c(100, 110, 90)) ==> data.frame(lat=c(30, 20, 40), 
#'    lon=c(100, 110, 90))}
#'  \item{data.frame: only extract the first two columns}{
#'    data.frame(v1=c(30, 20), v2=c(100, 110), v3=...)  ==> data.frame(lat=c(30, 20), 
#'    lon=c(100, 110))}
#' }
#' @param ics the coordinate system of inputing location, including 'WGS-84', 'GCJ-02'
#' and 'BD-09', which are the GCSs of Google Earth, Google Map in China and Baidu
#' Map, respectively. For location out of China, ics is automatically set to 'WGS-84'
#' and other values are ignored.
#' @param output character, either 'address', 'addressc', 'all' or 'raw'. 
#' Default 'address'. \itemize{
#'   \item 'address': formatted address \cr
#'   \item 'addressc': formmatted address with address components \cr
#'   \item 'all': all information \cr
#'   \item 'raw': return the raw data parsed from JSON by \code{\link[jsonlite]{fromJSON}()}.
#'  You will then need to parse the data on your own.
#' }
#' @param api use 'google', 'baidu' or 'gaode' maps api
#' @param messaging turn messaging on/off. The default value is FALSE.
#' @param time the time interval to revgeocode, in seconds. Default value is zero.
#' When you revgeocode multiple locations, set a proper time interval to avoid
#' exceeding usage limits. For details see
#' \url{https://developers.google.com/maps/premium/usage-limits}
#' @param use_curl logical, whether use \code{curl} (TRUE) or \code{url} (FALSE)
#' to create the connection when calling the APIs. Default TRUE. The avialability 
#' of \code{curl} dependes on your network conditions.
#' @param key an api key must be provided when calling the Maps APIs. 
#' Default NULL, which indicates that the function will search for cache. If no
#' match is found, a GUI wizard will be launched to enter the api key. If the API
#' does not call for a key, set it to NA.
#' @param auto_fix_latlng logical, if the latlng data is of the opposite order,
#' whether to let the function automatically fix it. Default TRUE.
#' @param ... other arguments to pass to the function, dependent on \code{api}. 
#' \describe{
#'  \item{\code{api} == 'google'}{\itemize{
#'   \item \code{client} and \code{signature} instead of \code{key} for higher security 
#'     (if you have a premium account) \cr
#'   \item \code{language} the language in which to return results, the supported
#'    languages refers to \url{https://developers.google.com/maps/faq#languagesupport} \cr
#'   \item \code{result_type} a filter of one or moree address types (separated by
#'    a pipe '|'), e.g., 'country|political'. \cr
#'   \item \code{location_type} a filter of one or moree address types (separated by
#'    a pipe '|'), e.g., 'ROOFTOP|APPROXIMATE'. \cr
#'   \item \code{name_type}: either 'long' or 'short' indicating long_name or 
#'     short_name is returned. Default 'long'.
#'  }}
#'  \item{\code{api} == 'baidu'}{\itemize{
#'   \item \code{pois}: 1 or 0. whether callback surrounding pois (1=Yes, 0=No). 
#'    You will need to apply for special service access when calling pois outside 
#'    China. \cr
#'   \item \code{radius}: the radius for calling back pois. 0-1000 (m). \cr
#'   \item \code{extensions_road}: callback 3 streets near the spot when set to 
#'    \code{TRUE}. \cr
#'   \item \code{extensions_town}: callback info of town when set to \code{TRUE}. \cr
#'   \item \code{language}: 'local' or short-form of languages, e.g., 'en', 'zh-CN'.
#'    You will need to apply for special service access when using multi-language
#'    functionality. \cr
#'   \item \code{langauge_auto}: whether auto fill the admin area (1=Yes, 0=No).
#'    You will need to apply for special service access when using multi-language
#'    functionality. \cr
#'   \item \code{latest_admin}: whether get the latest administrative area info 
#'    (1=Yes, 0=No).
#'  }}
#'  \item{\code{api} == 'gaode'}{\itemize{
#'   \item \code{extensions}: return basic info ('base') or base+additional info ('all'). 
#'    When \code{extensions='all'}, some more parameters will take effect: 
#'    \itemize{
#'     \item \code{poitype}: one or more POI type codes (separated by a pipe '|').
#'      The valid POI type codes refers to \url{https://lbs.amap.com/api/webservice/download}. 
#'      It will not take effect when applying batch mode (multiple \code{latlng}s are provided). \cr
#'     \item \code{radius}: the radius for calling back pois. 0-3000 (m). \cr
#'     \item \code{roadlevel}: return all the roads (0) or main roads only (1) \cr
#'     \item \code{homeorcorp}: order the callback pois, \itemize{
#'       \item 0: do not intervene the order of the pois. \cr
#'       \item 1: home-related pois are prioritized. \cr
#'       \item 2: coorporate-related pois are prioritized.
#'      }
#'   }
#'  }}
#' }
#' 
#' @return a data.frame with variables address or detail address components
#' @author \itemize{
#'  \item Create: Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), PhD student from
#'    Center for Earth System Science, Tsinghua University \cr
#'  \item Update: Yiying Wang (\email{wangy@@aetna.com})
#' }
#' @seealso \code{\link{geocode}()}, \code{\link{set_api_key}()}, \code{\link{geohost}()}. \cr
#'  Refer to \code{\link{transform_coord}} function family to read the details about
#'  argument \code{y}, which is consistent with \code{latlng}. \cr
#'  \code{\link{synthesize_googlemap_api}()}, \code{\link{synthesize_baidumap_api}()},
#'  \code{\link{synthesize_gaodemap_api}()}
#' @references \itemize{
#'  \item Google Maps API at 
#'    \url{https://developers.google.com/maps/documentation/geocoding/intro#ReverseGeocoding} \cr
#'  \item Baidu Maps API at 
#'    \url{http://lbsyun.baidu.com/index.php?title=webapi/guide/webservice-geocoding} \cr
#'  \item Gaode Map API at 
#'    \url{https://lbs.amap.com/api/webservice/guide/api/georegeo}
#' }
#' @export
#' @examples
#' \dontrun{
#' set_api_key(c("googlemap", "baidumap", "gaodemap"), 
#'             c(<GOOGLE MAPS API KEY>, <BAIDU MAPS API KEY>,
#'               <GAODE MAPS API KEY>))
#' 
#' # reverse geocode Beijing railway station
#' revgeocode(c(39.90105, 116.42079), ics = 'WGS-84', output = 'address', 
#'            api = 'google')
#' revgeocode(c(39.90245, 116.42703), ics = 'GCJ-02', output = 'address', 
#'            api = 'google', messaging = TRUE)
#' revgeocode(c(39.90851, 116.43351), ics = 'BD-09', output = 'addressc', 
#'            api = 'google',)
#' revgeocode(c(39.90851, 116.43351), ics = 'BD-09', output = 'address',
#'            api = 'baidu')
#' revgeocode(c(39.90245, 116.42703), ics = 'GCJ-02', output = 'address', 
#'            api = 'baidu',messaging = TRUE)
#' revgeocode(c(39.90105, 116.42079), ics = 'WGS-84', output = 'addressc',
#'            api = 'baidu')
#'
#' # reverse geocode multiple locations
#' latlng = data.frame(lat = c(39.99837, 39.98565), 
#'                     lng = c(116.3203, 116.2998))
#' revgeocode(latlng, ics = 'WGS-84', output = 'all', api = 'google')
#' revgeocode(latlng, ics = 'WGS-84', output = 'all', api = 'google', 
#'            time = 2)
#' }
revgeocode <- function(latlng, 
                       ics = c('WGS-84', 'GCJ-02', 'BD-09'),
                       output = c('address', 'addressc', 'all', 'raw'), 
                       api = c('google', 'baidu', 'gaode'), 
                       messaging = FALSE, time = 0, use_curl=TRUE, 
                       key = NULL, auto_fix_latlng=TRUE, ...){
    # updated 2019-1 
    
    # -------check parameters-----
    api <- match.arg(api)
    output <- match.arg(output)
    ics <- match.arg(ics)
    stopifnot(is.logical(messaging))
    stopifnot(is.numeric(time))
    
    # --------format latlng--------
    latlng <- getCoordArgs(latlng, auto_fix=auto_fix_latlng)
    
    # ------dict fun-----------
    fun_dict <- list(google=revgeocode_google_api, baidu=revgeocode_baidu_api,
                     gaode=revgeocode_gaode_api)
    fun <- fun_dict[[api]]
    
    fun(latlng, ics=ics, output=output, messaging=messaging, 
        time=time, key=key, use_curl=use_curl, ...)
}

# ==================Parse Rev-Geocoding Rsults============

#' Parse revgeocode API results
#' 
#' This is a wrapper of API result parsers for google, baidu and gaode map. 
#' It can serve as a subsequent handler to convert 'raw' results yielded
#' using \code{\link{revgeocode}()} to a structured data.frame.
#' 
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param rgclst a list comprising of lists of class 'google_revgeocode', 
#' 'baidu_revgeocode' or 'gaode_revgeocode', subclasses of 'api_data'.
#' @param output character, 'address', 'addressc', or 'all'. Default 
#' 'address'. Refer to \code{\link{revgeocode}()}.
#' @param gcs character, the geo-coordinate system for output. 'WGS-84', 'GCJ-02', 
#'  or 'BD-09'. Default NULL, which indicates that no coordinate conversion will be 
#'  conducted on the results.
#' @param idf logical, whether generate an identifier column 'idf' in the output. 
#'  Default TRUE.
#' @param ... optional arguments. Generic args: \itemize{
#'  \item ics_china: default coordinate system for locations in China, 'WGS-84', 
#'   'GCJ-02', or 'BD-09' \cr
#'  \item ics_intl: default coordinate system for locations outside China, 'WGS-84', 
#'   'GCJ-02', or 'BD-09'  
#' }
#' API-specific args: \describe{
#'  \item{google api}{\itemize{
#'   \item name_type: character 'long' or 'short', indicating whether to extract
#'    long_name or short_name. Default 'long'.
#'  }}
#'  \item{baidu api}{\itemize{
#'   \item (not used yet)
#'  }}
#'  \item{gaode api}{\itemize{
#'   \item (not used yet)
#'  }}
#' }
#'  
#' @return a \code{\link[tibble]{tibble}}
#' 
#' @export
#' @aliases parse_revgeocode
#' 
#' @importFrom aseskit iif ifna
#' @importFrom dplyr bind_rows mutate if_else as_tibble
#' @seealso \code{\link{parse_api_data}()}, \code{\link{revgeocode}()},
#' \code{\link{geocode}()}, \code{\link{parse_geocodes}()};
#' \code{\link{geohost}()}, \code{\link{parse_geohosts}()};
#' @examples
#' \dontrun{
#' rgc <- revgeocode(c(<lat1>, <lon1>), api='google', output='raw')
#' parse_revgeocodes(rgc, output='all')
#' }
parse_revgeocodes <- function(
    rgclst, output=c('address', 'addressc', 'all'), gcs=NULL, idf=TRUE, 
    ...){
    # loop over gclst using parse_geocode_result
    # the lists in gclst must be of 'api_data' class
    
    # ----check args-----
    dots <- list(...)
    if (! is.null(gcs)) 
        gcs <- match.arg(gcs, c('WGS-84', 'GCJ-02', 'BD-09'))
    stopifnot(is.logical(idf))
    output <- match.arg(output)
    name_out <- names(rgclst)  # names of rgclist
    
    rgcdf <- mapply(parse_revgeocode_result, rgc=rgclst, name_out=name_out,
                    MoreArgs=list(output=output, gcs=gcs, ...),
                    SIMPLIFY=FALSE) %>% 
        bind_rows %>% as_tibble
    
    # if (output == 'all')
    #     rgcdf <- rgcdf %>% 
    #     mutate(street_no=tryCatch(as.integer(street_no), error=function(e) NA_integer_), 
    #            postal_code=tryCatch(as.integer(postal_code), error=function(e) NA_integer_))
    invalid_latlng <- aseskit::ifna(abs(rgcdf$lat) > 90 & abs(rgcdf$lng) > 180, FALSE)
    if (any(invalid_latlng)) rgcdf[invalid_latlng, ] <- NA
    
    which_addrc_rm <- which(names(rgcdf) %in% c(
        if (idf) NULL else 'idf', 'lat', 'lng', 'loctype'))
    
    out_dict <- list(
        address = c(dplyr::if_else(idf, 'idf', NULL), 'address'),
        addressc = - which_addrc_rm,
        all = if (idf) names(rgcdf) else names(rgcdf)[names(rgcdf) != 'idf']
    )
    return(rgcdf[out_dict[[output]]])
}

parse_revgeocode_result <- function(rgc, ...){
    ## convert a single list 'rgc' to a data.frame
    stopifnot(inherits(rgc, 'api_data'))
    UseMethod(".parse_revgeocode_result", rgc)
}

# google: geocode = revgeocode
#' @export
.parse_revgeocode_result.google_revgeocode <- function(
    rgc, output=c('latlng', 'latlngc', 'latlnga', 'all'), gcs=NULL,
    ics_china='GCJ-02', ics_intl='WGS-84', name_type=c('long', 'short'),
    name_out=NULL, idf=TRUE,
...){
    .parse_geocode_result.google_geocode(
        gc=rgc, output=output, gcs=gcs, ics_china=ics_china, ics_intl=ics_intl,
        name_type=name_type, name_out=name_out, idf=idf, ...)
}

#' @export
#' @importFrom aseskit iif ifnull ifempty
#' @importFrom dplyr bind_rows bind_cols
.parse_revgeocode_result.baidu_revgeocode <- function(
    rgc, output=c('address', 'addressc', 'all'), gcs=NULL,
    ics_china='GCJ-02', ics_intl='WGS-84', 
    name_out=NULL, idf=TRUE,
    ...){
    
    # check args
    dots <- list(...)
    output <- match.arg(output)
    if (! is.null(gcs)) 
        gcs <- match.arg(gcs, c('WGS-84', 'GCJ-02', 'BD-09'))
    
    if (!missing(ics_china)) ics_china <- match.arg(ics_china, c('BD-09', 'WGS-84', 'GCJ-02'))
    if (!missing(ics_intl)) ics_intl <- match.arg(ics_intl, c('BD-09', 'WGS-84', 'GCJ-02'))
    name_out <- aseskit::ifnull(name_out, attr(gc, 'name_out'))
    
    # did geocode fail?
    if (length(rgc) == 0 || as.numeric(rgc$status) != 0) {
        rgcdf <- data.frame(address=NA_character_, lat=NA_real_, lng=NA_real_)
        if (length(rgc) > 0) if (as.numeric(rgc$status) != 0)
            warning(paste0('revgeocode failed with status code ', rgc$status, ' (',
                           rgc$msg, ') , location = "', name_out, '". see more ',
                           'details in the response code table of Baidu Geocoding API'), 
                    call. = FALSE)
    }else{
        loc <- aseskit::ifempty(rgc$result$location, list(lat=NA_real_, lng=NA_real_))
        adr <- aseskit::ifempty(rgc$result$formatted_address, NA_character_)
        rgcdf <- data.frame(address=adr, lat=loc['lat'], lng=loc['lng'],
                            stringsAsFactors=FALSE)
        if (output == 'all'){
            biz <- aseskit::ifempty(rgc$result$business, NA_character_)
            adc <- aseskit::ifempty(rgc$result$addressComponent, list(
                country=NA_character_, province=NA_character_, city=NA_character_,
                city_level=NA_integer_, district=NA_character_, town=NA_character_,
                street=NA_character_, street_no=NA_character_, direction=NA_character_,
                distance=NA_character_))
            poi <- rgc$result$pois
            prg <- rgc$result$poiRegions
            dsc <- rgc$result$semantic_description
            
            attrdf <- data.frame(
                business = biz, country = adc[['country']], province = adc[['province']],
                city = adc[['city']], city_level = as.integer(adc[['city_level']]),
                district = adc[['district']], town = adc[['town']], street = adc[['street']],
                street_no = adc[['street_number']], 
                adcode = as.integer(adc[['adcode']]), direction = adc[['direction']],
                distance = adc[['distance']], stringsAsFactors = FALSE)
            if (length(poi) == 0){
                poidf <- data.frame(
                    poi_addr = NA_character_, poi_name = NA_character_,
                    poi_type = NA_character_, poi_lat = NA_real_,
                    poi_lng = NA_real_)
                poidf <- list(poidf, poidf, poidf) %>% bind_rows
            }else{
                poidf <- data.frame(
                    poi_addr = aseskit::ifempty(poi[['addr']], NA_character_), 
                    poi_name = aseskit::ifempty(poi[['name']], NA_character_), 
                    poi_type = aseskit::ifempty(poi[['poiType']], NA_character_), 
                    poi_lat = aseskit::ifempty(poi[['point']][['y']], NA_real_),
                    poi_lng = aseskit::ifempty(poi[['point']][['x']], NA_real_), 
                    stringsAsFactors = FALSE)
            }
            poidf <- aseskit:::flatten_df(poidf, output='data.frame')
            
            if (length(prg) == 0){
                prgdf <- data.frame(reg_name = NA_character_, 
                                    reg_tag = NA_character_,
                                    description = NA_character_)
            }else{
                prgdf <- data.frame(
                    reg_name = aseskit::ifempty(prg[['name']], NA_character_), 
                    reg_tag = aseskit::ifempty(prg[['tag']], NA_character_), 
                    description = aseskit::ifempty(dsc, NA_character_), 
                    stringsAsFactors=FALSE)
            }
            
            rgcdf <- list(rgcdf, attrdf, poidf, prgdf) %>% bind_cols
        }    
    }
    
    out <- if (is.null(gcs)) rgcdf else 
        format_gcdf(rgcdf, ics_china=ics_china, ics_intl=ics_intl, ocs=gcs)
    if (idf) out <- data.frame(idf=name_out, out)
    return(out)
}

#' @export
#' @importFrom aseskit iif ifnull ifempty
#' @importFrom dplyr bind_rows mutate select
#' @importFrom tidyr separate
.parse_revgeocode_result.gaode_revgeocode <- function(
    rgc, output=c('address', 'addressc', 'all'), gcs=NULL,
    ics_china='GCJ-02', ics_intl='WGS-84', 
    name_out=NULL, idf=TRUE,
...){
    # check args
    dots <- list(...)
    output <- match.arg(output)
    if (! is.null(gcs)) 
        gcs <- match.arg(gcs, c('WGS-84', 'GCJ-02', 'BD-09'))
    
    if (!missing(ics_china)) 
        ics_china <- match.arg(ics_china, c('WGS-84', 'GCJ-02', 'BD-09'))
    if (!missing(ics_intl)) 
        ics_intl <- match.arg(ics_intl, c('WGS-84', 'GCJ-02', 'BD-09'))
    name_out <- aseskit::ifnull(name_out, attr(gc, 'name_out'))
    
    # did revgeocode fail?
    if (length(rgc) == 0 || as.numeric(rgc$status) != 1) {
        rgcdf <- data.frame(address=NA_character_, lat=NA_real_, lng=NA_real_)
        if (length(rgc) > 0) if (as.numeric(rgc$status) != 1)
            warning(paste0('revgeocode failed with status code ', rgc$status, ' (',
                           rgc$info, '), location = "', name_out, '". see more ',
                           'details in the response code table of Gaode Geocoding API'),
                call. = FALSE)
    }else{
        rgc <- if ('revgeocodes' %in% names(rgc)) rgc$regeocodes else rgc$regeocode
        
        # parse the nested structure of rgc
        # address
        rgcdf <- list(address=unlist(ifempty(rgc$formatted_address, NA_character_)))
        n_out <- length(rgc$formatted_address)

        rgcdf$lat <- rgcdf$lng <- rep(NA_real_, n_out)
        
        if (output == 'all'){
            # address component
            rgcdf <- append(rgcdf, rgc$addressComponent[
                - which(names(rgc$addressComponent) %in% c(
                    'streetNumber', 'businessAreas', 'building', 'neighborhood'))])
            rgcdf[c('street_no', 'street_direction', 'street_distance',
                    'street')] <- rgc$addressComponent$streetNumber[c(
                        'number', 'direction', 'distance','street')]
            if ('location' %in% names(rgc$addressComponent$streetNumber)){
                rgcdf$location <- rgc$addressComponent$streetNumber$location
            }else{
                rgcdf$location <- rgc$addressComponent$streetNumber$number
            }
            rgcdf$location <- aseskit::ifempty(rgcdf$location, ',')
            rgcdf[c('nbrhd_name', 'nbrhd_type')] <- with(
                rgc$addressComponent$neighborhood, list(name, type))
            rgcdf[c('blding_name', 'blding_type')] <- with(
                rgc$addressComponent$building, list(name, type))
            bizAreas <- rgc$addressComponent$businessAreas
            if (is.data.frame(bizAreas)){
                rgcdf[paste0('biz_loc', 1:3)] <- bizAreas$location[1:3]
                rgcdf[paste0('biz_name', 1:3)] <- bizAreas$name[1:3]
            }else{
                rgcdf[paste0('biz_loc', 1:3)] <- lapply(1:3, function(i) {
                    vapply(bizAreas, function(bizArea) {
                        aseskit::ifnull(bizArea$location[i], ',')
                    }, FUN.VALUE=character(1L))})
                rgcdf[paste0('biz_name', 1:3)] <- lapply(1:3, function(i) {
                    vapply(bizAreas, function(bizArea) {
                        aseskit::ifnull(bizArea$name[i], NA_character_)
                    }, FUN.VALUE=character(1L))})
            }
            rgcdf[paste0('biz_lat', 1:3)] <- rep(list(rep(NA_real_, n_out)), 3)
            rgcdf[paste0('biz_lng', 1:3)] <- rep(list(rep(NA_real_, n_out)), 3)
            
            # coerce nested list() to NAs
            nms <- names(rgcdf)
            rgcdf <- aseskit::ifempty(rgcdf, NA_character_)
            rgcdf_var_islist <- vapply(rgcdf, is.list, FUN.VALUE=logical(1L))
            if (any(rgcdf_var_islist))
                for (i in which(rgcdf_var_islist)) rgcdf[[i]] <- unlist(rgcdf[[i]])
            rgcdf <- as.data.frame(rgcdf, stringsAsFactors=FALSE)
            names(rgcdf) <- nms
            
            # coords
            rgcdf <- tidyr::separate(rgcdf, location, into=c('lng', 'lat'), sep=',',
                                     convert=TRUE, fill='left') %>% 
                tidyr::separate(biz_loc1, into=c('biz_lng1', 'biz_lat1'), sep=',',
                                convert=TRUE, fill='left') %>% 
                tidyr::separate(biz_loc2, into=c('biz_lng2', 'biz_lat2'), sep=',',
                                convert=TRUE, fill='left') %>% 
                tidyr::separate(biz_loc3, into=c('biz_lng3', 'biz_lat3'), sep=',',
                                convert=TRUE, fill='left') 
            
            rgcdf <- rgcdf %>% 
                mutate(adcode=as.integer(adcode), 
                       street_distance=as.numeric(street_distance))
        }
    }
    if (! is.data.frame(rgcdf))
        rgcdf <- as.data.frame(rgcdf, stringsAsFactors=FALSE)
    # convert coordinates
    out <- if (is.null(gcs)) rgcdf else 
        format_gcdf(rgcdf, ics_china=ics_china, ics_intl=ics_intl, ocs=gcs)
    
    # gaode batch mode gets 'latlng1|latlng2|latlng3|...', str_split it and name the result
    if (idf) out <- data.frame(
        idf=as.factor(name_out) %>% levels %>% strsplit('\\|') %>% unlist, out)
    return(out)
}


# ==================Specific RevGeocoding APIs==========================
#' @importFrom aseskit get_api_data getApiKey iif ifnull
revgeocode_google_api <- function(
    latlng, output = c('address', 'addressc', 'all', 'raw'), 
    ics = c('WGS-84', 'GCJ-02', 'BD-09'),
    messaging = FALSE, time = 0, key = NULL, 
    use_curl=TRUE, idf=TRUE, client=NULL, signature=NULL, 
    name_type=c('long', 'short'), language=NULL, location_type=NULL,
    result_type=NULL,
...){
    # 1 synthesizes urls and retrieve json lists and then, 2 parse the lists
    
    # -----check args-----
    dots <- list(...)
    output <- match.arg(output)
    ics <- match.arg(ics)
    stopifnot(is.logical(use_curl))
    
    # different google maps api is used based on user's location. If user is 
    # inside China, ditu.google.cn is used; otherwise maps.google.com is used.
    ip.country()  # check ip country and store the result in options
    
    ## authorization parameters
    client <- aseskit::ifnull(client, '')
    signature <- aseskit::ifnull(signature, '')
    if (nchar(client) > 0 && nchar(signature) > 0){
        key <- NULL
    }else if (is.null(key)){
        key <- aseskit::getApiKey("googlemap")
    }else if (! is.character(key) || key == ""){
        stop("Please use either a valid client + signature pair (premium account), ",
             "or a google maps API key.")
    }
    name_type <- aseskit::ifnull(name_type, 'long')
    name_type <- match.arg(name_type, c('long', 'short'))

    ## latlng vector
    if (nrow(latlng) > 1) {
        if (messaging) invisible(message(paste(
            'Each request costs around USD $0.01.\n', 
            'Google Cloud Platform limits 50 requests/second at most.')))
        if (nrow(latlng) > 50 & messaging) 
            message("You passed in 50+ coordinates. Mind your quota and costs.")
    }
    latlng <- paste(latlng$lat, latlng$lon, sep=",")  # paste lat, lon
    
    # -----synthesize urls-----
    urls <- aseskit::synthesize_api(
        url_body=latlng, provider='googlemap', api='revgeocode', key=key, 
        name_type=name_type, client=client, signature=signature, use_curl=use_curl, 
        language=language, location_type=location_type, result_type=result_type, ...)
    
    # -----geocode------
    rgclst <- aseskit::get_api_data(urls, use_curl=use_curl, time=time, 
                                    messaging=messaging, name_out=latlng)
    
    # ----- output -----
    if (output == 'raw') return(rgclst)
    return(parse_revgeocodes(
        rgclst, gcs=ics, idf=idf, output=output, name_type=name_type, 
        ics_china='GCJ-02',  ics_intl=if (getOption('ip.country') == 'CN') 
            'GCJ-02' else 'WGS-84'))
}

#' @importFrom aseskit get_api_data getApiKey iif ifnull
revgeocode_baidu_api <- function(
    latlng, output = c('address', 'addressc', 'all', 'raw'),
    ics=c('WGS-84', 'GCJ-02', 'BD-09'), messaging = FALSE, time = 0, 
    use_curl=FALSE, idf=TRUE, key=NULL, city=NULL,
...){
    
    # -----check args-------
    dots <- list(...)
    output <- match.arg(output)
    ics <- match.arg(ics)
    ocs <- if (ics == 'WGS-84') 'BD-09' else ics
    city <- ifnull(city, '')
    stopifnot(is.character(city))
    
    if (is.null(key)){
        key <- aseskit::getApiKey("baidumap")
    }else if (!is.character(key) || key == ""){
        stop("Please use a valid baidu map API key.")
    }
    latlng <- paste(latlng$lat, latlng$lon, sep=",")
    
    # # if outside China, ics must be WGS-84
    # coord_wgs <- conv_coord(latlng$lat, latlng$lon, from=ics, to='WGS-84')
    # outsideChina <- which(isOutOfChina(coord_wgs$lat, coord_wgs$lng))
    # ics_vec <- rep(ics, nrow(latlng))
    # if (length(outsideChina) > 0){
    #     ics_vec[outsideChina] <- 'WGS-84'
    #     message("A total of ", length(outsideChina), " points are outside China.",
    #             "\n`ics` has thus been replaced with 'WGS-84' for these points.")
    # }
    
    # -----synthesize urls-------
    urls <- aseskit::synthesize_api(
        url_body=latlng, provider='baidumap', api='revgeocode', city=city, key=key, 
        use_curl=use_curl,...)
    
    # -----geocode------
    rgclst <- aseskit::get_api_data(urls, use_curl=use_curl, time=time, 
                                    messaging=messaging, name_out=latlng)
    
    # ----- output -----
    if (output == 'raw') return(rgclst)
    return(parse_revgeocodes(
        rgclst, gcs=ocs, idf=idf, output=output, ics_china=ics, ics_intl=ics))
}

#' @importFrom aseskit get_api_data getApiKey
revgeocode_gaode_api <- function(
    latlng, output = c('address', 'addressc', 'all', 'raw'), 
    ics=c('WGS-84', 'GCJ-02', 'BD-09'), messaging = FALSE, time = 0, 
    use_curl=FALSE, idf=TRUE, key=NULL, sig=NULL, city=NULL, batch=TRUE,
    extensions=c('all', 'base'), poitype=NULL, radius=NULL, roadlevel=NULL, 
    homeorcorp=NULL,
...){
    # ----check args-----
    dots <- list(...)
    output <- match.arg(output)
    ics <- match.arg(ics)
    ocs <- if (ics == 'WGS-84') 'GCJ-02' else ics
    stopifnot(is.logical(batch))
    extensions <- match.arg(extensions)
    
    if (is.null(key)){
        key <- aseskit::getApiKey("gaodemap")
    }else if (!is.character(key) || key == ""){
        stop("Please use a valid gaode map API key.")
    }
    
    # -----concatenate latlng, 20 / batch-------
    latlng <- paste(sprintf("%.6f", latlng$lon), sprintf("%.6f", latlng$lat), 
                    sep=",")  # paste together, lng and lat
    if (length(latlng) > 1){
        split_by <- as.vector(mapply(
            rep, 1:ceiling(length(latlng)/20), 20))[seq_along(latlng)]
        latlng <- split(latlng, split_by)
        latlng <- vapply(latlng, paste, FUN.VALUE=character(1L), collapse="|")
    }else{
        batch <- FALSE
    }
    
    # -----synthesize urls-------
    urls <- aseskit::synthesize_api(
        url_body=latlng, provider='gaodemap', api='revgeocode', key=key, sig=sig, 
        use_curl=use_curl, batch=batch, extensions=extensions, poitype=poitype, 
        radius=radius, roadlevel=roadlevel, homeorcorp=homeorcorp, ...)
    
    # -----geocode------
    rgclst <- aseskit::get_api_data(urls, use_curl=use_curl, time=time, 
                                    messaging=messaging, name_out=latlng)
    
    # ----- output -----
    if (output == 'raw') return(rgclst)
    return(parse_revgeocodes(
        rgclst, gcs=ocs, idf=idf, output=output, ics_china='GCJ-02', ics_intl='WGS-84'))
}

 