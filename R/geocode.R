# ================Generic geocode function===========================

#' Geocode
#'
#' geocodes an address using Google, Baidu or Gaode Maps API. Note that in most cases by
#' using this function you are agreeing to the Google Maps API Terms of Service
#' at \url{https://cloud.google.com/maps-platform/terms/}, the Baidu Maps API Terms
#' of Use at \url{http://lbsyun.baidu.com/index.php?title=open/question} or the 
#' Gaode Maps API Terms of Use at \url{https://lbs.amap.com/faq/top/notice}. \cr
#' Note that geocoding service may cause charges. Take care of our app key and 
#' check the quota carefully.
#' 
#' @param address a character vector specifying the locations of interest (e.g.,
#' "Tsinghua Univeristy").
#' @param output character, either'latlng', 'latlngc', 'latlnga', 'all' or 'raw'. 
#' Default 'latlng'.
#' \itemize{
#' \item 'latlng': lat/lng coordinates with location type (Goolge Map) \cr
#' \item 'latlngc': lat/lng coordinates with location type (Goolge Map) | 
#'   confidence/precision (Baidu Map) \cr
#' \item 'latlnga': lat/lng coordinates with formated address and address 
#'  components (only available for Google and Gaode Map API) \cr
#' \item 'all': return all the information \cr
#' \item 'raw': return the raw data parsed from JSON by \code{\link[jsonlite]{fromJSON}()}.
#'  You will then need to parse the data on your own. Typically you can use
#'  \code{\link{parse_geocodes}()}
#' }
#' @param api character, the API to use ("google", "baidu" or "gaode"). Default 'google'.
#' When using Baidu or Gaode Maps API, the address must be in Chinese.
#' @param ocs output coordinate systems including 'WGS-84', 'GCJ-02' and 'BD-09', which
#' are the GCSs of Google Earth, Google Map in China and Baidu Map, respectively.
#' For address out of China, ocs is automatically set to 'WGS-84' and other values
#' are igored. Default 'WGS-84'.
#' @param messaging turn messaging on/off. Default FALSE.
#' @param time the time interval to geocode, in seconds. Default value is zero.
#' When you geocode multiple addresses, set a proper time interval to avoid
#' exceeding usage limits. For details see
#' \url{https://developers.google.com/maps/premium/usage-limits}
#' @param use_curl logical, whether use \code{curl} (TRUE) or \code{url} (FALSE)
#' to create the connection when calling the APIs. Default TRUE. The avialability 
#' of \code{curl} dependes on your network conditions.
#' @param idf logical, whether add an identifier column to the result . If
#' TRUE, \code{address} will be applied as an identifier column of the result 
#' data.frame. If FALSE, the identifier column will not be generated. You can also 
#' manually assign a vector as the identifier. Default TRUE.
#' @param key an api key must be provided when calling the Maps APIs. 
#' Default NULL, which indicates that the function will search for cache. If no
#' match is found, a GUI wizard will be launched to enter the api key. If the API
#' does not call for a key, set it to NA.
#' @param ... other arguments to pass to the function, dependent on \code{api}. 
#' \describe{
#'  \item{\code{api} == 'google'}{\itemize{
#'   \item \code{client} and \code{signature}: (instead of \code{key}) for higher 
#'     security (if you have a premium account) \cr
#'   \item \code{region}: a character vector to restrain the scope for a better match.
#'     It will be expanded to fit the length of \code{address}. You need to refer
#'     to the API guide. \cr
#'   \item \code{components}: a formatted character vector to restrain the scope
#'     for a better match. It will be expanded to fit the length of \code{address}. 
#'     You need to refer to the API guide. \cr
#'   \item \code{name_type}: either 'long' or 'short' indicating long_name or 
#'     short_name is returned. Default 'long'. \cr
#'   \item \code{language}: the language in which to return results. Default 
#'    NULL (the native language).
#'  }}
#'  \item{\code{api} == 'baidu'}{\itemize{
#'   \item \code{city}: a valid Chinese city name to limit the search range
#'  }}
#'  \item{\code{api} == 'gaode'}{\itemize{
#'   \item \code{city}: a valid city character to limit the search range, accepts
#'    4 form: Chinaese city name, full pinyin (e.g., beijing), citycode (e.g., 010)
#'    or adcode (110000).
#'  }}
#' }
#' @return a data.frame with variables lat/lng or more info
#' @author \itemize{
#'  \item Creat: Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), PhD student from
#'   Center for Earth System Science, Tsinghua University \cr
#'  \item Update: Yiying Wang (\email{wangy@@aetna.com})
#' }
#' @seealso \code{\link{revgeocode}()}, \code{\link{set_api_key}()}, \code{\link{geohost}()},
#'  \code{\link{parse_geocodes}()}
#' @references \itemize{
#'  \item Google Maps API guide: 
#'    \url{https://developers.google.com/maps/documentation/geocoding/start?csw=1} \cr
#'  \item Baidu Maps API guide: 
#'    \url{http://lbsyun.baidu.com/index.php?title=webapi/guide/webservice-geocoding} \cr
#'  \item Gaode (Amap) Map API guide: 
#'    \url{https://lbs.amap.com/api/webservice/guide/api/georegeo}
#' }
#' @export
#' @examples
#' \dontrun{
#' set_api_key(c("google", "baidu", "gaode"), 
#'             c(<GOOGLE MAPS API KEY>, <BAIDU MAPS API KEY>,
#'               <GAODE MAPS API KEY>))
#' 
#' geocode('Tsinghua University', api='google', ocs='GCJ-02')
#' geocode('Tsinghua University', api='google', ocs='WGS-84',
#'         messaging=TRUE)
#' geocode('Beijing railway station', output='latlngc', api='google', 
#'         ocs='WGS-84')
#' geocode('Beijing railway station', output='latlnga', api='google', 
#'         ocs='WGS-84')
#' geocode(c('Tsinghua University', 'Beijing railway station'), api='google',
#'         ocs='GCJ-02')
#' geocode(c('Tsinghua University', 'Beijing railway station'), 
#'         output='latlngc', api='google', ocs='WGS-84', messaging=TRUE)
#' geocode(c('Tsinghua University', 'Beijing railway station'), 
#'         output='latlnga', api='google', ocs='WGS-84', messaging=TRUE)
#' geocode(c('Tsinghua University', 'Beijing railway station'), 
#'         output='latlngc', api='google', ocs='WGS-84', messaging=TRUE, 
#'         time=2)
#' geocode('Beijing railway station', api='baidu', ocs='BD-09')
#' geocode('Beijing railway station', api='baidu', ocs='GCJ-02', 
#'         messaging=TRUE)
#' geocode('Beijing railway station', output='latlngc', api='baidu', 
#'         ocs='BD-09')
#' geocode(c('Tsinghua University', 'Beijing railway station'), api='baidu',
#'         ocs='BD-09')
#' geocode(c('Tsinghua University', 'Beijing railway station'), 
#'         output='latlngc', api='baidu', ocs='WGS-84')
#' }
geocode <- function(address, 
                    output = c('latlng', 'latlngc', 'latlnga', 'all', 'raw'), 
                    api = c('google', 'baidu', 'gaode'), 
                    ocs = c('WGS-84', 'GCJ-02', 'BD-09'),
                    messaging = FALSE, time = 0, use_curl=TRUE, 
                    idf = TRUE, key = NULL, ...){
    # updated 2019-1 
    
    # -----check args-----
    stopifnot(is.character(address))
    output <- match.arg(output)
    api <- match.arg(api)
    ocs <- match.arg(ocs)
    stopifnot(is.logical(messaging))
    stopifnot(is.numeric(time))
    # -----function dict-------
    fun_dict <- list(google=geocode_google_api, baidu=geocode_baidu_api,
                     gaode=geocode_gaode_api)
    fun <- fun_dict[[api]]
    # location encoding
    address <- enc2utf8(address)
    
    fun(address, output=output, ocs=ocs, messaging=messaging, 
        time=time, key=key, use_curl=use_curl, ...)
}

# ================Generic geocode data parser===================

#' Parse geocode API results
#' 
#' This is a wrapper of API result parsers for google, baidu and gaode map. 
#' It can serve as a subsequent handler to convert 'raw' results yielded
#' using \code{\link{geocode}()} to a structured data.frame.
#' 
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param gclst a list comprising of lists of class 'google_geocode', 'baidu_geocode'
#'  or 'gaode_geocode', subclasses of 'api_data'.
#' @param output character, 'latlng', 'latlngc', 'latlnga', or 'all'. Default 
#' 'latlng'. Refer to \code{\link{geocode}()}.
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
#' @aliases parse_geocode
#' 
#' @importFrom aseskit iif ifna
#' @importFrom dplyr bind_rows mutate if_else as_tibble
#' @seealso \code{\link{parse_api_data}()}, \code{\link{geocode}()},
#'  \code{\link{revgeocode}()}, \code{\link{parse_revgeocodes}()};
#'  \code{\link{geohost}()}, \code{\link{parse_geohoss}()}
#' @examples
#' \dontrun{
#' gc <- geocode(c('<addr1>', '<addr2>'), api='google', output='raw')
#' parse_geocodes(gc, output='all')
#' }
parse_geocodes <- function(
    gclst, output=c('latlng', 'latlngc', 'latlnga', 'all'), gcs=NULL, idf=TRUE, 
...){
    
    # loop over gclst using parse_geocode_result
    # the lists in gclst must be of 'api_data' class
    
    # ----check args, rely on parse_geocode_result()-----
    # output, gcs, idf will be checked in .parse_geocode_re
    dots <- list(...)
    if (! is.null(gcs)) 
        gcs <- match.arg(gcs, c('WGS-84', 'GCJ-02', 'BD-09'))
    stopifnot(is.logical(idf))
    name_out <- names(gclst)  # names of gclist
    output <- match.arg(output)
    
    gcdf <- mapply(parse_geocode_result, gc=gclst, name_out=name_out, 
                   MoreArgs=list(output=output, gcs=gcs, ...), 
                   SIMPLIFY=FALSE) %>% 
        bind_rows %>% as_tibble
    invalid_latlng <- ifna(abs(gcdf$lat) > 90 & abs(gcdf$lng) > 180, FALSE)
    if (any(invalid_latlng)) gcdf[invalid_latlng, ] <- NA
    
    out_dict <- list(
        latlng = c(if (idf) 'idf' else NULL, 'lat', 'lng'),
        latlngc = c(if (idf) 'idf' else NULL, 'lat', 'lng', 'loctype'),
        latlnga = c(if (idf) 'idf' else NULL, 'lat', 'lng', 'address'),
        all = if (idf) names(gcdf) else names(gcdf)[names(gcdf) != 'idf']
    )
    return(gcdf[out_dict[[output]]])
}

# the working function to process one 'api_data' list

parse_geocode_result <- function(gc, ...){
    ## convert a single list 'gc' to a data.frame
    stopifnot(inherits(gc, 'api_data'))
    UseMethod(".parse_geocode_result", gc)
}

#' @importFrom aseskit iif ifnull
#' @importFrom dplyr bind_rows filter
#' @importFrom tidyr unnest
.parse_geocode_result.google_geocode <- function(
    gc, output=c('latlng', 'latlngc', 'latlnga', 'all'), gcs=NULL,
    ics_china='GCJ-02', ics_intl='WGS-84', name_type=c('long', 'short'),
    name_out=NULL, idf=TRUE,
...){
    
    # check args
    dots <- list(...)
    output <- match.arg(output)
    if (! is.null(gcs)) 
        gcs <- match.arg(gcs, c('WGS-84', 'GCJ-02', 'BD-09'))
    
    if (!missing(ics_china)) ics_china <- match.arg(ics_china, c('WGS-84', 'GCJ-02', 'BD-09'))
    if (!missing(ics_intl)) ics_intl <- match.arg(ics_intl, c('WGS-84', 'GCJ-02', 'BD-09'))
    name_type <- match.arg(name_type)
    name_out <- ifnull(name_out, attr(gc, 'name_out'))
    
    ## valid attributes
    valid_attrs <- structure(
        c("poi", "room", "floor", "subpremise", "premise", "nbrhd", "street_no", 
          "street_addr", "intersection", "route", 
          paste0("subloc_l", 5:1), "locality", "coloq_area", 
          paste0("admin_area_l", 5:1), "country", "postal_code"), 
        names=c(
            "point_of_interest", "room", "floor",  "subpremise", "premise", 
            "neighborhood", "street_number", 
            "street_address", "intersection", "route", 
            paste0("sublocality_level_", 5:1), "locality", "colloquial_area", 
            paste0("administrative_area_level_", 5:1), "country", "postal_code"))
    ## short_name or long_name
    attr_name <- paste0(name_type, "_name")
    ## put less important info together
    attr_tags <- c("establishment", "park", "airport", "university", "natural_feature")
    
    ## did geocode fail?
    if (length(gc) == 0 || gc$status != 'OK') {
        coord <- data.frame(lat = NA_real_, lng = NA_real_, 
                            loctype = NA_character_, address = NA_character_)
        if (length(gc) > 0) if (gc$status != 'OK')
            warning(paste0('geocode failed with status ', gc$status, 
                           ', location = "', gc_name, '"'), 
                    call. = FALSE)
    }else{
        coord <- data.frame(
            lat = ifnull(gc$results$geometry$location[['lat']], NA_real_),
            lng = ifnull(gc$results$geometry$location[['lng']], NA_real_),
            loctype = ifnull(gc$results$geometry$location_type, NA_character_),
            address = ifnull(gc$results$formatted_address, NA_character_),
            stringsAsFactors=FALSE)
        if (output == 'all'){
            # address components
            attrdf <- lapply(gc$results$address_components, function(df) 
                unnest(df) %>% filter(! is.na(types)))
            coord <- lapply(seq_len(nrow(coord)), function(i){
                # named vector
                vec_attr <- structure(attrdf[[i]][[attr_name]], 
                                      names=attrdf[[i]]$types)
                df_coord <- coord[i, , drop=FALSE]
                df_coord[, valid_attrs] <- ifnull(
                    vec_attr[names(valid_attrs)], NA_character_)
                df_coord[, "tags"] <- paste(attr_tags[attr_tags %in% names(vec_attr)], 
                                            collapse=", ")
                return(df_coord)
            }) %>% do.call('bind_rows', .) %>% 
                mutate(street_no=tryCatch(as.integer(street_no), 
                                          error=function(e) NA_integer_), 
                       postal_code=tryCatch(as.integer(postal_code), 
                                            error=function(e) NA_integer_))
        }
    }    

    out <- if (is.null(gcs)) coord else 
        format_gcdf(coord, ics_china=ics_china, ics_intl=ics_intl, ocs=gcs)
    if (idf) out <- data.frame(idf=name_out, out)
    
    return(out)
}

#' @importFrom aseskit iif ifnull
#' @importFrom dplyr mutate
.parse_geocode_result.baidu_geocode <- function(
    gc, output = c('latlng', 'latlngc', 'all'), gcs=NULL, ics_china='BD-09', 
    ics_intl='BD-09', name_out=NULL, idf=TRUE,
...){
    
    # check args
    dots <- list(...)
    output <- match.arg(output)
    if (! is.null(gcs)) 
        gcs <- match.arg(gcs, c('WGS-84', 'GCJ-02', 'BD-09'))

    if (!missing(ics_china)) ics_china <- match.arg(ics_china, c('WGS-84', 'GCJ-02', 'BD-09'))
    if (!missing(ics_intl)) ics_intl <- match.arg(ics_intl, c('WGS-84', 'GCJ-02', 'BD-09'))
    name_out <- ifnull(name_out, attr(gc, 'name_out'))
    
    # did geocode fail?
    if (length(gc) == 0 || as.numeric(gc$status) != 0) {
        coord <- data.frame(lat = NA_real_, lng = NA_real_,
                            loctype = NA_character_, address = NA_character_)
        if (length(gc) > 0) if (as.numeric(gc$status) != 0)
            warning(paste0('geocode failed with status code ', gc$status, ' (',
                           gc$msg, ') , location = "', name_out, '". see more ',
                           'details in the response code table of Baidu Geocoding API'),
                    call. = FALSE)
    }else{
        coord <- data.frame(lat = ifnull(gc$result$location['lat'], NA_real_),
                            lng = ifnull(gc$result$location['lng'], NA_real_),
                            loctype = ifnull(gc$result$level, NA_character_),
                            address = NA_character_,
                            stringsAsFactors=FALSE)
        if (output == 'all'){
            attrdf <- data.frame(
                precise = ifnull(gc$result$precise, NA_integer_),
                conf = ifnull(gc$result$confidence, NA_integer_),
                compreh = ifnull(gc$result$comprehension, NA_integer_),
                stringsAsFactors=FALSE)
            coord <- cbind(coord, attrdf) %>% 
                mutate(street_no=tryCatch(as.integer(street_no), 
                                          error=function(e) NA_integer_), 
                       postal_code=tryCatch(as.integer(postal_code), 
                                            error=function(e) NA_integer_))
        }
    }
    
    out <- if (is.null(gcs)) coord else 
        format_gcdf(coord, ics_china=ics_china, ics_intl=ics_intl, ocs=gcs)
    if (idf) out <- data.frame(idf=name_out, out)
    return(out)
}

#' @importFrom aseskit iif ifnull ifempty ifna
#' @importFrom dplyr mutate select rename
#' @importFrom tidyr separate
.parse_geocode_result.gaode_geocode <- function(
    gc, output=c('latlng', 'latlngc', 'latlnga', 'all'), gcs=NULL,
    ics_china='GCJ-02', ics_intl='WGS-84', name_out=NULL, idf=TRUE,
...){
    
    # ----check args----
    dots <- list(...)
    output <- match.arg(output)
    if (! is.null(gcs)) 
        gcs <- match.arg(gcs, c('WGS-84', 'GCJ-02', 'BD-09'))
    
    if (!missing(ics_china)) ics_china <- match.arg(ics_china, c('WGS-84', 'GCJ-02', 'BD-09'))
    if (!missing(ics_intl)) ics_intl <- match.arg(ics_intl, c('WGS-84', 'GCJ-02', 'BD-09'))
    name_out <- ifnull(name_out, attr(gc, 'name_out'))
    
    # did geocode fail?
    if (length(gc) == 0 || as.numeric(gc$status) != 1) {
        coord <- data.frame(lat = NA_real_, lng = NA_real_, 
                            loctype = NA_character_,
                            address = NA_character_)
        if (length(gc) > 0) if (as.numeric(gc$status) != 1)
            warning(paste0('`geocode` failed with status code ', gc$status, ' (',
                           gc$info, '), location = "', name_out,  '". see more details ',
                           'in the response code table of Gaode Geocoding API'),
                    call. = FALSE)
    }else{
        n_out <- as.integer(gc$count)  # number of parsed results
        if (n_out == 0){
            warning('`geocode` cannot parse the address. No matches were returned.')
            coord <- data.frame(lat = NA_real_, lng = NA_real_, 
                                loctype = NA_character_,
                                address = NA_character_)
        }else{
            coord <- ifempty(gc$geocodes[c('formatted_address', 'level', 'location')],
                             NA_character_)
            coord$location <- ifna(coord$location, ',') %>% unlist()
            # divide location to lng and lat, and exchange them
            coord <- separate(coord, location, into=c('lng', 'lat'), sep=',', 
                              convert=TRUE, fill='left') %>% 
                mutate(tmp=lat, lat=lng, lng=tmp) %>% select(-tmp) %>% 
                rename(address=formatted_address, loctype=level, lat=lng, lng=lat)
            
            if (output == 'all'){
                attrdf <- gc$geocodes %>% 
                    select(-formatted_address, -level, -location) %>% 
                    mutate(nbrhd_name = gc$geocodes$neighborhood$name,
                           nbrhd_type = gc$geocodes$neighborhood$type,
                           blding_name = gc$geocodes$building$name,
                           blding_type = gc$geocodes$building$type) %>% 
                    select(-neighborhood, -building)
                
                # # coerce nested list() to NAs. Deprecated
                # attrdf[] <- vapply(names(attrdf), function(nm){
                #     o <- ifempty(attrdf[[nm]], NA_character_)
                #     if (is.list(o)) o <- vapply(o, function(v) {
                #         if (all(is.na(v))) NA_character_ else paste(ifna(v, ""))
                #     }, FUN.VALUE=character(1L))
                #     return(unlist(o))
                # }, FUN.VALUE = character(n_out)) 
                
                attrdf <- ifempty(attrdf, NA_character_) %>% 
                    mutate(adcode = as.integer(adcode), 
                           number = as.integer(number)) %>% 
                    rename(street_no = number) %>% 
                    mutate(postal_code=tryCatch(as.integer(postal_code), 
                                                error=function(e) NA_integer_))
                
                coord <- cbind(coord, attrdf)
            }
        }
    }
    
    out <- if (is.null(gcs)) coord else 
        format_gcdf(coord, ics_china=ics_china, ics_intl=ics_intl, ocs=gcs)
    
    # gaode batch mode gets 'adr1|adr2|adr3|...', str_split it and name the result
    if (idf) out <- data.frame(
        idf=as.factor(name_out) %>% levels %>% strsplit('\\|') %>% unlist, out)
    return(out)
}

.parse_geocode_reuslt.default <- function(
    gc, output=c('latlng', 'latlngc', 'latlnga', 'all'), gcs=NULL,
    ics_china='GCJ-02', ics_intl='WGS-84',
...){
    if (! inherits(gc, 'api_data'))
        warning('gc must be of "api_data" class.')
    stop('gc must be of subclasses of "api_data", typically yielded using aseskit::get_api_data().')
}

# ================= Specific Geocoders =============================
#' @importFrom aseskit get_api_data synthesize_api getApiKey iif ifnull
geocode_google_api <- function(
    address, output=c('latlng', 'latlngc', 'latlnga', 'all', 'raw'), 
    ocs=c('WGS-84', 'GCJ-02', 'BD-09'), messaging = FALSE, time = 0, 
    use_curl=TRUE, idf=TRUE, key=NULL, client=NULL, signature=NULL, 
    name_type=c('long', 'short'), language=NULL, region=NULL, components=NULL, 
...){
    # 1 synthesizes urls and retrieve json lists and then, 2 parse the lists

    # -----check args-----
    dots <- list(...)
    output <- match.arg(output)
    ocs <- match.arg(ocs)
    stopifnot(is.logical(use_curl))
    stopifnot(is.character(address))
    
    # different google maps api is used based on user's location. If user is 
    # inside China, ditu.google.cn is used; otherwise maps.google.com is used.
    ip.country()  # check ip country and store the result in options
    
    ## authorization parameters
    client <- ifnull(client, '')
    signature <- ifnull(signature, '')
    if (nchar(client) > 0 && nchar(signature) > 0){
        key <- NULL
    }else if (is.null(key)){
        key <- getApiKey("google")
    }else if (! is.character(key) || key == ""){
        stop("Please use either a valid client + signature pair (premium account), ",
             "or a google maps API key.")
    }
    name_type <- ifnull(name_type, 'long')
    name_type <- match.arg(name_type, c('long', 'short'))
    ## address vector
    if (length(address) > 1) {  # if many addresses are given, warn it.
        if (messaging) invisible(message(paste(
            'Each request costs around USD $0.01.\n', 
            'Google Cloud Platform limits 50 requests/second at most.')))
        if (length(address) > 50 & messaging) 
            message("You passed in 50+ addresses. Mind your quota and costs.")
    }
    address <- gsub("\\s+", "\\+", address)  # replace spaces in addresses with '+'
    region <- ifnull(region, '')
    components <- ifnull(components, '')
    
    # -----synthesize urls-----
    urls <- synthesize_api(
        url_body=address, provider='google', api='geocode', region=region, 
        components=components, key=key, client=client, signature=signature, 
        use_curl=use_curl, name_type=name_type, language=language, ...)

    # -----geocode------
    gclst <- get_api_data(urls, use_curl=use_curl, time=time, messaging=messaging,
                          name_out=address)
    
    # ----- output -----
    if (output == 'raw') return(gclst)
    return(parse_geocodes(
        gclst, gcs=ocs, idf=idf, output=output, name_type=name_type, 
        ics_china='GCJ-02',  ics_intl=if (getOption('ip.country') == 'CN') 
            'GCJ-02' else 'WGS-84'))
}

#' @importFrom aseskit get_api_data synthesize_api getApiKey iif ifnull
geocode_baidu_api <- function(
    address, output = c('latlng', 'latlngc', 'all', 'raw'),
    ocs=c('WGS-84', 'GCJ-02', 'BD-09'), messaging = FALSE, time = 0, 
    use_curl=FALSE, idf=TRUE, key=NULL, city=NULL,
...){
    
    # -----check args-------
    dots <- list(...)
    output <- match.arg(output)
    ocs <- match.arg(ocs)
    ics <- if (ocs == 'WGS-84') 'BD-09' else ocs
    city <- ifnull(city, '')
    stopifnot(is.character(city))
    stopifnot(is.character(address))
    
    if (is.null(key)){
        key <- getApiKey("baidu")
    }else if (!is.character(key) || key == ""){
        stop("Please use a valid baidu map API key.")
    }
    
    # -----synthesize urls-------
    urls <- synthesize_api(
        url_body=address, provider='baidu', api='geocode', city=city, key=key, 
        use_curl=use_curl, ...)
    
    # -----geocode------
    gclst <- get_api_data(urls, use_curl=use_curl, time=time, messaging=messaging,
                          name_out=address)
    
    # ----- output -----
    if (output == 'raw') return(gclst)
    return(parse_geocodes(
        gclst, gcs=ocs, idf=idf, output=output, ics_china=ics, ics_intl=ics))
}

#' @importFrom aseskit get_api_data synthesize_api getApiKey
geocode_gaode_api <- function(
    address, output = c('latlng', 'latlngc', 'latlnga', 'all', 'raw'), 
    ocs=c('WGS-84', 'GCJ-02', 'BD-09'), messaging = FALSE, time = 0, 
    use_curl=FALSE, idf=TRUE, key=NULL, sig=NULL, city=NULL, batch=TRUE,
...){
    # ----check args-----
    dots <- list(...)
    output <- match.arg(output)
    ocs <- match.arg(ocs)
    stopifnot(is.character(address))
    stopifnot(is.logical(batch))
    
    if (is.null(key)){
        key <- getApiKey("gaode")
    }else if (!is.character(key) || key == ""){
        stop("Please use a valid gaode map API key.")
    }
    # backup address: Gaode supports batch mode, address will be concatenated.
    address_in <- address  
    # -----concatenate address, 10 / batch-------
    if (length(address) > 1){
        split_by <- as.vector(mapply(
            rep, 1:ceiling(length(address)/10), 10))[seq_along(address)]
        address <- split(address, split_by)
        address <- vapply(address, paste, FUN.VALUE=character(1L), 
                          collapse="|")
    }else{
        batch <- FALSE
    }
    
    # -----synthesize urls-------
    urls <- synthesize_api(
        url_body=address, provider='gaode', api='geocode', city=city, key=key, 
        sig=sig, use_curl=use_curl, batch=batch, ...)
    
    # -----geocode------
    gclst <- get_api_data(urls, use_curl=use_curl, time=time, messaging=messaging,
                          name_out=address)
    
    # ----- output -----
    if (output == 'raw') return(gclst)
    return(parse_geocodes(
        gclst, gcs=ocs, idf=idf, output=output, ics_china='GCJ-02', ics_intl='WGS-84'))
}

