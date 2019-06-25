# ======================Read API=======================
# use aseskit::parse_api_data

#' @export
.parse_api_data.google_geohost <- function(x, ...)
    parse_geohost_result(x, ...)

#' @export
.parse_api_data.baidu_geohost <- .parse_api_data.google_geohost

#' @export
.parse_api_data.gaode_geohost <- .parse_api_data.google_geohost

#' @export
.parse_api_data.google_geocode <- function(x, ...)
    parse_geocode_result(x, ...)

#' @export
.parse_api_data.baidu_geocode <- .parse_api_data.google_geocode

#' @export
.parse_api_data.gaode_geocode <- .parse_api_data.google_geocode

#' @export
.parse_api_data.google_revgeocode <- function(x, ...)
    parse_revgeocode_result(x, ...)

#' @export
.parse_api_data.baidu_revgeocode <- .parse_api_data.google_revgeocode

#' @export
.parse_api_data.gaode_revgeocode <- .parse_api_data.google_revgeocode

# .parse_api_data.google_convcoord <- function(x, ...)
#     parse_convcoord_result(x, ...)

#' @export
.parse_api_data.baidu_convcoord <- function(x, ...)
     parse_convcoord_result(x, ...)

#' @export
.parse_api_data.gaode_convcoord <- .parse_api_data.baidu_convcoord

#' @export
.parse_api_data.ipify_geohost <- function(x, ...)
    parse_geohost_result(x, ...)

#' @export
.parse_api_data.ipinfo_geohost <- .parse_api_data.ipify_geohost

#' @export
.parse_api_data.ipstack_geohost <- .parse_api_data.ipify_geohost

#' @export
.parse_api_data.ipify_getip <- function(x, ...)
    parse_getip_result(x, ...)

#' @export
.parse_api_data.default <- aseskit:::.parse_api_data.default

# ==============Read API for data============
# use aseskit::read_api

#' @export
.read_api.default <- aseskit:::.read_api.default

#' @export
.read_api.google_api_geocode <- .read_api.default

#' @export
.read_api.google_api_revgeocode <- .read_api.google_api_geocode

#' @export
.read_api.baidu_api_geocode <- function(
    api_url, use_curl=FALSE, parse_json=TRUE, time=0, encoding='UTF-8', 
    url_encode=TRUE, name_out=NULL, ...){
    # use_curl = FALSE, and encoding='UTF-8'
    extract_api_data(api_url=api_url, use_curl=use_curl, parse_json=TRUE, 
                     time=time, encoding=encoding, url_encode=url_encode, 
                     name_out=name_out, ...)
}

#' @export
.read_api.baidu_api_revgeocode <- .read_api.baidu_api_geocode

#' @export
.read_api.baidu_api_convcoord <- .read_api.baidu_api_geocode

#' @export
.read_api.gaode_api_geocode <- function(
    api_url, use_curl=FALSE, parse_json=TRUE, time=0, encoding='UTF-8', 
    url_encode=TRUE, name_out=NULL, ...){
    # use_curl = FALSE, and encoding='UTF-8'
    extract_api_data(api_url=api_url, use_curl=use_curl, parse_json=TRUE, 
                     time=time, encoding=encoding, url_encode=url_encode, 
                     name_out=name_out, ...)
}

#' @export
.read_api.gaode_api_revgeocode <- .read_api.gaode_api_geocode

#' @export
.read_api.gaode_api_convcoord <- .read_api.gaode_api_geocode

#' @export
.read_api.ipinfo_api_geohost <- function(
    api_url, use_curl=FALSE, parse_json=TRUE, time=0, encoding='unknown', 
    url_encode=TRUE, name_out=NULL, ...){
    # use_curl = FALSE, and encoding='UTF-8'
    extract_api_data(api_url=api_url, use_curl=use_curl, parse_json=TRUE, 
                     time=time, encoding=encoding, url_encode=url_encode, 
                     name_out=name_out, ...)
}

#' @export
.read_api.ipstack_api_geohost <- .read_api.ipinfo_api_geohost

#' @export
.read_api.ipify_api_geohost <- .read_api.ipinfo_api_geohost

#' @export
.read_api.ipify_api_getip <- .read_api.ipinfo_api_geohost

# ================Synthesize API URLs======================
# use aseskit::synthesize_apis()

## -------------Google api-----------------

synthesize_google_api <- function(
    url_body, api=c('geocode', 'revgeocode'), use_curl=TRUE, ...){
    
    api <- match.arg(api)
    stopifnot(is.character(url_body))
    stopifnot(is.logical(use_curl))
    
    fun_dict <- list(geocode    = synthesize_google_api_geocode,
                     revgeocode = synthesize_google_api_revgeocode
                )
    fun <- fun_dict[[api]]
    fun(url_body=url_body, use_curl=use_curl, ...)
}

#' @importFrom glue glue
#' @importFrom aseskit iif ifnull aline
embed_google_api_generic <- function(
    url_char, language=NULL, client='', signature='', key=NULL, ...){
    # generic function to add auth in API
    stopifnot(is.character(url_char))
    len <- length(url_char)
    
    if (! is.null(language)){
        valid_lang <- c(
            "ar", "be", "lv", "bg", "mk", "bn", "ml", "ca", "mr", "cs", "my",
            "da", "nl", "de", "no", "el", "pa", "en", "pl", "en-Au", "pt", 
            "en-GB", "pt-BR", "es", "pt-PT", "eu", "ro", "fa", "ru", "fi", "sk",
            "fil", "sl", "fr", "sq", "gl", "sr", "gu", "sv", "hi", "ta", "hr", 
            "te", "hu", "th", "id", "tl", "it", "tr", "iw", "uk", "ja", "uz", 
            "kk", "vi", "kn", "zh-CN","ko", "zh-TW", "ky")
        lang <- match.arg(language, valid_lang, several.ok=TRUE)
        url_char <- glue('{url_char}&language={aline(lang, len)}')
    }
    # add authorization part
    if (nchar(ifnull(client, '')) > 0 && nchar(ifnull(signature, '')) > 0){
        url_char <- glue('{url_char}&client={client}&signature={signature}')
    }else if (is.null(key)) {
        url_char <- glue('{url_char}&key={getApiKey("google")}')
    }else if (is.na(key)){
        url_char <- url_char
    }else if (is.character(key) && nchar(key) > 0) {
        url_char <- glue('{url_char}&key={key}')
    }else{
        stop('either client and signature, or a valid API key must be provided ',
             'for Google API!')
    }
    return(url_char)
}

#' @importFrom aseskit iif ifnull check_curl aline
#' @importFrom glue glue
synthesize_google_api_geocode <- function(
    url_body, region=NULL, components=NULL, use_curl=TRUE, ...){
    
    # check args
    dots <- list(...)
    stopifnot(is.null(region) || is.character(region))
    stopifnot(is.null(components) || is.character(components))
    name_out <- ifnull(dots$name_out, url_body)
    
    len <- length(url_body)
    
    # Choose a google api_url
    # https://maps.googleapis.com/maps/api/geocode/json?address=ADDRESS&key=API_KEY 
    # for outside China
    # https://ditu.google.cn/maps/api/geocode/json?address=ADDRESS&key=API_KEY 
    # for inside China
    cname <- try(ip.country(), silent=TRUE)
    api_url <- if (cname == "CN") 'https://ditu.google.cn/maps/api/' else
        api_url <- 'https://maps.googleapis.com/maps/api/'
    
    # start
    api_url <- glue('{api_url}geocode/json')
    use_curl <- check_curl(api_url, "Google Maps Geocoding", use_curl)
    
    url_char <- glue('{api_url}?address={url_body}')
    
    if (! is.null(region)){
        region <- aline(region, length(url_body))
        valid_reg <- which(nchar(region) > 0)
        if (length(valid_reg) > 0) url_char[valid_reg] <- glue(
            '{url_char[valid_reg]}&region={region[valid_reg]}')
    }
    if (! is.null(components)){
        components <- aline(components, length(url_body))
        valid_cmp <- which(nchar(components) > 0)
        if (length(valid_cmp) > 0) url_char[valid_cmp] <- glue(
            '{url_char[valid_cmp]}&components={components[valid_cmp]}')
    }
    url_char <- embed_google_api_generic(url_char, ...)
    return(mapply(structure, url_char, name_out=name_out, MoreArgs=list(
        class=c('google_api_geocode', 'url'), use_curl=use_curl),
        SIMPLIFY=FALSE))
}

#' @importFrom glue glue
#' @importFrom aseskit iif ifnull check_curl aline
synthesize_google_api_revgeocode <- function(
    url_body, result_type=NULL, location_type=NULL, use_curl=TRUE, 
    ...){
    
    # check args
    dots <- list(...)
    stopifnot(is.null(result_type) || is.character(result_type))
    stopifnot(is.null(location_type) || is.character(location_type))
    ## url_body valid? if invalid, url = NA_character_
    stopifnot(is.character(url_body))
    valid_url_body <- grepl("^\\-?\\d+?\\.?\\d*?,\\-?\\d+?\\.?\\d*$", url_body)
    if (! all(valid_url_body))
        warning('latlng must be in the format of "lat,lon;lat,lon". Coordinates #',
                paste(which(! valid_url_body), collapse=', '), 
                'are not correctly formatted.')
    name_out <- ifnull(dots$name_out, url_body)
    
    len <- length(url_body)
    
    # Choose a google api_url
    # https://maps.googleapis.com/maps/api/geocode/json?address=ADDRESS&key=API_KEY 
    # for outside China
    # https://ditu.google.cn/maps/api/geocode/json?address=ADDRESS&key=API_KEY 
    # for inside China
    cname <- try(ip.country(), silent=TRUE)
    api_url <- if (cname == "CN") 'https://ditu.google.cn/maps/api/' else
        api_url <- 'https://maps.googleapis.com/maps/api/'
    
    # start
    api_url <- glue('{api_url}geocode/json')
    use_curl <- check_curl(api_url, "Google Maps Geocoding", use_curl)
    url_char <- glue('{api_url}?latlng={url_body}')
    
    if (length(result_type) > 0){
        result_type_dict <- c(
            'street_address', 'route', 'intersection', 'political', 'country',
            paste('administrative_area_level_', 1:5, sep=''), 'colloquial_area',
            'locality', 'sublocality', paste('sublocality_level_', 1:5, sep=''),
            'neighborhood', 'premise', 'subpremise', 'postal_code', 'natural_feature',
            'airport', 'park', 'point_of_interest')
        result_type <- aline(result_type, len)
        valid_rslt_type <- vapply(
            strsplit(result_type, '\\|'), function(rslt_type){
                all(rslt_type %in% result_type_dict)}, FUN.VALUE=logical(1L))
        if (! all(valid_rslt_type)) 
            warning('result_type must be in ', paste(result_type_dict, collapse=', '),
                    ' or these valid values concatenated with pipe (|).')
        if (any(valid_rslt_type))
            url_char[valid_rslt_type] <- glue(
                '{url_char[valid_rslt_type]}&result_type={result_type[valid_rslt_type]}')
    }
    if (length(location_type) > 0){
        location_type_dict <- c(
            'ROOFTOP', 'RANGE_INTERPOLATED', 'GEOMETRIC_CENTER', 'APPROXIMATE')
        location_type <- aline(location_type, len)
        valid_loc_type <- vapply(
            strsplit(location_type, '\\|'), function(loc_type){
                all(loc_type %in% location_type_dict)}, FUN.VALUE=logical(1L))
        if (! all(valid_loc_type))
            warning('location_type must be in ', paste(location_type_dict, collapse=', '),
                    ' or these valid values concatenated with pipe (|).')
        if (any(valid_loc_type))
            url_char[valid_loc_type] <- glue(
                '{url_char[valid_loc_type]}&location_type={location_type[valid_loc_type]}')
    }
    url_char <- embed_google_api_generic(url_char, ...)
    url_char[! valid_url_body] <- NA_character_
    return(mapply(structure, url_char, name_out=url_body, MoreArgs=list(
        class=c('google_api_revgeocode', 'url'), use_curl=use_curl),
        SIMPLIFY=FALSE))
}

# --------------baidu api-----------------------

synthesize_baidu_api <- function(
    url_body, api=c('geocode', 'revgeocode', 'convcoord'), use_curl=FALSE, ...){
    
    api <- match.arg(api)
    stopifnot(is.character(url_body))
    
    fun_dict <- list(geocode    = synthesize_baidu_api_geocode,
                     revgeocode = synthesize_baidu_api_revgeocode,
                     convcoord  = synthesize_baidu_api_convcoord
    )
    fun <- fun_dict[[api]]
    fun(url_body=url_body, use_curl=use_curl, ...)
}

#' @importFrom glue glue
#' @importFrom aseskit getApiKey iif
embed_baidu_api_generic <- function(url_char, key=NULL, ...){
    stopifnot(is.character(url_char))
    if (is.null(key)){
        key <- getApiKey("baidu")
        url_char <- glue('{url_char}&ak={key}')
    }else if (is.na(key)){
        
    }else if (is.character(key) && nchar(key) > 0){
        url_char <- glue('{url_char}&ak={key}')
    }else{
        stop("Please use a valid baidu API key.")
    }
    return(url_char)
}

#' @importFrom aseskit check_curl aline iif ifnull
#' @importFrom glue glue
synthesize_baidu_api_geocode <- function(
    url_body, ics=c('BD-09', 'GCJ-02', 'WGS-84'), 
    ocs=c('BD-09', 'GCJ-02', 'WGS-84'), city=NULL, use_curl=FALSE, ...){
    # check args
    dots <- list(...)
    stopifnot(is.null(city) || is.character(city))
    ics <- if (! is.null(ics)) match.arg(ics, several.ok=TRUE) else 'BD-09'
    ocs <- if (! is.null(ocs)) match.arg(ocs, several.ok=TRUE) else 'BD-09'
    
    dots <- list(...)
    name_out <- ifnull(dots$name_out, url_body)
    
    len <- length(url_body)
    ocs <- aline(ocs, len)
    ics <- aline(ics, len)
    
    # start
    api_url <- 'http://api.map.baidu.com/geocoder/v2/'
    use_curl <- check_curl(api_url, "Baidu Map Geocoding", use_curl)
    
    url_char <- glue('{api_url}?address={url_body}&output=json')
    ocs_gcj <- which(ocs == "GCJ-02")
    if (length(ocs_gcj) > 0)
        url_char[ocs_gcj] <- glue('{url_char[ocs_gcj]}&ret_coordtype=gcj02ll')
    if (! is.null(city)) {
        city <- aline(city, len)
        valid_city <- city[nchar(city) > 0]
        url_char[valid_city] <- glue(
            '{url_char[valid_city]}&city={city[valid_city]}')
    }
    url_char <- embed_baidu_api_generic(url_char, ...)
    return(mapply(structure, url_char, name_out=url_body, MoreArgs=list(
        class=c('baidu_api_geocode', 'url'), use_curl=use_curl),
        SIMPLIFY=FALSE))
}

#' @importFrom glue glue
#' @importFrom aseskit iif ifnull check_curl aline
synthesize_baidu_api_revgeocode <- function(
    url_body, ics=c('BD-09', 'GCJ-02', 'WGS-84'), 
    ocs=c('BD-09', 'GCJ-02', 'WGS-84'), pois=NULL, radius=NULL, 
    extensions_road=FALSE, extensions_town=FALSE, language=NULL, 
    language_auto=NULL, latest_admin=0, use_curl=FALSE, ...
){
    # check args
    stopifnot(is.character(url_body))
    # valid url? if invalid, url = NA_character_
    valid_url_body <- grepl("^\\-?\\d+?\\.?\\d*?,\\-?\\d+?\\.?\\d*$", url_body)
    if (! all(valid_url_body))
        warning('latlng must be in the format of "lat,lon". Coordinates #',
                paste(which(! valid_url_body), collapse=', '), 
                'are not correctly formatted.')
    
    stopifnot(is.null(pois) || is.numeric(pois))
    stopifnot(is.null(radius) || is.numeric(radius))
    stopifnot(is.logical(extensions_road))
    stopifnot(is.logical(extensions_town))
    stopifnot(is.null(language) || is.character(language))
    stopifnot(is.null(language_auto) || is.numeric(language_auto))
    stopifnot(is.numeric(latest_admin))
    ics <- if (! is.null(ics)) match.arg(ics, several.ok=TRUE) else 'BD-09'
    ocs <- if (! is.null(ocs)) match.arg(ocs, several.ok=TRUE) else 'BD-09'
    
    dots <- list(...)
    name_out <- ifnull(dots$name_out, url_body)
    
    len <- length(url_body)
    ocs <- aline(ocs, len)
    ics <- aline(ics, len)
    
    # start
    api_url <- 'http://api.map.baidu.com/geocoder/v2/'
    use_curl <- check_curl(api_url, "Baidu Map RevGeocoding", use_curl)
    url_char <- glue('{api_url}?location={url_body}&output=json')
    
    # if ics == 'BD-09', coordtype can be blank
    ics_gcj <- which(ics == "GCJ-02")
    ics_wgs <- which(ics == "WGS-84")
    url_char[ics_gcj] <- glue('{url_char[ics_gcj]}&coordtype=gcj02ll')
    url_char[ics_wgs] <- glue('{url_char[ics_wgs]}&coordtype=wgs84ll')
    ocs_gcj <- which(ocs == 'GCJ-02')
    url_char[ocs_gcj] <- glue('{url_char[ocs_gcj]}&ret_coordtype=gcj02ll')
    
    if (! is.null(pois)) {
        pois <- aline(pois, len)
        valid_pois <- which(pois %in% 0:1)
        if (length(valid_pois) > 0) url_char[valid_pois] <- glue(
            '{url_char[valid_pois]}&pois={pois[valid_pois]}')
    }
    if (! is.null(radius)){
        radius <- aline(radius, len)
        valid_radius <- which(radius > 0)
        if (length(valid_radius) > 0) url_char[valid_radius] <- glue(
            '{url_char[valid_radius]}&radius={radius[valid_radius]}')
    }
    ext_road <- aline(extensions_road, len)
    if (any(ext_road))
        url_char[ext_road] <- glue(
            '{url_char[ext_road]}&extensions_road=true')
    ext_town <- aline(extensions_town, len)
    if (any(ext_town))
        url_char[ext_town] <- glue(
            '{url_char[ext_town]}&extensions_town=true')
    if (! is.null(language)){
        valid_lang <- c(
            "el", "gu", "en", "vi", "ca", "it", "iw", "sv", "eu", "ar", "cs",
            "gl", "id", "es", "ru", "sr", "nl", "pt", "tr", "tl", "lv", "lt", 
            "th", "ro", "fil", "ta", "fr", "bg", "hr", "bn", "de", "hu", "fa", 
            "hi", "fi", "da","ja", "te", "ml", "ko", "kn", "sk", "pl", "uk", 
            "sl", "mr", "local", "en-GB", "en-AU", "zh-TW", "pt-BR", "pt-PT", 
            "zh-CN")
        language <- match.arg(language, valid_lang, several.ok=TRUE)
        url_char <- glue('{url_char}&language={aline(language, len)}')
    }
    if (! is.null(language_auto)) if (any(language_auto %in% 0:1)){
        language_auto <- aline(language_auto, len)
        valid_lang_auto <- which(language_auto %in% 0:1)
        url_char[valid_lang_auto] <- glue(
            '{url_char[valid_lang_auto]}&language_auto=',
            '{language_auto[valid_lang_auto]}')
    }
    if (any(latest_admin == 1)){
        latest_admin <- aline(latest_admin, len)
        valid_ladmin <- which(latest_admin == 1)
        url_char[valid_ladmin] <- glue(
            '{url_char[valid_ladmin]}&latest_admin=',
            '{latest_admin[valid_ladmin]}')
    }
    url_char <- embed_baidu_api_generic(url_char, ...)
    url_char[! valid_url_body] <- NA_character_
    return(mapply(structure, url_char, name_out=url_body, MoreArgs=list(
        class=c('baidu_api_revgeocode', 'url'), use_curl=use_curl),
        SIMPLIFY=FALSE))
}

#' @importFrom glue glue
#' @importFrom aseskit iif ifnull check_curl
synthesize_baidu_api_convcoord <- function(
    url_body, coord_from=c('BD-09', 'GCJ-02', 'WGS-84'),
    coord_to=c('BD-09', 'GCJ-02', 'WGS-84'), use_curl=FALSE,
...){
    # check args
    coordsys_code <- c(`WGS-84`=1, `GCJ-02`=3, `BD-09`=5)
    coord_from <- match.arg(coord_from)
    coord_from <- coordsys_code[coord_from]
    coord_to <- match.arg(coord_to)
    coord_to <- coordsys_code[coord_to]
    dots <- list(...)
    name_out <- ifnull(dots$name_out, url_body)
    # valid url? if invalid, replace with 999,999
    url_body <- unlist(strsplit(url_body, ';'))
    valid_url_body <- grepl("^\\-?\\d+?\\.?\\d*?,\\-?\\d+?\\.?\\d*$", url_body)
    if (! all(valid_url_body)){
        warning('latlng must be in the format of "lon,lat;lon,lat". Coordinates #',
                paste(which(! valid_url_body), collapse=', '), 
                'are not correctly formatted.')
        url_body[! valid_url_body] <- '999,999'
    }
    url_body <- paste(url_body, collapse=';')

    # start
    api_url <- 'http://api.map.baidu.com/geoconv/v1/'
    use_curl <- check_curl(api_url, "Baidu Coords Convert", use_curl)
    url_char <- glue('{api_url}?coords={url_body}&from={coord_from}',
                     '&to={coord_to}')
    url_char <- embed_baidu_api_generic(url_char, ...)
    return(mapply(structure, url_char, name_out=url_body, MoreArgs=list(
        class=c('baidu_api_convcoord', 'url'), use_curl=use_curl),
        SIMPLIFY=FALSE))
}

# -------------- Gaode api--------------------------

synthesize_gaode_api <- function(
    url_body, api=c('geocode', 'revgeocode', 'convcoord'), use_curl=FALSE, ...){
    
    api <- match.arg(api)
    stopifnot(is.character(url_body))
    
    fun_dict <- list(geocode    = synthesize_gaode_api_geocode,
                     revgeocode = synthesize_gaode_api_revgeocode,
                     convcoord  = synthesize_gaode_api_convcoord
    )
    fun <- fun_dict[[api]]
    fun(url_body=url_body, use_curl=use_curl, ...)
}

#' @importFrom glue glue
#' @importFrom aseskit getApiKey iif ifnull
embed_gaode_api_generic <- function(
    url_char, key=NULL, sig=NULL, ...){
    
    stopifnot(is.character(url_char))
    
    if (is.null(key)){
        key <- getApiKey("gaode")
        url_char <- glue('{url_char}&key={key}')
    }else if (is.na(key)){
        
    }else if (is.character(key) && nchar(key) > 0){
        url_char <- glue('{url_char}&key={key}')
    }else{
        stop("Please use a valid gaode map API key.")
    }
    
    if (nchar(ifnull(sig, '')) > 0) 
        url_char <- glue('{url_char}&sig={sig}')
    
    return(url_char)
}

#' @importFrom aseskit check_curl aline iif ifnull
#' @importFrom glue glue
synthesize_gaode_api_geocode <- function(
    url_body, batch=FALSE, city=NULL, use_curl=FALSE, ...){

    # check args
    dots <- list(...)
    stopifnot(is.logical(batch))
    stopifnot(is.null(city) || is.character(city))
    name_out <- ifnull(dots$name_out, url_body)
    
    len <- length(url_body)
    
    # start
    api_url <- 'https://restapi.amap.com/v3/geocode/geo'
    use_curl <- check_curl(api_url, "Gaode Map (Amap) Geocoding", use_curl)
    
    url_char <- glue('{api_url}?address={url_body}&output=JSON')
    if (! is.null(city)) 
        url_char <- glue('{url_char}&city={aline(city, len)}')
    if (any(batch)) 
        url_char[batch] <- glue('{url_char[batch]}&batch=true')
    
    url_char <- embed_gaode_api_generic(url_char, ...)
    return(mapply(structure, enc2utf8(url_char), name_out=name_out, MoreArgs=list(
        class=c('gaode_api_geocode', 'url'), use_curl=use_curl),
        SIMPLIFY=FALSE))
}

#' @importFrom glue glue
#' @importFrom aseskit iif ifnull check_curl aline
synthesize_gaode_api_revgeocode <- function(
    url_body, batch=FALSE, extensions=c('all', 'base'), poitype=NULL, radius=NULL, 
    roadlevel=NULL, homeorcorp=NULL, use_curl=FALSE, ...){
    # check args
    dots <- list(...)
    if (!is.null(extensions)) 
        extensions <- match.arg(extensions, several.ok=TRUE)
    stopifnot(is.character(url_body))
    # url_body valid? If invalid, replace it with 999,999
    url_body <- unlist(strsplit(url_body, '\\|'))
    valid_url_body <- grepl("^\\-?\\d+?\\.?\\d*?,\\-?\\d+?\\.?\\d*$", url_body)
    if (! all(valid_url_body)){
        warning('latlng must be in the format of "lon,lat;lon,lat". Coordinates #',
                paste(which(! valid_url_body), collapse=', '), 
                'are not correctly formatted.')
        url_body[! valid_url_body] <- '999,999'
    }
    url_body <- paste(url_body, collapse='|')
    stopifnot(is.null(poitype) || is.character(poitype))
    stopifnot(is.null(radius) || is.numeric(radius))
    stopifnot(is.null(roadlevel) || is.numeric(roadlevel))
    stopifnot(is.null(homeorcorp) || is.numeric(homeorcorp))
    name_out <- ifnull(dots$name_out, url_body)
    
    # start 
    api_url <- 'https://restapi.amap.com/v3/geocode/regeo'
    use_curl <- check_curl(api_url, "Gaode Map (Amap) RevGeocoding", use_curl)
    url_char <- glue('{api_url}?location={url_body}&output=JSON')
    if (any(batch)) 
        url_char[batch] <- glue('{url_char[batch]}&batch=true')
    if (!is.null(extensions)) if (any(extensions == 'all')){
        ext_all <- which(extensions == 'all')
        url_char[ext_all] <- glue('{url_char[ext_all]}&extensions=all')
        if (! is.null(poitype)){
            valid_poitype <- grepl('^(\\d{6}\\|)*\\d{6}$', poitype)
            if (! (all(valid_poitype))) {
                warning("poitype must be valid POI TYPECODEs concatenated with pipe (|)',
                        '(e.g, '001010' or '001010|001020').")
                poitype <- poitype[valid_poitype]
                # Note: when batch is TRUE, poitype not effective
                url_char[valid_poitype] <- glue(
                    '{url_char[valid_poitype]}&poitype=',
                    '{poitype[valid_poitype]}')
            }
        }
        if (! is.null(radius)) {
            radius <- aline(radius, len)
            valid_radius <- which(radius > 0)
            if (length(valid_radius) > 0) url_char[valid_radius] <- glue(
                '{url_char[valid_radius]}&radius={radius[valid_radius]}')
        }
        if (! is.null(roadlevel)) {
            roadlevel <- aline(roadlevel, len)
            valid_roadlvl <- which(roadlevel %in% 0:1)
            if (length(vald_roadlvl)) url_char[valid_roadlvl] <- glue(
                '{url_char[valid_roadlvl]}&roadlevel={roadlevel[valid_roadlvl]}')
        }
        if (! is.null(homeorcorp)){
            homeorcorp <- aline(homeorcorp, len)
            valid_hoc <- which(homeorcorp %in% 0:2)
            if (length(valid_hoc) > 0) url_char[valid_hoc] <- glue(
                '{url_char[valid_hoc]}&homeorcorp={homeorcorp[valid_hoc]}')
        }
    }
    url_char <- embed_gaode_api_generic(url_char, ...)
    
    return(mapply(structure, enc2utf8(url_char), name_out=name_out, MoreArgs=list(
        class=c('gaode_api_revgeocode', 'url'), use_curl=use_curl),
        SIMPLIFY=FALSE))
}


#' @importFrom glue glue
#' @importFrom aseskit iif ifnull check_curl
synthesize_gaode_api_convcoord <- function(
    url_body, batch=FALSE, coord_from=c('BD-09', 'GCJ-02', 'WGS-84'),
    coord_to=c('BD-09', 'GCJ-02', 'WGS-84'), use_curl=FALSE, ...){
    
    # check args
    dots <- list(...)
    coord_code <- c(`WGS-84`='gps', `GCJ-02`='autonavi', `BD-09`='baidu')
    coord_from <- match.arg(coord_from)
    if (coord_from == 'GCJ-02')
        stop('gaode map only supports conversion to GCJ-02. No transformation ',
             'will be actually conducted (but API usage quota will be consumed anyway). ',
             'Coordinate conversion has been stopped.')
    coord_from <- coord_code[coord_from]
    coord_to <- match.arg(coord_to)
    if (coord_to != "GCJ-02") 
        stop('gaode map API now only supports conversion to GCJ-02 (autonavi).')
    name_out <- ifnull(dots$name_out, url_body)
    # url_body valid? If invalid, replace with 999,999
    stopifnot(is.character(url_body))
    url_body <- unlist(strsplit(url_body, ';'))
    valid_url_body <- grepl("^\\-?\\d+?\\.?\\d*?,\\-?\\d+?\\.?\\d*$", url_body)
    if (! all(valid_url_body)){
        warning('latlng must be in the format of "lon,lat;lon,lat". Coordinates #',
                paste(which(! valid_url_body), collapse=', '), 
                'are not correctly formatted.')
        url_body[! valid_url_body] <- '999,999'
    }
    url_body <- paste(url_body, collapse=';')
    
    # start
    api_url <- 'https://restapi.amap.com/v3/assistant/coordinate/'
    use_curl <- check_curl(api_url, "Baidu Coords Convert", use_curl)
    url_char <- glue('{api_url}convert?locations={url_body}',
                     '&coordsys={coord_from}&output=json')
    url_char <- embed_gaode_api_generic(url_char, ...)
    
    return(mapply(structure, enc2utf8(url_char), name_out=name_out, MoreArgs=list(
        class=c('gaode_api_convcoord', 'url'), use_curl=use_curl),
        SIMPLIFY=FALSE))
}

# -----------------ipify api------------------

synthesize_ipify_api <- function(
    url_body, api=c('geohost', 'getip'), use_curl=FALSE, ...){
    
    api <- match.arg(api)
    stopifnot(is.character(url_body))
    
    fun_dict <- list(geohost = synthesize_ipify_api_geohost,
                     getip   = synthesize_ipify_api_getip)
    fun <- fun_dict[[api]]
    fun(url_body=url_body, use_curl=use_curl, ...)
}

#' @importFrom glue glue
#' @importFrom aseskit getApiKey iif check_curl
embed_ipify_api_generic <- function(url_char, key=NULL, ...){
    stopifnot(is.character(url_char))
    
    if (is.null(key)){
        key <- getApiKey('ipify')
    }else if (is.na(key)){
        return(url_char)
    }
    url_char <- glue('{url_char}?apiKey={key}')
    return(url_char)
}

#' @importFrom glue glue
#' @importFrom aseskit iif ifnull
synthesize_ipify_api_geohost <- function(url_body, use_curl=FALSE,...){
    # check args
    dots <- list(...)
    stopifnot(is.character(url_body))
    name_out <- ifnull(dots$name_out, url_body)
    
    # start
    api_url <- "https://geo.ipify.org/api/v1"
    use_curl <- check_curl(api_url, "ipify geohost", use_curl)
    url_char <- api_url
    url_char <- embed_ipify_api_generic(url_char, ...)  # key first
    url_char <- glue('{url_char}&ipAddress={url_body}')
    
    return(mapply(structure, url_char, name_out=name_out, MoreArgs=list(
        class=c('ipify_api_geohost', 'url'), use_curl=use_curl),
        SIMPLIFY=FALSE))
}

#' @importFrom aseskit iif ifnull
#' @importFrom glue glue
synthesize_ipify_api_getip <- function(url_body, use_curl=FALSE,...){
    # check args
    dots <- list(...)
    stopifnot(is.character(url_body))
    name_out <- ifnull(dots$name_out, url_body)
    
    # start
    api_url <- "https://api.ipify.org?format=json"
    use_curl <- check_curl(api_url, "ipify getip", use_curl)
    url_char <- api_url
    
    return(mapply(structure, url_char, name_out=name_out, MoreArgs=list(
        class=c('ipify_api_getip', 'url'), use_curl=use_curl),
        SIMPLIFY=FALSE))
}

# -----------ipstack api-----------

synthesize_ipstack_api <- function(
    url_body, api=c('geohost'), use_curl=FALSE, ...){
    
    api <- match.arg(api)
    stopifnot(is.character(url_body))
    
    fun_dict <- list(geohost    = synthesize_ipstack_api_geohost)
    fun <- fun_dict[[api]]
    fun(url_body=url_body, use_curl=use_curl, ...)
}

#' @importFrom glue glue
#' @importFrom aseskit getApiKey iif check_curl
embed_ipstack_api_generic <- function(url_char, key=NULL, ...){
    stopifnot(is.character(url_char))
    if (is.null(key)) {
        key <- getApiKey('ipstack')
    }else if (is.na(key)) {  # if NA, do not use the key
        return(url_char)
    }
    url_char <- glue('{url_char}?access_key={key}')
    return(url_char)
}

#' @importFrom aseskit iif ifnull
#' @importFrom glue glue
synthesize_ipstack_api_geohost <- function(url_body, use_curl=FALSE, ...){
    # check args
    dots <- list(...)
    stopifnot(is.character(url_body))
    name_out <- ifnull(dots$name_out, url_body)
    
    # start
    api_url= "http://api.ipstack.com/"
    use_curl <- check_curl(api_url, "ipstack geohost", use_curl)
    url_char <- glue('{api_url}{url_body}')
    url_char <- embed_ipstack_api_generic(url_char, ...)

    return(mapply(structure, url_char, name_out=name_out, MoreArgs=list(
        class=c('ipstack_api_geohost', 'url'), use_curl=use_curl),
        SIMPLIFY=FALSE))
}

# -----------------ipinfo api------------------

synthesize_ipinfo_api <- function(
    url_body, api=c('geohost'), use_curl=FALSE, ...){
    
    api <- match.arg(api)
    stopifnot(is.character(url_body))
    
    fun_dict <- list(geohost    = synthesize_ipinfo_api_geohost)
    fun <- fun_dict[[api]]
    fun(url_body=url_body, use_curl=use_curl, ...)
}

#' @importFrom glue glue
#' @importFrom aseskit getApiKey iif
embed_ipinfo_api_generic <- function(url_char, key=NULL, ...){
    stopifnot(is.character(url_char))

    # key is not mandatory
    if (is.null(key)) {
        key <- getApiKey('ipinfo')
    }else if (is.na(key)) {
        return(url_char)
    }
    url_char <- glue('{url_char}?token={key}')
    return(url_char)
}

#' @importFrom aseskit check_curl iif ifnull
#' @importFrom glue glue
synthesize_ipinfo_api_geohost <- function(url_body, use_curl=FALSE,...){
    # check args
    dots <- list(...)
    stopifnot(is.character(url_body))
    name_out <- ifnull(dots$name_out, url_body)
    
    # start
    api_url <- "https://ipinfo.io/"
    use_curl <- check_curl(api_url, "ipinfo geohost", use_curl)
    url_char <- glue('{api_url}{url_body}/json')
    url_char <- embed_ipinfo_api_generic(url_char, ...)
    
    return(mapply(structure, url_char, name_out=name_out, MoreArgs=list(
        class=c('ipinfo_api_geohost', 'url'), use_curl=use_curl), 
        SIMPLIFY=FALSE))
}
