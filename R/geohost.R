#' IP address lookup
#'
#' geocodes an IP address using either ipify.org (\url{https://www.ipify.org}), 
#' ipstack.com (\url{https://ipstack.com}) or ipinfo.io (\url{http://ipinfo.io/developers}) 
#' IP lookup API.
#'
#' @param ip a character vector specifying IPs (IPv4 or IPv6) (e.g., "12.215.42.19").
#' The default value is no IP is specified and the host IP is used.
#' @param api use 'ipify' (ipify.org), 'ipstack' (ipstack.com) or 'ipinfo'(ipinfo.io) 
#' lookup API. By default 'ipinfo' is used.
#' @param output character, 'country', 'region', 'city', 'all' or 'raw'. It decides
#' which columns to return in the output data.frame. \itemize{
#'  \item 'country': ip and country \cr
#'  \item 'region': ip, region and country \cr
#'  \item 'city': ip, city, region and country \cr
#'  \item 'all': all the info \cr
#'  \item 'raw': the raw JSON list. You can then apply \code{\link{parse_geohosts}}
#'  to process the list for structured data.
#' }
#' @param use_curl logical, whether use curl to access the APIs. Default FALSE.
#' @param time numeric, time interval to lookup the IP address, by seconds. It is
#' used to avoid overuse of the APIs. Default 0.
#' @param key character, the api key for ipinfo or ipstack. Default NULL, which
#' indicates that the function will try to find it in cache. When no match is 
#' found, a GUI wizard will be launched for you to enter the key. If the API does
#' not call for a key, set it to NA.
#'
#' @return a data.frame.
#' @author Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), PhD student from
#' Center for Earth System Science, Tsinghua University \cr
#' Yiying Wang (\email{wangy@@aetna.com})
#' @details note that ipstack.com API is allowed up to 10,000 queries per month
#' by default, ipinfo API is limited to 1,000 requests per day, and ipify.org API
#' is limited to 1,000 requests per month.
#' @seealso \code{\link{geocode}}, \code{\link{revgeocode}}, \code{\link{parse_geohosts}}.
#' @references \itemize{
#'  \item ipipy.org IP lookup API at \url{https://www.ipify.org} \cr
#'  \item ipstack.com IP lookup API at \url{https://ipstack.com/documentation} \cr
#'  \item ipinfo.io IP lookup API at \url{https://ipinfo.io/developers}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # geocode host IP
#' geohost()
#' geohost(api = "ipinfo")
#' 
#' # specify an IP for geocoding
#' geohost(ip = "12.215.42.19")
#' geohost(ip = "12.215.42.19", api = "ipinfo")
#' 
#' # geocode multiple IPs
#' geohost(ip = c("61.135.169.81", "12.215.42.19"))
#' geohost(ip = c("61.135.169.81", "12.215.42.19"), api = "ipinfo")
#' }
geohost <- function(ip='', api=c('ipinfo', 'ipify', 'ipstack'), 
                    output = c('country', 'region', 'city', 'all', 'raw'), 
                    use_curl=FALSE, time=0, key=NULL) {
    # check arguments
    stopifnot(is.character(ip))
    stopifnot(is.logical(use_curl))
    api <- match.arg(api)
    output <- match.arg(output)
    
    check_my_ip()  # reuse options()[['my_ip]]
    if (identical(ip, '')) ip <- options()[["my_ip"]]
    ip <- ip[validate_ip(ip)]
    
    # API-specific functions
    geohost_api <- list(ipinfo  = geohost_ipinfo_api,
                        ipify   = geohost_ipify_api,
                        ipstack = geohost_ipstack_api)
    fun <- geohost_api[[api]]
    fun(ip=ip, output=output, use_curl=use_curl, time=time, key=key)
}


#' Parse geohost API results
#' 
#' This is a wrapper of API result parsers for ipify, ipinfo and ipstack. 
#' If \code{\link{geohost}} yielded 'raw' results, you can then convert the 
#' output lists to a structured data using this function.
#' @param ghlst a list comprising of lists of class 'ipify_geohost', 'ipinfo_geohost'
#'  or 'ipstack_geohost'.
#' @param output character, 'country', 'region', 'city', 'all', or 'raw'. Refer to
#' \code{\link{geohost}}.
#' @param ... other arguments to pass to the function
#'
#' @return a \code{\link[tibble]{tibble}}
#' @importFrom dplyr bind_rows as_tibble

#' @export
#' @aliases parse_geohost
#' 
#' @seealso \code{\link{parse_api_data}}, \code{\link{geohost}}, 
#' \code{\link{geocode}}, \code{\link{parse_geocode}};
#' \code{\link{revgeocode}}, \code{\link{parse_revgeocode}}
#' 
#' @examples
#' \dontrun{
#' ghlst <- geohost(c('<IP1>', '<IP2>'), output='raw')  # yield a list
#' parse_geohosts(ghlst) 
#' }
parse_geohosts <- function(ghlst, output=c('country', 'region', 'city', 'all'),
                           ...){
    output <- match.arg(output)
    name_out <- names(ghlst)
    out <- mapply(parse_geohost_result, gh=ghlst, name_out=name_out, 
                  MoreArgs=list(output=output, ...), SIMPLIFY=FALSE) %>% 
        bind_rows %>% as_tibble
    
    out_dict <- list(
        country = c('ip', 'country'),
        region = c('ip', 'region', 'country'),
        city = c('ip', 'city', 'region', 'country'),
        all = names(out)
    )
    return(out[out_dict[[output]]])
}

# work funciton for parse_geohosts
parse_geohost_result <- function(gh, ...) {
    stopifnot(inherits(gh, 'api_data'))
    UseMethod(".parse_geohost_result", gh)
}

#' @importFrom aseskit iif ifnull
.parse_geohost_result.ipify_geohost <- function(
    gh, output=c('country', 'region', 'city', 'all'), name_out=NULL, ...){
    
    output <- match.arg(output)
    name_out <- ifnull(name_out, attr(gh, 'name_out'))
    
    # fail?
    if (length(gh) == 0 || identical(unclass(gh), 'ACCESS RESTRICTED')){
        o <- data.frame(ip=NA_character_, city=NA_character_, region=NA_character_,
                        country=NA_character_)
        if (length(gh) > 0) if (identical(unclass(gh), 'ACCESS RESTRICTED'))
            warning(paste0('geohost failed with , ip = "', gc_name, 
                           '". see more details in the response code ',
                           'table of ipify API'),
                    call. = FALSE)
    }else{
        o <- data.frame(ip=gh$ip, as.data.frame(gh$location, stringsAsFactors=FALSE)) %>% 
            mutate(lat=as.numeric(lat), lng=as.numeric(lng))
    }
    
    return(o)
}

#' @importFrom aseskit iif ifnull
#' @importFrom tidyr separate
.parse_geohost_result.ipinfo_geohost <- function(
    gh, output=c('country', 'region', 'city', 'all'), name_out=NULL, ...){
    
    output <- match.arg(output)
    name_out <- ifnull(name_out, attr(gh, 'name_out'))
    
    o <- gh %>% unclass %>% as.list %>% as.data.frame(stringsAsFactors=FALSE)
    o <- separate(o, loc, into=c('lat', 'lng'), sep=',', convert=TRUE, fill='left')
    return(o)
}

#' @importFrom aseskit iif ifnull
#' @importFrom dplyr rename
.parse_geohost_result.ipstack_geohost <- function(
    gh, output=c('country', 'region', 'city', 'all'), name_out=NULL, ...){
    
    if (length(gh) == 0 || 'error' %in% names(gh)){
        o <- data.frame(ip=NA_character_, city=NA_character_, region=NA_character_,
                        country=NA_character_)
        if (length(gh) > 0) if ('error' %in% names(gh))
            warning(paste0('geohost failed with status code ', gh$error$code, 
                           ' (', gc$error$type, ', ', gc$error$info, ') , ip = "', 
                           gc_name, '". see more details in the response code ', 
                           'table of ipstack API'),
                    call. = FALSE)
    }else{
        o <- as.data.frame(ifnull(gh[1:(length(gh)-1)], NA_character_), 
                           stringsAsFactors=FALSE)
        loc <- ifnull(gh$location, NA_character_)
        o <- o %>% 
            mutate(capital=loc$capital, language=loc$languages$code,
                   language_name=loc$languages$name, 
                   language_native=loc$languages$native,
                   country_flag=loc$country_flag) %>% 
            rename(country=country_code, region=region_code, lat=latitude,
                   lng=longitude)
    }
    return(o)
}

.parse_geohost_result.default <- function(
    gh, output=c('country', 'region', 'city', 'all'), ...){
    stop('parse_geohost_result fails. gh should be of subclasses of "api_data", ', 
         'e.g., xxx_geohost, typically yielded using aseskit::get_api_data() on an "url" ',
         'object.')
}

# work functions
#' @importFrom aseskit get_api_data synthesize_api
geohost_ipinfo_api <- function(ip, output=c('country', 'region', 'city', 'all', 'raw'), 
                               time=0, use_curl=FALSE, key=NULL, ...){
    # check args
    stopifnot(is.character(ip))
    stopifnot(is.numeric(time))
    output <- match.arg(output)
    
    # synthesize urls
    urls <- synthesize_api(ip, provider="ipinfo", api='geohost', key=key)
    
    # read api
    ghlst <- get_api_data(urls, use_curl=use_curl, time=time, name_out=ip)
    
    # parse result
    if (output == 'raw') return(ghlst)
    return(parse_geohosts(ghlst, output=output))
}

# work functions
#' @importFrom aseskit get_api_data synthesize_api
geohost_ipify_api <- function(ip, output=c('country', 'region', 'city', 'all', 'raw'), 
                              time=0, use_curl=FALSE, key=NULL, ...){
    # check args
    stopifnot(is.character(ip))
    stopifnot(is.numeric(time))
    output <- match.arg(output)
    
    # synthesize urls
    urls <- synthesize_api(ip, provider="ipify", api='geohost', key=key)
    
    # read api
    ghlst <- get_api_data(urls, use_curl=use_curl, time=time, name_out=ip)
    
    # parse result
    if (output == 'raw') return(ghlst)
    parse_geohosts(ghlst, output=output)
}

# work functions
#' @importFrom aseskit get_api_data synthesize_api
geohost_ipstack_api <- function(ip, output=c('country', 'region', 'city', 'all', 'raw'), 
                                time=0, use_curl=FALSE, key=NULL, ...){
    # check args
    stopifnot(is.character(ip))
    stopifnot(is.numeric(time))
    output <- match.arg(output)

    # synthesize urls
    urls <- synthesize_api(ip, provider="ipstack", api='geohost', key=key)
    
    # read api
    ghlst <- get_api_data(urls, use_curl=use_curl, time=time, name_out=ip)
    
    # parse result
    if (output == 'raw') return(ghlst)
    parse_geohosts(ghlst, output=output)
}


# ------------Misc functions----------------

parse_getip_result <- function(ip, ...) {
    stopifnot(inherits(ip, 'api_data'))
    UseMethod(".parse_getip_result", ip)
}

#' @importFrom aseskit iif ifnull
.parse_getip_result.ipify_getip <- function(
    ip, output=c('all'), name_out=NULL, ...){
    
    output <- match.arg(output)
    name_out <- ifnull(name_out, attr(ip, 'name_out'))
    
    o <- ip[[1]]$ip
    
    return(o)
}

#' @importFrom aseskit get_api_data synthesize_api iif ifnull
check_my_ip <- function(overide=FALSE, lifecycle=3600){
    # update options('my_ip')
    # return nothing
    stopifnot(is.logical(overide))
    stopifnot(is.numeric(lifecycle))
    
    my_ip_updated <- attr(getOption("my_ip"), "updated")
    if (! is.character(getOption("my_ip")) || 
        (! overide && Sys.time() - ifnull(my_ip_updated, 0) > lifecycle)){
        # my_ip no set or updated > an hour ago
        url_char <- synthesize_api('', provider='ipify', api='getip', name_out='my_ip')
        ip_data <- get_api_data(url_char, use_curl=FALSE)
        options(my_ip = structure(ip_data[[1]][["ip"]], updated=Sys.time()))
    }
    invisible()
}

#' Display my own IP address
#' 
#' Call ipify.org API to detect my own IP address. If you cannot get access to
#' the API, NULL will be returned.
#'
#' @return character, current IP address. If fails, returns NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' show_my_ip()
#' }
show_my_ip <- function(){
    check_my_ip()
    tryCatch(as.vector(getOption('my_ip')), error=function(e) NULL)
}

# option ip.country to store IP country and avoid calling geohost() repeatedly
# when geocoding multiple addresses or revgeocoding multiple locations
ip.country <- function(api=c("ipinfo", "ipify", "ipstack")){
    api <- match.arg(api)
    if (! "ip.country" %in% names(options())) {
        ip_data <- geohost(api = api)
        options(ip.country = unname(ip_data[["country"]]))
    }
    getOption("ip.country")
}

