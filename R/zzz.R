#' asesgeo: An R Geographic Analytics Toolkit for ASES
#'
#' An analytic toolkit comprising of a series of work functions on geographics,
#' e.g., coordinate conversion for Chinese coordinates/addresses, IP parsing, ...
#' 
#' @details This package is comprised of \cr \describe{
#'   \item{Coordinate conversion}{\itemize{
#'     \item Local tranformer: \code{\link{wgs2gcj}()}, \code{\link{wgs2bd}()}, 
#'       \code{\link{gcj2wgs}()}, \code{\link{gcj2bd}()}, \code{\link{bd2wgs}()}, 
#'       \code{\link{bd2gcj}()}
#'     \item Generic wrapper: \code{\link{conv_coord}()}
#'   }}
#'   \item{Geohost (ip parser)}{\code{\link{geohost}()} or \code{\link{parse_geohost}()}}
#'   \item{Geocode (address locator)}{\code{\link{geocode}()} or \code{\link{parse_geocode}()}}
#'   \item{Reverse geocode (coordinate locator)}{\code{\link{revgeocode}()} or 
#'     \code{\link{parse_revgeocode}()}}
#'   \item{Helpers}{\itemize{
#'     \item Formatter: \code{\link{coord_format}}, \code{\link{lat_coord}()}, 
#'       \code{\link{lon_coord}()}
#'     \item Inside or outside China: \code{\link{is_out_of_china}()}
#'     \item Show my IP: \code{\link{show_my_ip}()}     
#'   }}
#' }
#' @author \strong{Creator, Maintainer}: Yiying Wang, \email{wangy@@aetna.com}
#' 
#' @importFrom magrittr %>%
#' @export %>%
#' @docType package
#' @keywords internal
#' @seealso \pkg{\link{aseskit}}
#' @name asesgeo
NULL

#' @importFrom aseskit addRtoolsPath
.onLoad <- function(libname, pkgname="asesgeo"){
    
    if (Sys.info()[['sysname']] == 'Windows'){
        Sys.setlocale('LC_CTYPE', 'Chs')
    }else{
        Sys.setlocale('LC_CTYPE', 'zh_CN.utf-8')
    }
    if (Sys.info()[['machine']] == "x64") if (Sys.getenv("JAVA_HOME") != "")
        Sys.setenv(JAVA_HOME="")
    
	addRtoolsPath()
	
    # pkgenv is a hidden env under pacakge:asesgeo
    # -----------------------------------------------------------
    assign("pkgenv", new.env(), envir=parent.env(environment()))
    
    # constants for coord conversion
    ## Krasovsky 1940 ellipsoid parameters
    ## semi-major axis
    pkgenv$A <- 6378245.0
    # F = 1 / 298.3
    # B = A * (1 - F)
    # EE = (A^2 - B^2) / A^2
    pkgenv$EE <- 0.00669342162296594323
    pkgenv$XM_PI <- pi * 3000.0 / 180.0
    
    # ----------------------------------------------------------
    
    # options
    assign("op", options(), envir=pkgenv)
    
    options(stringsAsFactors=FALSE)
    pkgParam <- aseskit:::.getPkgPara(pkgname)	
    toset <- !(names(pkgParam) %in% names(pkgenv$op))
    if (any(toset)) options(pkgParam[toset])
}


.onUnload <- function(libname, pkgname="asesgeo"){
    op <- aseskit:::.resetPkgPara(pkgname)
    options(op)    
}


.onAttach <- function(libname, pkgname="asesgeo"){
    ver.warn <- ""
    latest.ver <- getOption(pkgname)$latest.version
    current.ver <- getOption(pkgname)$version
    if (!is.null(latest.ver) && !is.null(current.ver))
        if (latest.ver > current.ver)
            ver.warn <- paste0("\nThe most up-to-date version of ", pkgname, " is ",
			                   latest.ver, ". You are currently using ", current.ver)
    packageStartupMessage(paste("Welcome to", pkgname, current.ver, 
                                 ver.warn))
}

