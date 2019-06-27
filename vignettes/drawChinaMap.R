## ----setup, include=FALSE------------------------------------------------
Sys.setlocale("LC_CTYPE", 'Chs')
knitr::opts_chunk$set(fig.width=7, fig.height=5.5)

## ----UDF, include=FALSE--------------------------------------------------
capOpt <- function(fig=c('Figure ', 'Fig.', 'Fig ', '图'),
                   tab=c('Table ', 'Tab.', 'Tab ', '表'),
                   sep=c('-', '.'), chap=0, sect=0, use.head=1){
    fig <- match.arg(fig)
    tab <- match.arg(tab)
    sep <- match.arg(sep)
    return(list(fig=fig, tab=tab, sep=sep, chap=chap, sect=sect, use.head=use.head))
}

cap <- capOpt(fig='Figure ', tab='Table ', sep='-', chap=1, sect=1, use.head=2)

fn = local({  ## counter for figures
    i = 0
    function(x, renum = c(-1, 0, 1, 2), use.head=cap$use.head, reset.base=1,
             chap=NULL, sect=NULL) {
        ## Calc title numbers
        ##  x: chart title
        ##  renum: if -1/0, no effect; 1, reset chap and i; 2, reset sect and i.
        ##  use.head: if -1/0, 'Table 1'; if 1, 'Table 1-1'; if 2, 'Table 1-1-1'
        ##  reset.base: if reset chap/sect, reset to this value. default 1.
        ##  chap: explicitly declare the chap value (overwrite cap$chap). default NULL.
        ##  sect: explicitly declare the sect value (overwrite cap$sect). default NULL.
        renum = renum[[1]]
        use.head = use.head[[1]]
        stopifnot(renum >= -1 && renum <= 2)
        stopifnot(use.head >= -1 && use.head <= 2)
        stopifnot(is.null(chap) || is.numeric(chap))
        stopifnot(is.null(sect) || is.numeric(sect))
        
        if (!is.null(chap)) cap$chap <<- chap -1 
        if (!is.null(sect)) cap$sect <<- sect -1 
        
        if (renum > use.head) renum <- use.head
        if (renum > 0) { 
            i <<- 0
            if (renum == 1) {
                cap$chap <<- cap$chap + 1
                cap$sect <<- reset.base
            }
            if (renum == 2) {
                cap$sect <<- cap$sect + 1
            }
        }
        i <<- i + 1
        if (use.head == 1) cap.head <- paste0(cap$chap, cap$sep)
        if (use.head == 2) cap.head <- paste0(cap$chap, cap$sep, cap$sect, cap$sep)
        paste0('<b>', cap$fig, cap.head, i, '</b>: ', x)
    }
})
tn = local( {  ## counter for tables
    j = 0
    function(x, renum = c(-1, 0, 1, 2), use.head = cap$use.head, reset.base=1,
             chap=NULL, sect=NULL) {
        renum = renum[[1]]
        use.head = use.head[[1]]
        stopifnot(renum >= -1 && renum <= 2)
        stopifnot(use.head >= -1 && use.head <= 2)
        stopifnot(is.null(chap) || is.numeric(chap))
        stopifnot(is.null(sect) || is.numeric(sect))
        
        if (!is.null(chap)) cap$chap <<- chap -1 
        if (!is.null(sect)) cap$sect <<- sect -1 
        
        if (renum > use.head) renum <- use.head
        if (renum > 0) {
            j <<- 0
            if (renum == 1) {
                cap$chap <<- cap$chap + 1
                cap$sect <<- reset.base
            }
            if (renum == 2) {
                cap$sect <<- cap$sect + 1
            }
        }
        j <<- j + 1
        if (use.head == 1) cap.head <- paste0(cap$chap, cap$sep)
        if (use.head == 2) cap.head <- paste0(cap$chap, cap$sep, cap$sect, cap$sep)
        paste0('<b>', cap$tab, cap.head, j, '</b>: ', x)
    }
})

## ----mapL1 str, echo=FALSE, results='markup'-----------------------------
invisible(library(asesgeo))
lapply(structure(list(chnmap0, chnmap1, chnmap2, chnmap3), 
                 names=c("Nation", "Province", "Prefecture", "County")), 
       function(obj) {
         str(obj@data)
         invisible()
       })

## ----L0 basemap map, fig.cap=fn('China Base Map by `map` (Nation Level)', 1, chap=2)----
library(maps)
op <- par(mar=c(0, 0, 0, 0))
map(cnmap0(), projection="albers", parameters=c(24, 47))
par(op)

## ----L0 basemap geom_map, fig.cap=fn('China Base Map by `geom_map` (Nation Level)')----
library(ggplot2)
map0 <- fortify(cnmap0())
ggplot(data=map0, aes(map_id=id)) + 
    geom_map(map=map0, fill='white', colour='lightgray') +
    expand_limits(x=map0$long, y=map0$lat) +
    coord_map(projection="albers", parameters=c(24, 47))

## ----L0 basemap geom_sf, fig.cap=fn('China Base Map by `geom_sf` (Nation Level)')----
chn_crs <- "+init=epsg:4490 +proj=laea +ellps=GRS80 +lon_0=105 +lat_0=30"
library(sf)
map0 <- st_as_sf(cnmap0())
ggplot() + 
    geom_sf(data=map0, fill='white', colour='lightgray') +
    coord_sf(crs=chn_crs)

## ----L1 basemap with lab geom_polygon, fig.cap=fn('China Base Map by `geom_polygon` (Province Level)', 2)----
library(extrafont)
map1 <- fortify(cnmap1())
meta1 <- cnmap1()@data
ggplot() + 
    geom_polygon(aes(x=long, y=lat, group=group), data=map1, fill='white', 
                 colour='lightgray') +
    geom_text(aes(PTX, PTY, label=NAME_LAB), data=meta1, colour='gray',
              family="Microsoft YaHei") +
    coord_map(projection="albers", parameters=c(24, 47))

## ----L1 basemap with lab geom_sf, fig.cap=fn('China Base Map by `geom_sf` (Province Level)')----
map1 <- st_as_sf(cnmap1())
ggplot() + 
    geom_sf(data=map1, fill='white', colour='lightgray') +
    geom_sf_text(aes(label=NAME_LAB), data=map1, colour='gray',
                 family="Microsoft YaHei") +
    coord_sf(crs=chn_crs)

## ----L2 basemap geom_sf, fig.cap=fn('China Base Map (City Level)', 2)----
map2 <- st_as_sf(cnmap2())
ggplot() + 
    geom_sf(data=map2, fill='white', colour='lightgray') +
    coord_sf(crs=chn_crs)

## ----L3 basemap geom_sf, fig.cap=fn('China Base Map (County Level)', 2)----
map3 <- st_as_sf(cnmap3())
ggplot() + 
    geom_sf(data=map3, fill='white', colour='lightgray') +
    coord_sf(crs=chn_crs)

## ----mixed basemap, fig.cap=fn('China Base Map - Mixed Levels', 2)-------
ggplot() + 
    geom_sf(data=map3, fill='white', colour='grey85') +
    geom_sf(data=map2, fill='transparent', colour='grey60') +
    geom_sf(data=map1, fill='transparent', colour='grey30') +
    geom_sf(data=map0, fill='transparent', colour='grey5') +
    coord_sf(crs=chn_crs)

## ----get adcode of GD, HK, MC, fig.cap=tn('ADCODE of GD, HK & MC')-------
suppressWarnings(library(dplyr))
knitr::kable(cnmap1()@data %>% filter(NAME_EN %in% c("Guangdong", "Hong Kong", "Macau")) %>% 
    select(NAME, NAME_EN, ADCODE), caption=tn('ADCODE of GD, HK & MC', 2, sect=6))

## ----sub basemap, fig.cap=fn('China Base Map with Labels - GD, HK & MC', 2)----
submap1 <- st_as_sf(cnmap1(regions=c("^44", "^8[12]")))
submap2 <- st_as_sf(cnmap2(regions=c("^44", "^8[12]")))
ggplot() +
    geom_sf(data=submap2, fill="white", color="gray") +
    geom_sf(data=submap1, fill="transparent", color="darkgray") +
    geom_sf_text(aes(label=NAME_LAB), data=submap2, colour='darkgray',
                 family="Microsoft YaHei") +
    coord_sf(crs=chn_crs)

## ----pop data, fig.cap=fn('Big Cities of China on Cartesian Coordinate System', 1)----
data('world.cities')
big.cities <- world.cities[world.cities$country.etc %in% c('China', 'Taiwan'), ]
ggplot() + 
    geom_point(aes(x=long, y=lat, size=pop), data=big.cities, color='red', 
               alpha=0.3)

## ----scatter on basemap, fig.cap=fn('Big Cities of China')---------------
big.cities$capital <- as.factor(big.cities$capital)
levels(big.cities$capital) <- c(
    'City', 'Capital', 'Municipality', 'Province capital')
big.cities <- st_as_sf(
    big.cities, coords=c("long", "lat"), crs="+init=epsg:4326", agr="constant")
ggplot() + 
    geom_sf(data=map1, fill='white', color='gray85', size=0.2) +
    geom_sf(aes(size=pop, color=capital), data=big.cities, shape=19,
            alpha=0.5, show.legend="point") + 
    coord_sf(crs=chn_crs)


## ----simplify the map----------------------------------------------------
map1_simp1 <- cnmap1(simplify_level=0.05)
map1_simp2 <- cnmap1(simplify_level=0.05, drop_fragment=TRUE)

size_orig <- object.size(cnmap1()) / 1024^2
size_simp1 <- object.size(map1_simp1) / 1024^2
size_simp2 <- object.size(map1_simp2) / 1024^2

## ----simplify the map 1, fig.cap=fn('Simplified China Map (Retain Islands)', 1)----
ggplot() + 
    geom_sf(data=st_as_sf(map1_simp1), fill='white', colour='lightgray') +
    coord_sf(crs=chn_crs)

## ----simplify the map 2, fig.cap=fn('Simplified China Map (Drop Islands)')----
ggplot() + 
    geom_sf(data=st_as_sf(map1_simp2), fill='white', colour='lightgray') +
    coord_sf(crs=chn_crs)

## ----3d scatter, fig.cap=fn('Big Cities of China - Shadow 3D', 2)--------
# translate the geometry of map0 by 1 degree west and 0.5 degree south
map0_geom_translated <- st_geometry(map0) + c(-1, -0.5)
# apply the tranlated geometry
map0_bg <- st_set_geometry(map0, map0_geom_translated) %>% 
    st_set_crs(st_crs(map0))
# add the translated geometry layer
ggplot() + 
    geom_sf(data=map0_bg, fill='grey50', color='gray50', size=0.2) +
    geom_sf(data=map1, fill='white', color='gray85', size=0.2) +
    geom_sf(aes(size=pop, color=capital), data=big.cities, shape=19,
            alpha=0.5, show.legend="point") + 
    coord_sf(crs=chn_crs)

## ----choropleth geom_polygon, fig.cap=fn('Size of China Cities using `geom_polygon`', 1)----
area.map <- fortify(cnmap2()) %>% left_join(
    cnmap2()@data %>% select(NAME, AREA, ID), by=c("id"="ID"))
ggplot() +
    geom_polygon(aes(x=long, y=lat, group=group, fill=AREA), data=area.map, 
                 color='white', size=0.05) +
    scale_fill_gradient(low='skyblue', high='darkgreen') +
    coord_map('albers', parameters=c(24, 47)) 

## ----choropleth geom_sf, fig.cap=fn('Size of China Cities using `geom_sf`', 2)----
ggplot() +
    geom_sf(aes(fill=AREA), data=map2, color='white', size=0.05) +
    scale_fill_gradient(low='skyblue', high='darkgreen') +
    coord_sf(crs=chn_crs) 

