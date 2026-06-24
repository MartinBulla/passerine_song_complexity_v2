# ==========================================================================
# ❗ The script runs relative to the project's root directory,
# uses rangeMapper sqlite database and breeding ranges from published sources
# (see the manuscript for references) to estimate center of the breeding
# range and export its latitude and longitude for each species as
# `DAT_range_center.csv` to ./Data/.
# ==========================================================================

# Packages, settings
source(here::here("R/__init__.R"))
# package sdb should be installed from https://github.com/ornitho-logics/sdb

sapply(
    c(
        "dplyr", "exactextractr", "fasterize", "ggplot2", "lwgeom","proj4", "rangeMapper", "rmapshaper", "rnaturalearth", "rnaturalearthdata", "qs",
        "raster", "tidygeocoder", "tidyverse","units", "sdb", "sf", "sfheaders", "stars"
    ),
    function(x) suppressPackageStartupMessages(using(x))
)

plot_ = TRUE

# DATA
# world map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
world_eck4 <- st_transform(world, projeck4)

# load breeding ranges
con <- rmap_connect(path = here("Data/DAT_rangeMapper.sqlite"))
br <- rmap_to_sf(con, "wkt_ranges")
br <- mutate(br, scinam = str_replace(bio_id, "_", " ")) |> dplyr::select(-bio_id)

# EXTRACT coordinates for center of the breeding ranges
y <- br[, "scinam"] # all species names
#O <- foreach(i = 1:nrow(y), .errorhandling = "pass") %dopar% {
lat_lon = data.table(pk = integer(), scinam = character(), lat = numeric(), lon = numeric())
#lat_lon <- foreach(i = 1:nrow(y), .combine = rbind) %dopar% {
#lat_lon <- foreach(i = 57:62, .combine = rbind) %dopar% {
#O <- foreach(i = 1:3, .errorhandling = "pass") %dopar% {
for(i in 1:nrow(y)){
    sci <- y[i, ]$scinam # sp name i;
    # sci = "geothlypis aequinoctialis"#"anthus rubescens" "anthus cervinus"
    # i = 1686
    rangei <- br[i, ] # range i ( x is a data.table that contains all ranges)
    # rangei<- br[br$scinam %in% sci, ]

    borderi <- st_cast(rangei, "MULTILINESTRING") # convert range to lines

    # how many points you are gonna spread across your range. More points, the finer resolution.
    if (sci %in% c("acrocephalus familiaris", "philesturnus carunculatus","myiagra azureocapilla")) {
        ni = 50
    } else {
        ni = 1000
    }
    #  acrocephalus familiaris (two tiny island in Hawaiian Archipelago), philesturnus carunculatus (mini habitat in New Zealand), myiagra azureocapilla (Taveuni) have very small breeding ranges and hence run only if less points sampled

    gri <- st_sample(rangei, ni, type = "regular", exact = FALSE) # sample 'ni' many points at uniform grid over your range

    # 'type' options: random, hexagonal (triangular), regular, or spatstat methods
    # 'exact': "should the length of output be exactly the same as specified by size?
    #  Only applies to polygons and when 'type'="random"

    # if x has dimension 2 (polygons) and geographical coordinates (long/lat),
    # uniform random sampling on the sphere is applied.

    # For regular or hexagonal sampling of polygons, the resulting size is only an approximation.

    df <- data.table(dist = as.vector(st_distance(borderi, gri)), st_coordinates(gri)) # for each sample point, get the distance to the nearest border

    xy = st_as_sf(data.table(scinam = sci, lon = as.numeric(df[dist == max(dist), .(X)][1]), lat = as.numeric(df[dist == max(dist), .(Y)][1])), coords = c("lon", "lat"), crs = projeck4) # get the center of the breeding range in Eckert4 projection

    xy_ = as.numeric(st_coordinates(st_transform(xy, projll))) # lat/lon in degrees

    if(plot_ == TRUE){
    g =
    ggplot(data = world_eck4) +
        geom_sf(fill = "lightgrey", col = "lightgrey") +
        geom_sf(data = rangei, size = .1, col = "#0e3b4a", fill = "#0e3b4a") +
        geom_sf(data = xy, col = "red", fill = "red") +
        # coord_sf(xlim = c(-179.999999, 179.999999)) +
        labs(subtitle = sci, x = NULL, y = NULL) +
        theme_bw()
    ggsave(file = paste0("Output/maps/", sci, ".png"), g, width = 10 * 2, height = 5 * 2, units = "cm")
    }
    a = data.table(pk = i, scinam = sci, lat = xy_[2], lon = xy_[1])
    print(a)
    lat_lon = rbind(lat_lon, a)
    #return(b)
}

# export
fwrite(file = "Data/DAT_range_center.csv", lat_lon)

# lat_lon <- fread(file = "Data/DAT_range_center.csv")

# plot better ranges crossing meridian (e.g. anthus rubescens) - not working because of issues with gdal
#rangei_ll <- st_transform(rangei, crs = projll)
#rangei_ll_w <- st_wrap_dateline(rangei_ll, options = c("WRAPDATELINE=YES")) #option <- c("WRAPDATELINE=YES", "DATELINEOFFSET='20'")


# testing 1 - species that initially did not run because of mini breeding range - DONE
#lat_lon[, pk := 1:nrow(lat_lon)]
#lat_lon[is.na(lat), pk]
#sp = data.table(scinam = unique(br$scinam))
#sp[, pk := 1:nrow(sp)]
#sp[pk %in% lat_lon[is.na(lat), pk]] #  acrocephalus familiaris, philesturnus carunculatus = need less sampling point

# testing 2 - get plot for species crossing meridaina (anthus rubescens, etc) without bug lines
# st_transform(st_as_sf(data.table(lon = c(-179.999999, 179.999999), lat = 10), coords = c("lon", "lat"), crs = projll), projeck4) # get lon = c(-179.999999, 179.999999) in Eckert4 projection
#st_bbox(rangei)$xmin
#st_bbox(rangei)$xmax

#  ggplot(data = world) +
      #geom_sf(fill = "lightgrey", col = "lightgrey") + geom_sf(data = rangei, size = .1, col = "#0e3b4a", fill = "#0e3b4a") + geom_sf(data = xy, col = "red", fill = "red") +
      #coord_sf(xlim = c(-16818515, 16818515), crs = projeck4) +
      #labs(subtitle = sci, x = NULL, y = NULL) +
      #theme_bw()

# doesn't work becaus of gdal issues In CPL_wrap_dateline(x, options, quiet): GDAL Error 6: GEOS support not enabled.

#rangeii <- st_transform(rangei, projll)
#rangeiii <- st_shift_longitude(rangeii)
#xyii <- st_shift_longitude(st_transform(xy, projll))
#xyiii <- st_shift_longitude(st_transform(xy, projll))
#worldii = st_shift_longitude(world)
#ggplot(data = worldii) +
    #geom_sf(fill = "lightgrey", col = "lightgrey") +
    #geom_sf(data = rangeiii, size = .1, col = "#0e3b4a", fill = "#0e3b4a") +
    #geom_sf(data = xyiii, col = "red", fill = "red") +
    #labs(subtitle = sci, x = NULL, y = NULL) +
    #theme_bw()
#ggplot(data = world) +
 #   geom_sf(fill = "lightgrey", col = "lightgrey") +
  #  geom_sf(data = rangeii, size = .1, col = "#0e3b4a", fill = "#0e3b4a") +
   # geom_sf(data = xyii, col = "red", fill = "red") +
    #labs(subtitle = sci, x = NULL, y = NULL) +
    #coord_sf(xlim = c(-180, 180)) +
    #theme_bw()
# END
