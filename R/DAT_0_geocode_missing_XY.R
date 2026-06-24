# ==========================================================================
# ❗ The script runs relative to the project's root directory and
#  extracts missing geo-coordinates using country and site information and
#  exports them into .Data/DAT_song_latlon_geocode.csv.
# ==========================================================================

# PACKAGES, SETTINGS
  source(here::here('R/__init__.R'))
  sapply(c('httr','magrittr','tidygeocoder'),
         FUN=require,  character.only = TRUE, quietly = TRUE)

  set_config(config(ssl_verifypeer = 0L))

# COLLECT: missing latit, longit in song_raw.csv
  d = fread(here::here('Data/song_raw.csv'))[is.na(latit), .(country, site)]  %>% unique
  d[, i := .I]

# RUN geocode
  o = d[,
    {
      print(i)
      geo(paste(site, country, sep = ","), method = "osm")
    },
    by = i
  ]

# EXPORT
  fwrite(oo[!is.na(lat)][!duplicated(address)], here("Data/DAT_song_latlon_geocode.csv"))
