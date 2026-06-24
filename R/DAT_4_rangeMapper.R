# ==========================================================================
# ❗ This script is provided as reference only. It contains links to
# the internal database of the Max Planck Institute's Lab of Ornithology.
# The script runs relative to the project's root directory, gathers data
# from published sources (see the manuscript for references) and exports
# the collected data as ./Data/DAT_rangeMapper.sqlite (per grid cell data for
# species in our dataset) and ./Data/DAT_rangeMapper_all.sqlite (for all
# passerine species).
# ==========================================================================

# PACKAGES, SETTINGS
  source(here::here("R/__init__.R")) # source( here::here('R/__init__comp.R'))

  sapply(c('doFuture', 'dplyr', 'fasterize', 'rangeMapper', 'raster', 'qs', 'sdb', 'sf',  "units"),
           FUN=require,  character.only = TRUE, quietly = TRUE)
  require(ggplot2)
  require(ggpubr)
  #con = dbcon("mbulla", db = "AVES_taxonomy")
  #con_t <- dbcon(db = "AVES_taxonomy")
  #closeCon(con_r)

# WORLD MAP
    M <- rnaturalearth::ne_countries(returnclass = "sf", scale = "small") |>
      sf::st_transform(projeck4) |>
      rmapshaper::ms_dissolve()

#! SETTINGS parallel support
  registerDoFuture()
  if( supportsMulticore())
    plan(multicore) else
      plan(multisession)

# get synonyms
  sy <- fread(here::here("Data/DAT_taxa_synonyms.csv")) #   con <- dbcon("mbulla", db = "AVES_taxonomy"); sy = dbq(con, q = 'select scinam, syid FROM AVES_taxonomy.synonyms_v2'); sy = sy[!duplicated(paste(scinam, syid))]; sy = sy[!duplicated(scinam)]; fwrite(sy, here::here("Data/taxa_synonyms.csv")) # all duplicates are true duplicates and have no synonyms associated

# MAKE for all passeriformes
## get all species with data on ranges
  con_r <- dbcon("mbulla", db = "AVES_ranges")
  tx <- dbq(con_r, "SELECT DISTINCT scinam FROM AVES_ranges.breeding_ranges_v2")
  tx$id = 1:nrow(tx)
  tx$sci_r = tx$scinam
  closeCon(con_r)

## get all passeriformes from birdtree
  con = dbcon("mbulla", db = "AVES_taxonomy")
  tss_1 <- dbq(con, q = "SELECT scinam FROM AVES_taxonomy.birdtree where order_ = 'passeriformes'") # scinam for passeriformes
  closeCon(con)

# save(file = 'Data/scinam_check.Rdata', sy, tx, tss_1)
# load('Data/scinam_check.Rdata')

## check whether no duplicated/synonym species names in birdtree
    nrow(merge(tss_1, sy, all.x = TRUE)) == nrow(tss_1)
    xx = merge(tss_1, sy, all.x = TRUE)
    nrow(xx[duplicated(syid)]) # 8 duplicated species based on synonyms

## check how many from passeriformes from birdtree present in ranges
    nrow(tss_1) # 5966 unique species in birdtree (8 duplicated based on synonym)
    nrow(tx[tx$scinam %in% tss_1$scinam, ]) # for 5948 species ranges under the same name

## get additional species for ranges where species names differ in birdtree and ranges data
    tes_1 <- copy(tss_1)
    tes_1[, song_id := 1:nrow(tes_1)]
    x1 <- tes_1[!scinam %in% tx$scinam]
    y1 <- symerge(x1, tx, sy)
    y1$.duplicates=NULL # we are happy to take ranges of all subspecies for aa given taxon

    nrow(y1[is.na(id)]) # no range entries for 8 species that are often consideredd subspecies:
    # malurus campbelli of chenorhamphus grayi,
    # terpsiphone smithii of terpsiphone rufiventer,    (nominal in),
    # eremomela salvadorii of eremomela icteropygialis  (nominal in),
    # geothlypis velata of geothlypis aequinoctialis    (nominal in),
    # zimmerius flavidifrons of zimmerius viridiflavus  (nominal in),
    # turdus daguae of turdus assimilis                 (nominal in),
    # corvus minutus of corvus palmarum                 (nominal in),
    # emberiza vincenti  of emberiza capensis           (nominal in),
    # while the subspecies have no range in the db, the nominal species (with exception of chenorhamphus grayi) do and are already within the birdtree dataset, so no need to consider

    y1 <- y1[!is.na(id)] # the species with multiple ranges (often by subsppecies)

    # test whether the subspecies overlap - they do not, so all can go in
      #sy[syid == sy[scinam == "basileuterus chlorophrys", syid]]
      #tss_1[scinam %in% sy[syid == sy[scinam == "basileuterus chlorophrys", syid], scinam]]

      for (i in unique(y1[!is.na(id), scinam])){
        #i = "xenopipo holochlora"
        print(i)
        yi = y1[!is.na(id) & scinam %in% i]

        tar <- paste(yi$sci_r %>% shQuote(), collapse = ",")

        con_r <- dbcon("mbulla", db = "AVES_ranges")
        ra = dbq(con_r, glue("SELECT scinam, taxonomy tax, SHAPE
                FROM breeding_ranges_v2 WHERE
                scinam in ({tar})"), geom = "SHAPE") # ('cryptopipo lita')
        closeCon(con_r)
          # use the following if st_wrap_dateline works and then no need to use st_crs(ra) = projll
            #st_crs(ra) <- 4326
            #ra = st_wrap_dateline(ra, options = "WRAPDATELINE=YES")
        st_crs(ra) = projll
        ra = st_transform(ra, projeck4)
        g = ggplot() +
          geom_sf(data = M, size = .1, fill = "grey95", col = NA) +
          geom_sf(data = ra, aes(fill = scinam), col = NA)+
          labs(subtitle = i)
        ggsave(file = paste0('Output/check_map_',i,'_.png'), width =10, height = 5)
      }

    y2 = data.table(scinam = 'malurus campbelli', song_id = 1239, id = 1748, sci_r = 'chenorhamphus grayi') # has a range, but under a different name and synonym was not noted

    y12 = rbind(y1,y2)
    all_sp_for_ranges <- c(tss_1[scinam %in% tx$scinam, scinam], y12[, sci_r])
    length(all_sp_for_ranges) # 5968 species

    tss_1_scinam <- paste(all_sp_for_ranges %>% unique() %>% str_replace("_", " ") %>% shQuote(), collapse = ",")

  con_r <- dbcon("mbulla", db = "AVES_ranges")
  ra = dbq(con_r, glue("SELECT scinam, taxonomy tax, SHAPE
                      FROM breeding_ranges_v2 WHERE
                      scinam in ({tss_1_scinam})"), geom = "SHAPE")
  closeCon(con_r)
  #save(file='Data/temp_BR.Rdata', ra)
  #load("Data/temp_BR.Rdata")
  st_crs(ra) <- 4326
  ra = st_wrap_dateline(ra, options = "WRAPDATELINE=YES")
  # save(file='Data/temp_BR_fixed.RData', ra)
  # load("Data/temp_BR_fixed.RData"); ra = x
  # test troublesome ranges - OK
    #ra_t = ra[ra$scinam %in% c("anthus cervinus", "anthus gustavi"),]#, "anthus rubescens", "aplonis tabuensis", "calcarius lapponicus", "clytorhynchus nigrogularis", "clytorhynchus vitiensis", "corvus corax", "ficedeula albicilla", "lalaga maculosar", "myagra azureocapilla", "myagra vanikorensis", "myzomela jugularis", "phyloscopus borealis", "plectrophenax nivalis", "turdus poliocephalus", "zosterops lateralis"), ]
    # plot(ra_t)
  #st_crs(ra) = projll
  ra = st_transform(ra, projeck4)
  ra$tax = NULL
  # CHECK
    nrow(ra) == length(all_sp_for_ranges)-1 # one was duplicated

## PREPARE rangeMapper project for all passerines
  con_rm = rmap_connect(path = here("Data/DAT_rangeMapper_all.sqlite"))
  rmap_add_ranges(con_rm, x = ra, ID = "scinam")
  rmap_prepare(con_rm, "hex", cellsize = 112500)  #~ 1deg
  rmap_save_map(con_rm)  # creates a species_richness map
  dbDisconnect(con_rm)

## test plotting
  con_m <- rmap_connect(path = here("Data/DAT_rangeMapper_all.sqlite")) # con_m <- rmap_connect(path = here("DATA/rangeMapper_all.sqlite"))
  # rmap_to_sf(con_m, c("mean_song_complexity_sub", "mean_SSD_sub", "mean_col_dimorph_sub", "mean_degree_of_polygyny_sub")) %>% summary()
  A <- rmap_to_sf(con_m)
  # summary(A)
  dbDisconnect(con_m)

  ggplot() +
  geom_sf(data = M, size = .1, fill = "grey95", col = NA) +
  geom_sf(data = A, aes(fill = species_richness), col = NA) +
  scale_fill_viridis_c()
  ggsave(file = paste0("Output/check_map_all.png"), width = 10, height = 5)


# MAKE for species in the complexity dataset

## get all aves with breeding range data
con <- dbcon("mbulla", db = "AVES_ranges")
p <- dbq(con, "SELECT DISTINCT scinam FROM AVES_ranges.breeding_ranges_v2")
closeCon(con)
#save(file='Data/temp_sci_BR.Rdata',p)
#load(file = "Data/temp_sci_BR.Rdata")
# p = tx[,.(scinam)]

## prepare for checking with complexity dataset (birdtree taxonomy)
p[, id_r := 1:nrow(p)] # p[duplicated(scinam)]
ps <- merge(p, sy, all.x = TRUE) #ps[!is.na(syid) & duplicated(syid)] # based on synonym dataset, some 384 species are synonyms (subspecies)
setnames(ps, "syid", "syid_r")
ps[, scinam_r := scinam]

## get complexity data and prepare for checking with range data
d <- fread("./Data/song_raw.csv")[!is.na(elements) & rec_quality_subj %in% c("A", "B"), .(scinam)] %>% unique()
d[, id_s := 1:nrow(d)]
d <- merge(d, sy, all.x = TRUE)
setnames(d, "syid", "syid_s")
# d[duplicated(syid_s)] # 4 same syid_s indicating that some species in the sample could have been previously one species

## checking general
x <- d[!scinam %in% p$scinam] # ;nrow(x) # 13 species names not in ranges
y <- symerge(x, ps, sy)

## checking all duplicates manualy assigned what is needed
y[`.duplicates` == TRUE]
# plot the duplicates
for (i in unique(y[`.duplicates` == TRUE, scinam])) {
  # i = "xenopipo holochlora"
  print(i)
  yi <- y[`.duplicates` == TRUE & scinam %in% i]

  tar <- paste(yi$scinam_r %>% shQuote(), collapse = ",")

  con_r <- dbcon("mbulla", db = "AVES_ranges")
  ra <- dbq(con_r, glue("SELECT scinam, taxonomy tax, SHAPE
                FROM breeding_ranges_v2 WHERE
                scinam in ({tar})"), geom = "SHAPE") # ('cryptopipo lita')
  closeCon(con_r)
    # use the following if st_wrap_dateline works and then no need to use st_crs(ra) = projll
        # st_crs(ra) <- 4326
        # ra = st_wrap_dateline(ra, options = "WRAPDATELINE=YES")
  st_crs(ra) <- projll
  ra <- st_transform(ra, projeck4)

  g <- ggplot() +
    geom_sf(data = M, size = .1, fill = "grey95", col = NA) +
    geom_sf(data = ra, aes(fill = scinam), col = NA) +
    labs(subtitle = i)
  ggsave(file = paste0("Output/check3_map_", i, ".png"), width = 10, height = 5)
}

#dd <- fread("./Data/song_raw.csv")[!is.na(elements) & rec_quality_subj %in% c("A", "B"), ]
#dd[scinam == "geothlypis velata"]
# y[scinam == "geothlypis aequinoctialis"]
# p[scinam == "geothlypis aequinoctialis"]

### FIXes duplicates and those not listed should be merged as one species
#### chlorothraupis frenat assigned to multiple, but should be only to habia frenata
y <- y[!(scinam == "chlorothraupis frenata" & !scinam_r %in% "habia frenata")]
#### remove range for a different species (checked with our observations)
y <- y[!(scinam == "hypsipetes virescens" & scinam_r == "ixos sumatranus")]

# y[duplicated(scinam) & `.duplicates` == FALSE]
# OK chech for further duplicates - # dp <- d[scinam %in% p$scinam, .(scinam)]; dp$match = 1; y$match = 0; yy = y[!duplicated(scinam)]; dy = rbind(dp[, .(scinam, match)], yy[, .(scinam, match)]); dy[duplicated(scinam)]; nrow(dy)

## check NAs
y[is.na(scinam_r)] # 4 species from our data has no match in ranges; these species are often consideredd subspecies (eremomela salvadorii of eremomela icteropygialis, geothlypis velata of geothlypis aequinoctialis, turdus daguae of turdus assimilis, zimmerius flavidifrons of zimmerius viridiflavus); while the subspecies have no range in the db, the nominal species are already part of the song data and hence their ranges are takn into account within the species richness tests

### plot nominal species
for (i in c('eremomela icteropygialis','geothlypis aequinoctialis', 'turdus assimilis', 'zimmerius viridiflavus')) {
  # i = "xenopipo holochlora"
  print(i)
  tar <- paste(i %>% shQuote(), collapse = ",")
  con_r <- dbcon("mbulla", db = "AVES_ranges")
  ra <- dbq(con_r, glue("SELECT scinam, SHAPE
          FROM breeding_ranges_v2 WHERE
          scinam in ({tar})"), geom = "SHAPE") # ('cryptopipo lita')
  closeCon(con_r)
  st_crs(ra) <- projll
  ra <- st_transform(ra, projeck4)
  g <- ggplot() +
    geom_sf(data = M, size = .1, fill = "grey95", col = NA) +
    geom_sf(data = ra, fill = 'red', col = NA) +
    labs(subtitle = i)
  ggsave(file = paste0("Output/check4_map_", i, ".png"), width = 10, height = 5)
}

## prepare species list to get the ranges for
s1 = merge(d, ps) #species with same name in song and range data; same as s1 = d[scinam %in% ps$scinam]
s2 = y[, 1:6] ### species with different name in song and range data, including multiple entries (subspcies) in ranges
ss = rbind(s1, s2) %>% na.omit()
#ss[duplicated(scinam_r)]
length(unique(ss$scinam)) # getting ranges for 4936 species; 4 missing of which geothlypis velata is added below by cutting the range of nominal geothlypis aequinoctialis

tss_scinam <- paste(ss$scinam_r %>% unique() %>% str_replace("_", " ") %>% shQuote(), collapse = ",")

## GET breeding ranges for song dataset [has birdtree taxonomy]
con <- dbcon("mbulla", db = "AVES_ranges")
r <- dbq(con, glue("SELECT scinam,  SHAPE
                FROM breeding_ranges_v2 WHERE
                scinam in ({tss_scinam})"), geom = "SHAPE")
closeCon(con)
save(file = 'Data/temp_BR_comp.Rdata',r)
st_crs(r) <- 4326
r = st_wrap_dateline(r, options = "WRAPDATELINE=YES")
save(file = "Data/temp_BR_comp_wrapdateline.Rdata", r)
load("Data/temp_BR_comp_wrapdateline.Rdata")
  #st_crs(r) <- projll
r <- st_transform(r, projeck4)
# test whether horizontal line issue is eliminated -  it is
plot(r[r$scinam == "anthus cervinus", ])
save(file = 'Data/temp_ranges_comp_2024-02-05.Rdata', r)
# load(file = "Data/temp_ranges.Rdata")

## combine ranges for subspecies
nrow(r) == length(ss$scinam_r)
r <- r[order(r$scinam), ]
ss = ss[order(scinam_r)]
r$scinam_s = ss$scinam

nrow(r[duplicated(r$scinam_s),])
r[r$scinam_s%in% r$scinam_s[duplicated(r$scinam_s)], ]

L = list()
for( i in unique(r$scinam_s[duplicated(r$scinam_s)])){
  print(i)
  ri <- r[r$scinam_s == i, ]
  p1 <-
    ggplot() +
    geom_sf(data = M, size = .1, fill = "grey95", col = NA) +
    geom_sf(data = ri, fill = "red", col = "black")
  p2 <-
    ggplot() +
    geom_sf(data = M, size = .1, fill = "grey95", col = NA) +
    geom_sf(data = ri, aes(fill = scinam), col = "black") +
    theme(legend.position = "none")
  ggarrange(p1, p2, nrow = 2)
  ggsave(file = paste0("Output/check5_map_",i,"_.png"), width = 10, height = 10)

rii = ri[1,]
rii$geometry = st_combine(ri$geometry)
L[[i]] = rii
}

r2 = do.call(rbind, L)
r1 <- r[!r$scinam_s %in% r2$scinam_s, ]
rr = rbind(r1,r2[, c("scinam", "geometry", "scinam_s")])

## adjust species names to match thosse in song dataset
rr$scinam = rr$scinam_s; rr$scinam_s=NULL

## add range for geothlypis velata  by splitting range of geothlypis aequinoctialis
rangei <- rr[rr$scinam == "geothlypis aequinoctialis", ]
#### split range into two
sl <- data.frame(
  "lon" = c(-85, -80, -60, -40),
  "lat" = c(-8, -8, -8, -8)
) %>%
  st_as_sf(coords = 1:2) %>%
  st_set_crs(projll) %>%
  st_transform(projeck4) %>%
  st_union() %>%
  st_cast("LINESTRING") %>%
  st_sf()

# ggplot() + geom_sf(data = rangei) + geom_sf(data = sl, col = "red")

sls <- sl %>%
  lwgeom::st_split(., rangei) %>%
  st_collection_extract("LINESTRING")

slc <- sls %>% st_sample(size = round(sum(st_length(.)) / as_units(10000, "m"), 0) %>% as.numeric())

rangeii <- rangei %>%
  lwgeom::st_split(., sls %>% st_cast("MULTILINESTRING") %>%
    st_union()) %>%
  st_collection_extract("POLYGON") %>%
  mutate(id = row_number())

# ggplot(rangeii) + geom_sf(aes(fill = factor(id))) + geom_sf_label(aes(label = id), alpha = 0.5)

#### test
  rangei1 = rangei
  rangei1$geometry <- st_combine(rangeii$geometry[1:16])
  rangei2 <- rangei
  rangei2$scinam <- "geothlypis velata"
  rangei2$geometry = st_combine(rangeii$geometry[17:55])
  r_test <- rbind(rangei, rangei2)
  ggplot() +
    geom_sf(data = M, size = .1, fill = "grey95", col = NA) +
    geom_sf(data = r_test, aes(fill = scinam), col = "black")

#### assign
rr$geometry[rr$scinam == "geothlypis aequinoctialis"] <- st_combine(rangeii$geometry[1:16])
rrr <- rbind(rr, rangei2)

## CREATE rangeMapper project (N = 4937 species)
con_m <- rmap_connect(path = here("Data/DAT_rangeMapper.sqlite"))
rmap_add_ranges(con_m, x = rrr, ID = "scinam")
rmap_prepare(con_m, "hex", cellsize = 112500) # ~ 1 degree
rmap_save_map(con_m) # creates a species_richness map
  # check whether correctly plotted = YES
  #M <- rnaturalearth::ne_countries(returnclass = "sf", scale = "small") |>sf::st_transform(projeck4) |> rmapshaper::ms_dissolve(); R <- rmap_to_sf(con_m); ggplot() + geom_sf(data = R, aes(fill = species_richness), col = NA) + scale_fill_viridis_c() #geom_sf(data = M, size = .1, fill = "grey95", col = NA) +
  # ggsave(file = paste0("Output/check_map_passeriforms.png"), width = 10, height = 5)
dbDisconnect(con_m)

# END
