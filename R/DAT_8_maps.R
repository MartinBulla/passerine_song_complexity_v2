# ==========================================================================
# ❗ The script runs relative to the project's root directory,
# and prepares map layers in the DAT_rangeMapper.sqlite.
# ==========================================================================

# PACKAGES, SETTINGS
source(here::here("R/__init__.R")) # source( here::here('R/__init__comp.R'))

sapply(c('doFuture','fasterize', 'rangeMapper', 'raster', 'qs', 'sdb', 'sf'),
  FUN = require, character.only = TRUE, quietly = TRUE
)

# DATA
d <- fread(here::here("Data/DAT_song_by_species.csv"), yaml = TRUE)
d[clade %in% c("Acanthisitti"), clade := "Suboscines"]
d[, scinam := str_replace(scinam, "_", " ")]

con_m <- rmap_connect(path = here::here("Data/DAT_rangeMapper.sqlite"))

# PREPARE MAPs

# for both clades

song_complexity = d[, .(scinam, clade, song_complexity = log10(element_types_extrapol_mean))]
rmap_add_bio(con_m, song_complexity, "scinam")
rmap_save_map(con_m,
  fun = "avg", src = "song_complexity", v = "song_complexity",
  dst = "mean_song_complexity"
)

SSD = d[, .(scinam, clade, SSD)]
rmap_add_bio(con_m, SSD, "scinam")
rmap_save_map(con_m,
  fun = "avg", src = "SSD", v = "SSD",
  dst = "mean_SSD"
)

col_dimorph = d[, .(scinam, clade, col_dimorph)]
rmap_add_bio(con_m, col_dimorph, "scinam")
rmap_save_map(con_m,
  fun = "avg", src = "col_dimorph", v = "col_dimorph",
  dst = "mean_col_dimorph"
)

degree_of_polygyny = d[, .(scinam, clade, degree_of_polygyny)]
rmap_add_bio(con_m, degree_of_polygyny, "scinam")
rmap_save_map(con_m,
  fun = "avg", src = "degree_of_polygyny", v = "degree_of_polygyny",
  dst = "mean_degree_of_polygyny"
)

male_plumage_score = d[, .(scinam, clade, male_plumage_score)]
rmap_add_bio(con_m, male_plumage_score, "scinam")
rmap_save_map(con_m,
  fun = "avg", src = "male_plumage_score", v = "male_plumage_score",
  dst = "mean_male_plumage_score"
)

colour_male = d[, .(scinam, clade, colour_male)]
rmap_add_bio(con_m, colour_male, "scinam")
rmap_save_map(con_m,
  fun = "avg", src = "colour_male", v = "colour_male",
  dst = "mean_colour_male"
)

territoriality = d[, .(scinam, clade, territoriality)]
rmap_add_bio(con_m, territoriality, "scinam")
rmap_save_map(con_m,
  fun = "avg", src = "territoriality", v = "territoriality",
  dst = "mean_territoriality"
)

social_bond = d[, .(scinam, clade, social_bond)]
rmap_add_bio(con_m, social_bond, "scinam")
rmap_save_map(con_m,
  fun = "avg", src = "social_bond", v = "social_bond",
  dst = "mean_social_bond"
)

habitat_breadth = d[, .(scinam, clade, habitat_breadth)]
rmap_add_bio(con_m, habitat_breadth, "scinam")
rmap_save_map(con_m,
  fun = "avg", src = "habitat_breadth", v = "habitat_breadth",
  dst = "mean_habitat_breadth"
)

migration_strategy = d[, .(scinam, clade, migration_strategy)]
rmap_add_bio(con_m, migration_strategy, "scinam")
rmap_save_map(con_m,
  fun = "avg", src = "migration_strategy", v = "migration_strategy",
  dst = "mean_migration_strategy"
)

tree_cover = d[, .(scinam, clade, tree_cover)]
rmap_add_bio(con_m, tree_cover, "scinam")
rmap_save_map(con_m,
  fun = "avg", src = "tree_cover", v = "tree_cover",
  dst = "mean_tree_cover"
)

# for Oscines
rmap_save_subset(con_m, dst = "osc", song_complexity = "clade = 'Oscines'")
rmap_save_map(con_m,
  fun = "avg", src = "song_complexity", v = "song_complexity", subset = "osc",
  dst = "mean_song_complexity_osc"
)

rmap_save_subset(con_m, dst = "osc", SSD = "clade = 'Oscines'")
rmap_save_map(con_m,
  fun = "avg", src = "SSD", v = "SSD", subset = "osc",
  dst = "mean_SSD_osc"
)

rmap_save_subset(con_m, dst = "osc", col_dimorph = "clade = 'Oscines'")
rmap_save_map(con_m,
  fun = "avg", src = "col_dimorph", v = "col_dimorph", subset = "osc",
  dst = "mean_col_dimorph_osc"
)

rmap_save_subset(con_m, dst = "osc", degree_of_polygyny = "clade = 'Oscines'")
rmap_save_map(con_m,
  fun = "avg", src = "degree_of_polygyny", v = "degree_of_polygyny", subset = "osc",
  dst = "mean_degree_of_polygyny_osc"
)

rmap_save_subset(con_m, dst = "osc", male_plumage_score = "clade = 'Oscines'")
rmap_save_map(con_m,
  fun = "avg", src = "male_plumage_score", v = "male_plumage_score", subset = "osc",
  dst = "mean_male_plumage_score_osc"
)

rmap_save_subset(con_m, dst = "osc", colour_male = "clade = 'Oscines'")
rmap_save_map(con_m,
  fun = "avg", src = "colour_male", v = "colour_male", subset = "osc",
  dst = "mean_colour_male_osc"
)

rmap_save_subset(con_m, dst = "osc", territoriality = "clade = 'Oscines'")
rmap_save_map(con_m,
  fun = "avg", src = "territoriality", v = "territoriality", subset = "osc",
  dst = "mean_territoriality_osc"
)

rmap_save_subset(con_m, dst = "osc", social_bond = "clade = 'Oscines'")
rmap_save_map(con_m,
  fun = "avg", src = "social_bond", v = "social_bond", subset = "osc",
  dst = "mean_social_bond_osc"
)

rmap_save_subset(con_m, dst = "osc", habitat_breadth = "clade = 'Oscines'")
rmap_save_map(con_m,
  fun = "avg", src = "habitat_breadth", v = "habitat_breadth", subset = "osc",
  dst = "mean_habitat_breadthy_osc"
)

rmap_save_subset(con_m, dst = "osc", migration_strategy = "clade = 'Oscines'")
rmap_save_map(con_m,
  fun = "avg", src = "migration_strategy", v = "migration_strategy", subset = "osc",
  dst = "mean_migration_strategy_osc"
)

rmap_save_subset(con_m, dst = "osc", tree_cover = "clade = 'Oscines'")
rmap_save_map(con_m,
  fun = "avg", src = "tree_cover", v = "tree_cover", subset = "osc",
  dst = "mean_tree_cover_osc"
)

# for Suboscines

rmap_save_subset(con_m, dst = "sub", song_complexity = "clade = 'Suboscines'")
rmap_save_map(con_m,
  fun = "avg", src = "song_complexity", v = "song_complexity", subset = "sub",
  dst = "mean_song_complexity_sub"
)

rmap_save_subset(con_m, dst = "sub", SSD = "clade = 'Suboscines'")
rmap_save_map(con_m,
  fun = "avg", src = "SSD", v = "SSD", subset = "sub",
  dst = "mean_SSD_sub"
)

rmap_save_subset(con_m, dst = "sub", col_dimorph = "clade = 'Suboscines'")
rmap_save_map(con_m,
  fun = "avg", src = "col_dimorph", v = "col_dimorph", subset = "sub",
  dst = "mean_col_dimorph_sub"
)

rmap_save_subset(con_m, dst = "sub", degree_of_polygyny = "clade = 'Suboscines'")
rmap_save_map(con_m,
  fun = "avg", src = "degree_of_polygyny", v = "degree_of_polygyny", subset = "sub",
  dst = "mean_degree_of_polygyny_sub"
)

rmap_save_subset(con_m, dst = "sub", male_plumage_score = "clade = 'Suboscines'")
rmap_save_map(con_m,
  fun = "avg", src = "male_plumage_score", v = "male_plumage_score", subset = "sub",
  dst = "mean_male_plumage_score_sub"
)

rmap_save_subset(con_m, dst = "sub", colour_male = "clade = 'Suboscines'")
rmap_save_map(con_m,
  fun = "avg", src = "colour_male", v = "colour_male", subset = "sub",
  dst = "mean_colour_male_sub"
)

rmap_save_subset(con_m, dst = "sub", territoriality = "clade = 'Suboscines'")
rmap_save_map(con_m,
  fun = "avg", src = "territoriality", v = "territoriality", subset = "sub",
  dst = "mean_territoriality_sub"
)

rmap_save_subset(con_m, dst = "sub", social_bond = "clade = 'Suboscines'")
rmap_save_map(con_m,
  fun = "avg", src = "social_bond", v = "social_bond", subset = "sub",
  dst = "mean_social_bond_sub"
)

rmap_save_subset(con_m, dst = "sub", habitat_breadth = "clade = 'Suboscines'")
rmap_save_map(con_m,
  fun = "avg", src = "habitat_breadth", v = "habitat_breadth", subset = "sub",
  dst = "mean_habitat_breadthy_sub"
)

rmap_save_subset(con_m, dst = "sub", migration_strategy = "clade = 'Suboscines'")
rmap_save_map(con_m,
  fun = "avg", src = "migration_strategy", v = "migration_strategy", subset = "sub",
  dst = "mean_migration_strategy_sub"
)

rmap_save_subset(con_m, dst = "sub", tree_cover = "clade = 'Suboscines'")
rmap_save_map(con_m,
  fun = "avg", src = "tree_cover", v = "tree_cover", subset = "sub",
  dst = "mean_tree_cover_sub"
)

dbDisconnect(con_m)


## realms
# zoor <- here("DATA/zoogeo_eck4.rds")
# zo <- readRDS(zoor)
# zo$Realm_num <- as.factor(zo$Realm) %>% as.numeric()
# r <- rmap_to_sf(con_m, "bbox") %>% raster(res = 112500 / 20)
# zov <- fasterize(zo, r, field = "Realm_num", fun = "min")
# rmap_save_map(con_m, fun = "max", src = zov, dst = "realms")
