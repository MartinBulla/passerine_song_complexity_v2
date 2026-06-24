# ==========================================================================
# ❗ This script is provided as reference only. It contains links to
# the internal database of the Max Planck Institute's Lab of Ornithology.
# The script runs relative to the project's root directory,
# gathers data from published sources (see the manuscript for references)
# and exports the collected data ./Data/Dat_traits.Rdata
# ==========================================================================

require(here); require(sdb)

# COLLECT: colouration
co <- dbq(q = "select scinam, Female_plumage_score, Male_plumage_score FROM
        AVES_lifeHistory.coloration_passerines",user = 'mbulla')

# COLLECT: sociality
soc <- dbq(q = "select scinam, social_bond from AVES_lifeHistory.sociality", user = "mbulla")

# COLLECT: territoriality
ter <- dbq(q = "SELECT scinam,territoriality FROM AVES_lifeHistory.territoriality", user = "mbulla")

# COLLECT: habitat_breadth
#hab <- dbq(q = "SELECT scinam, habitat_breadth from AVES_environment.habitat_breadth", user = "mbulla")

# COLLECT: migration
mig <- dbq(q = "select scinam, strategy_class migration_strategy from AVES_lifeHistory.migration_v2", user = "mbulla")
mig$migration_strategy[mig$scinam %in% c("phylloscopus proregulus","spizella pallida","vermivora bachmanii")] = 3 # correcting mistakes

# COLLECT: mating system (sexual_selection_intensity - degree of polygyny)
# ss <- dbq(q = "SELECT scinam, sexual_selection_intensity FROM AVES_lifeHistory.sexual_selection_on_males;", user = 'mbulla')

# EXPORT
save(file = here::here('Data/DAT_traits.Rdata'), co, soc, ter, mig)

# END
