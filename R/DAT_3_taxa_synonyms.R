# ==========================================================================
# ❗ This script is provided as reference only. It contains links to
# the internal database of the Max Planck Institute's Lab of Ornithology.
# Collects taxonomy synonyms and exports those as `Data/taxa_synonyms.csv``
# ==========================================================================

# PACKAGES,SETTINGS
require('dbo')
require('data.table')

con = dbcon("mbulla", db = "AVES_taxonomy")
sy = dbq(con, q = 'select scinam, syid FROM AVES_taxonomy.synonyms_v2')
fwrite(file = here::here("Data/DAT_taxa_synonyms.csv"), sy)
closeCon(con)
