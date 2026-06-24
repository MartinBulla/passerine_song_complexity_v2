# ==========================================================================
# 📍 The script runs relative to the project's root directory,
# uses 100 trees from Jetz et al. download from https://birdtree.org/,
# generates max credibility tree and exports it into
# .Data/DAT_passerine_maxcred_tree.qs.
# ==========================================================================

# settings
    source( here::here('R/__init__.R') )

    sapply(c( 'sdb', 'ape', 'phytools', 'phangorn'), FUN=require,
        character.only = TRUE, quietly = TRUE)

# build a max clade tree
    x = read.tree( here('Data/_100.birdtree.org.trees.tre'))
    ctree = phangorn::maxCladeCred(x)

    # check
    is.rooted(ctree)
    is.ultrametric(ctree)

    phy = data.table(scinam = ctree$tip.label %>%
                              str_to_lower %>%
                              str_replace('_', ' '))

# target species names
    x = fread('./Data/song_raw.csv')[, .(scinam)]  %>% unique

    sy = fread(here::here("Data/DAT_taxa_synonyms.csv"))#dbq(q = 'select scinam , syid FROM AVES_taxonomy.synonyms_v2')

    # @CHECK same species different names?
    cc = merge(x, sy, by = 'scinam')[, n := .N, by = syid]
    cc = cc[n>1] ; setorder(cc, syid)
    cc # OK

# find missing species in phy but existing in x
    z = symerge(x, phy, sy, clean = FALSE)
    # @check
    z[!is.na(syid) & !is.na(scinam_tdata)] # ok

# prepare ctree
    ctree$tip.label = ctree$tip.label %>% str_to_lower
    keep = str_replace(x$scinam, ' ', '_')

    ctrees = drop.tip(ctree, setdiff(ctree$tip.label, keep) )

# Export
   qs::qsave(ctrees, here('Data/DAT_passerine_maxcred_tree.qs') )

# END
