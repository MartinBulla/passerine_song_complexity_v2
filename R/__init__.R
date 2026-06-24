# ==============================================================================
# Project-specific environment variables, dependencies, functions and objects
# ==============================================================================


# PACKAGES
  # function to load/install packages
    install_if_missing <- function(
        pkgs,
        dependencies = NA,        # NA avoids Suggests (CRAN behavior)
        attach = TRUE,
        repos = getOption("repos"),
        extra_repos = NULL,       # e.g. c(r_universe="https://cran.r-universe.dev")
        github = NULL,            # e.g. c(dbo="ornitho-logics/dbo")
        upgrade_github = "never"  # avoids interactive prompts
        ) {
        pkgs <- unique(as.character(pkgs))

        # Put extra repos FIRST so they win when duplicates exist
        repos2 <- repos
        if (!is.null(extra_repos)) repos2 <- c(extra_repos, repos2)

        # Normalize github mapping to a named character vector
        gh_map <- NULL
        if (!is.null(github)) {
            if (is.list(github)) {
            gh_map <- unlist(github, use.names = TRUE)
            } else {
            gh_map <- github
            }
            if (is.null(names(gh_map)) || any(!nzchar(names(gh_map)))) {
            stop("github must be a named vector/list like c(dbo='ornitho-logics/dbo').", call. = FALSE)
            }
            gh_map <- as.character(gh_map)
        }

        install_from_repos <- function(p) {
            install.packages(p, dependencies = dependencies, repos = repos2)
        }

        install_from_github <- function(repo) {
            if (!requireNamespace("remotes", quietly = TRUE)) {
            install.packages("remotes", repos = repos2)
            }

            # For GitHub installs, mimic 'no Suggests' explicitly
            gh_deps <- c("Depends", "Imports", "LinkingTo")

            remotes::install_github(
            repo,
            upgrade = upgrade_github,
            dependencies = gh_deps
            )
        }

        # Identify missing
        is_installed <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
        to_install <- pkgs[!is_installed]

        if (length(to_install)) {
            message("Installing missing package(s): ", paste(to_install, collapse = ", "))

            for (p in to_install) {
            # 1) Try GitHub first if mapped
            if (!is.null(gh_map) && p %in% names(gh_map)) {
                repo <- unname(gh_map[[p]])
                try(install_from_github(repo), silent = TRUE)
            }

            # 2) If still missing, try CRAN-like repos
            if (!requireNamespace(p, quietly = TRUE)) {
                try(install_from_repos(p), silent = TRUE)
            }

            # 3) Final check
            if (!requireNamespace(p, quietly = TRUE)) {
                stop(
                sprintf(
                    "Package '%s' is still not installed.\n- If GitHub-only, add github=c(%s='%s').\n- Otherwise, check install output above (system deps / compiler / permissions).",
                    p, p, if (!is.null(gh_map) && p %in% names(gh_map)) gh_map[[p]] else "OWNER/REPO"
                ),
                call. = FALSE
                )
            }
            }
        }

        if (isTRUE(attach)) {
            invisible(lapply(pkgs, function(p) {
            suppressPackageStartupMessages(library(p, character.only = TRUE))
            }))
        }

        invisible(TRUE)
        }

    install_if_missing("data.table")

# VARIABLES & OPTIONS
options(stringsAsFactors = FALSE)

set.seed(25)

nsim = 5000 # set number of desired simulations

projll <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
projeck4 <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # Eckert IV projection

Parameter_names <- fread("
                        term                , Parameter
                        (Intercept)         , Intercept
                scale(colour_male)          , Male colorfulness
  cladeSuboscines:scale(colour_male)        , Male colorfulness S r O
                scale(mimicking)            , Mimicry
                        mimicking           , Mimicry
                scale(elements_mean)        , Mean number of scored elements
                log10(elements_mean)        , Mean number of scored elements
                elements_mean               , Mean number of scored elements
                flocking_num                , Flocking
    cladeSuboscines:scale(flocking_num      , Flocking S r O
                  cladeSuboscines           , Clade S r O
  scale(sexual_selection_intensity)         , Sexual selection intensity
        sexual_selection_intensity          , Sexual selection intensity
cladeSuboscines:scale(sexual_selection_intensity        , Sexual selection intensity S r O
cladeSuboscines:scale(sexual_selection_intensity)        , Sexual selection intensity S r O
                degree_of_polygyny          , Degree of polygyny
                avg_degree_of_polygyny          , Degree of polygyny
                avg_degree_of_polygyny_osc          , Degree of polygyny
                avg_degree_of_polygyny_sub          , Degree of polygyny
                scale(degree_of_polygyny)   , Degree of polygyny
                scale(avg_degree_of_polygyny)   , Degree of polygyny
                scale(avg_degree_of_polygyny_osc)   , Degree of polygyny
                scale(avg_degree_of_polygyny_sub)   , Degree of polygyny
cladeSuboscines:scale(degree_of_polygyny        , Degree of polygyny S r O
cladeSuboscines:scale(degree_of_polygyny)        , Degree of polygyny S r O
cladeSuboscines:scale(tree_cover)          , Tree cover S r O
cladeSuboscines:scale(tree_cover          , Tree cover S r O
scale(colour_male):scale(tree_cover)          , Male colorfulness × Tree cover
scale(abs(lat)):scale(tree_cover)          , Absolute latitude × Tree cover
scale(colour_male):scale(abs(lat))          , Male colorfulness × Absolute latitude
cladeSuboscines:scale(colour_male):scale(tree_cover)          , Male colorfulness × Tree cover S r O
cladeSuboscines:scale(abs(lat)):scale(tree_cover)          , Absolute latitude × Tree cover S r O
cladeSuboscines:scale(colour_male):scale(abs(lat))          , Male colorfulness × Absolute latitude S r O
scale(colour_male):scale(abs(lat)):scale(tree_cover)          , Male colorfulness × Absolute latitude × Tree cover
cladeSuboscines:scale(colour_male):scale(abs(lat)):scale(tree_cover)          , Male colorfulness × Absolute latitude × Tree cover S r O
cladeSuboscines:scale(duet)          , Duetting S r O
                       scale(duet)          , Duetting
                       tree_cover          , Tree cover
                       scale(tree_cover)          , Tree cover
                        scale(SSD)          , Sexual size dimorphism
                        scale(avg_SSD)          , Sexual size dimorphism
                        scale(avg_SSD_osc)          , Sexual size dimorphism
                        scale(avg_SSD_sub)          , Sexual size dimorphism
                               SSD          , Sexual size dimorphism
                              avg_SSD          , Sexual size dimorphism
                              avg_SSD_osc          , Sexual size dimorphism
                              avg_SSD_sub          , Sexual size dimorphism
                              scale(colourfulness)          , Male colorfulness
        cladeSuboscines:scale(colourfulness)           , Male colorfulness S r O
        cladeSuboscines:scale(SSD           , Sexual size dimorphism S r O
        cladeSuboscines:scale(SSD)           , Sexual size dimorphism S r O
        scale(sexual_dichromatism)          , Sexual dichromatism
                scale(col_dimorph)          , Sexual color dimorphism
                scale(avg_col_dimorph)         , Sexual color dimorphism
                scale(avg_col_dimorph_osc)         , Sexual color dimorphism
                scale(avg_col_dimorph_sub)         , Sexual color dimorphism
                   avg_col_dimorph          , Sexual color dimorphism
                   avg_col_dimorph_osc          , Sexual color dimorphism
                   avg_col_dimorph_sub          , Sexual color dimorphism
                       col_dimorph          , Sexual color dimorphism
 cladeSuboscines:scale(col_dimorph          , Sexual color dimorphism S r O
 cladeSuboscines:scale(col_dimorph)          , Sexual color dimorphism S r O
                scale(log(body_mass))       , ln(Body mass)
                scale(log10(wing_length))   , log(Wing length)
                log10(wing_length)          , log(Wing length)
cladeSuboscines:scale(log10(wing_length)    , log(Wing length) S r O
          scale(Male_plumage_score)         , Male-like coloration
          scale(avg_male_plumage_score)         , Male-like coloration
          scale(avg_male_plumage_score_osc)         , Male-like coloration
          scale(avg_male_plumage_score_sub)         , Male-like coloration
          scale(male_plumage_score)         , Male-like coloration
                 male_plumage_score         , Male-like coloration
                 avg_male_plumage_score_osc         , Male-like coloration
                 avg_male_plumage_score_sub         , Male-like coloration
                 avg_male_plumage_score         , Male-like coloration
cladeSuboscines:scale(male_plumage_score    , Male-like coloration S r O
cladeSuboscines:scale(male_plumage_score)   , Male-like coloration S r O
                scale(social_bond)          , Social bond
                scale(avg_social_bond)          , Social bond
                scale(avg_social_bond_osc)          , Social bond
                scale(avg_social_bond_sub)          , Social bond
                       social_bond          , Social bond
                       avg_social_bond          , Social bond
                       avg_social_bond_osc          , Social bond
                       avg_social_bond_sub          , Social bond
 cladeSuboscines:scale(social_bond          , Social bond S r O
 cladeSuboscines:scale(social_bond)          , Social bond S r O
              scale(territoriality)         , Territoriality
              scale(avg_territoriality)         , Territoriality
              scale(avg_territoriality_osc)         , Territoriality
              scale(avg_territoriality_sub)         , Territoriality
                     territoriality         , Territoriality
                     avg_territoriality         , Territoriality
                     avg_territoriality_osc         , Territoriality
                     avg_territoriality_sub         , Territoriality
cladeSuboscines:scale(territoriality        , Territoriality S r O
cladeSuboscines:scale(territoriality)        , Territoriality S r O
            scale(habitat_breadth)          , Habitat generalism
            scale(avg_habitat_breadth)          , Habitat generalism
            scale(avg_habitat_breadth_osc)          , Habitat generalism
            scale(avg_habitat_breadth_sub)          , Habitat generalism
                    habitat_breadth         , Habitat generalism
                    avg_habitat_breadth         , Habitat generalism
                    avg_habitat_breadth_osc         , Habitat generalism
                    avg_habitat_breadth_sub         , Habitat generalism
cladeSuboscines:scale(habitat_breadth       , Habitat generalism S r O
cladeSuboscines:scale(habitat_breadth)       , Habitat generalism S r O
                    migration_strategy      , Migration
                    avg_migration_strategy      , Migration
                    avg_migration_strategy_osc      , Migration
                    avg_migration_strategy_sub      , Migration
             scale(migration_strategy)      , Migration
             scale(avg_migration_strategy)      , Migration
             scale(avg_migration_strategy_osc)      , Migration
             scale(avg_migration_strategy_sub)      , Migration
cladeSuboscines:scale(migration_strategy)   , Migration S r O
cladeSuboscines:scale(migration_strategy    , Migration S r O
                    migration_AVONET        , Migration AVONET
cladeSuboscines:scale(migration_AVONET      , Migration AVONET S r O
                     scale(tempVar)         , Temperature seasonality
                     scale(median_tempVar)  , Temperature seasonality
                     scale(precvar)         , Precipitation seasonality
                     scale(median_precVar)  , Precipitation seasonality
                    abs(lat_mean)            , Absolute latitude
                    abs(lat)            , Absolute latitude
                    lat_abs            , Absolute latitude
                scale(abs(lat_mean))        , Absolute latitude
                scale(abs(lat))        , Absolute latitude
                scale(lat_abs)        , Absolute latitude
cladeSuboscines:scale(abs(lat_mean)         , Absolute latitude S r O
cladeSuboscines:scale(abs(lat_mean))        , Absolute latitude S r O
cladeSuboscines:scale(abs(lat)         , Absolute latitude S r O
cladeSuboscines:scale(abs(lat))        , Absolute latitude S r O
scale(abs_lat_lin)        , Absolute latitude (linear component)
scale(abs_lat_qua)        , Absolute latitude (quadratic component)
cladeSuboscines:scale(abs_lat_lin)        , Absolute latitude (linear component) S r O
cladeSuboscines:scale(abs_lat_qua)        , Absolute latitude (quadratic component) S r O
                scale(logaltitude)          , Altitude
                scale(logmedian_altitude)   , Altitude
                scale(avg_altitude_log)  , Altitude
                avg_altitude_log  , Altitude
                scale(log10(mean_sp_altitude))  , Altitude
                log10(mean_sp_altitude)     , Altitude
cladeSuboscines:scale(log10(mean_sp_altitude) , Altitude S r O
cladeSuboscines:scale(log10(mean_sp_altitude)) , Altitude S r O
      species_richness         , Species richness
      scale(species_richness)         , Species richness
      scale(local_species_richness)         , Local species richness
      scale(species_richness         , Species richness
      scale(local_species_richness_mean)    , Local species richness
            local_species_richness_mean     , Local species richness
cladeSuboscines:scale(local_species_richness_mean     , Local species richness S r O
cladeSuboscines:scale(local_species_richness_mean)     , Local species richness S r O
ecore (Intercept)     , eco-region
realm (Intercept)     , zoogeographical realm
genus:(family:(superfamily:(parvorder:infraorder)))     , genus
genus:(family:(superfamily:(parvorder:infraorder))) (Intercept)   , genus
family:(superfamily:(parvorder:infraorder))     , family
 family:(superfamily:(parvorder:infraorder)) (Intercept)     , family
superfamily:(parvorder:infraorder)      , superfamily
superfamily:(parvorder:infraorder) (Intercept)      , superfamily
parvorder:infraorder        , parvorder
parvorder:infraorder (Intercept)        , parvorder
infraorder (Intercept)        , infraorder
Residual        , residual variance
scale(epp_br_prop)         , Proportion of extra-pair broods
cladeSuboscines:scale(epp_br_prop)         , Proportion of extra-pair broods S r O
scale(epp_br_logit)         , Logit of proportion of extra-pair broods
cladeSuboscines:scale(epp_br_logit)         , Logit of proportion of extra-pair broods S r O
scale(epp_of_prop)         , Proportion of extra-pair offspring
cladeSuboscines:scale(epp_of_prop)         , Proportion of extra-pair offspring S r O
scale(epp_of_logit)         , Logit of proportion of extra-pair offspring
cladeSuboscines:scale(epp_of_logit)         , Logit of proportion of extra-pair offspring S r O
cladeSuboscines:scale(species_richness     , Species richness S r O
cladeSuboscines:scale(species_richness)     , Species richness S r O
      scale(median_SR)                      , Local species richness
                    scinam_scinam           , Species identity
scale(tree_cover):scale(abs(lat))         , Tree cover × Absolute latitude
cladeSuboscines:scale(tree_cover):scale(abs(lat))         , Tree cover × Absolute latitude S r O
            tip_labels_tip_labels           , Phylogeny
            scale(brain_resid)           , Residual brain mass
            scale(brain)           , Brain mass
            cladeSuboscines:scale(brain_resid           , Residual brain mass S r O
            cladeSuboscines:scale(brain)           , Brain mass S r O
            cladeSuboscines:scale(brain)           , Brain mass S r O
            cladeSuboscines:scale(brain_resid)           , Residual brain mass S r O
    ")

# IMPORTS
    import::from(DBI, "dbExecute")

# other FUNCTIONS ----
    # Helper function for Y-coordinates in meters
    get_y_eck4 <- function(lat) {
      p <- st_sfc(st_point(c(0, lat)), crs = 4326)
      return(st_coordinates(st_transform(p, projeck4))[2])
    }

    reset_parallel <- function() { # call before/after paralle sesion
        foreach::registerDoSEQ()
        closeAllConnections()
    }

    scaleFUN <- function(x) sprintf("%.2f", x)

    Mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
    }
    parens <- function(x) paste0("(", x, ")")
    onlyBars <- function(form) {
        reformulate(sapply(
            findbars(form),
            function(x) parens(deparse(x))),
            response = ".")
    }

    firstup <- function(x) {
        substr(x, 1, 1) <- toupper(substr(x, 1, 1))
        x
    }

    monitorini <- function(f = '/tmp/monitor.txt') {
      system( paste('>', f) )
      cat('check with:\n')
      cat("\n tail -n +1 -f", f ,"| awk '{printf \"\\r%lu\", NR}' \n")
      cat('\n')
      }

    monitor <- function(i) {
      cat(file = '/tmp/monitor.txt', i, '\n', append = TRUE)
      }


    dt_as_sf <- function(x, crs = projeck4) {
        o = st_as_sf(x, crs = crs)
        class(o) = c('sf', 'data.frame')
        o
        }


    tic <- function() {
        assign('.tic', proc.time(),envir =  .GlobalEnv)
    }

    tac <- function(msg = 'Done in') {
        cat(msg,data.table::timetaken(.tic),"\n")
        }

    fig <- function(p, fig, w, h, r = 400, ...) {
          ragg::agg_png(file = glue("./Output/{fig}.png"), width = w, height = h, res = r, ...)
          plot(p)
          dev.off()
    }

    sset <- function(x, f, rownams = "tip_labels") {
        colnams <- str_extract_all(
            as.character(f) |> paste(collapse = " "),
            names(x)
        ) |>
            unlist() |>
            unique()

        o = x[, c(colnams, rownams), with = FALSE] |>
            na.omit() |>
            as.data.frame()
        row.names(o) <- o[, rownams]

        o
    }

    physet <- function(x, p = phy_subset) {
      ape::drop.tip(p, setdiff(p$tip.label, x$tip_labels))
    }

    phylolm_out <- function(fm, p = phys, R2 = FALSE) {
        o <- summary(fm)
        ci <- confint(fm)

        # coefs & confint
        cc <- o |> coef()

        cc <- data.table(term = row.names(cc), cc, conf.low = ci[, 1], conf.high = ci[, 2])

        # Lambda
        if (!is.null(o$optpar)) {
            cc <- rbind(cc, data.table(term = "Pagel's λ", Estimate = o$optpar), fill = TRUE)
        }

        # Rsq-s
        if(R2){
        R2 <- rr2::R2_resid(fm, phy = p)
        rro <- data.table(term = "R²", Estimate = R2)

        o <- rbind(cc, rro, fill = TRUE)
        } else o = cc

        o <- merge(o, Parameter_names, all.x = TRUE, sort = FALSE)

        o[is.na(Parameter), Parameter := term][, term := NULL]

        setcolorder(o, "Parameter")

        # Add N
        o <-
            rbind(o,
                data.table(Parameter = glue("N species={nrow(fm$X)}") |> as.character()),
                fill = TRUE
            )
    }

    phylolm_cred <- function(form, dat = d, Tree = tree,lm=TRUE, r_r = 3, r_f =2, R2 = FALSE) {

        d = dat

        # subset d and tree
        x = d[, c( all.vars(form), 'scinam') , with = FALSE] %>% na.omit
        x = x[ scinam %in% Tree$tip.label]

        treei = drop.tip(Tree, setdiff(Tree$tip.label, x$scinam) )

        di = x[ scinam %in% treei$tip.label]
        di = as.data.frame(di)
        row.names(di) = x$scinam

        # run phylolm and extract summary and ci
        if(lm)
        fm = phylolm(form, di, treei, model = 'lambda') else
        fm = binaryPGLMM(form, di, treei)

        o = summary(fm)
        ci =confint(fm)

        # coefs & confint
        cc = o %>% coef

        cc = data.table(term = row.names(cc), round(cc,r_r), lwr = round(ci[, 1], r_r), upr = round(ci[, 2], r_r))

        cc[, term := str_replace_all(term, '^scale\\(', '') ]
        cc[, term := str_replace_all(term, '^\\(', '') ]
        cc[, term := str_replace_all(term, '\\)$', '') ]
        setnames(cc, c('Estimate'), c('estimate'))

        # Lambda
        cc = rbind(cc, data.table(term = 'Pagel\'s λ', estimate = round(o$optpar,r_f)) , fill = TRUE)

        if(R2){
        # Rsq-s
        R2full = rr2::R2_resid(fm, phy = treei)
        R2fixef = rr2::R2_resid(fm, update(fm, . ~ 1), phy = treei)
        rro = data.table(term = c( 'R²full','R²fixef') , estimate = paste0(100*round(c( R2full, R2fixef), r_f), '%'))

        o = rbind(cc, rro , fill = TRUE)
        } else o = cc
        # Add N  and i
        o[, N := nrow(di)]

        o <- merge(o, Parameter_names, all.x = TRUE, sort = FALSE)
        o[is.na(Parameter), Parameter := term][, term := NULL]
        setnames(o, old = "Parameter", new = "term")
        o

        }

      est_out = function(
          model = m, label = "", nsim = 5000, r_f =3, r_r = 2, R2 = FALSE,
          generate_new = TRUE,
          file_name = NULL,
          save_dir = here::here("Output", "est_out"),
          cache_path = NULL,
          seed = 5
        ) {

          # If a file name is provided, define where the est_out result
          # should be saved to / loaded from.
          if (!is.null(file_name)) { # load/save only if a file name provided
            file_path <- file.path(save_dir, paste0(file_name, ".rds")) # builds the full path to the saved file

            if (!generate_new) { # if generate_new = FALSE, do not rerun the simulation. Instead, try to load the saved output.
              if (!file.exists(file_path)) { # checks whether the saved file actually exists.
                stop("Saved est_out file does not exist: ", file_path,
                    "\nRun once with generate_new = TRUE.")
              }
              return(readRDS(file_path))
            }
          }

          set.seed(seed)
          bsim = sim(model, n.sim = nsim)
          v = round(apply(bsim@fixef, 2, quantile, prob = c(0.5)),r_f)
          ci = round(apply(bsim@fixef, 2, quantile, prob = c(0.025, 0.975)), r_f)
          sd = round(apply(bsim@fixef, 2, sd), r_f)
          o = data.table(term = rownames(coef(summary(model))), type = "fixed", estimate = v, lwr = ci[1, ], upr = ci[2, ], sd = sd, N = nobs(model), name = label)

          l = data.frame(summary(model)$varcor)
          l = l[is.na(l$var2), ]
          l$var1 = ifelse(is.na(l$var1), "", l$var1)
          l$pred = paste(l$grp, l$var1)

          q050 = {}
          q025 = {}
          q975 = {}
          pred = {}

          # variance of random effects
          for (ran in names(bsim@ranef)) {
              ran_type = l$var1[l$grp == ran]
              for (i in ran_type) {
                  q050 = c(q050, quantile(apply(bsim@ranef[[ran]][, , ran_type], 1, var), prob = c(0.5)))
                  q025 = c(q025, quantile(apply(bsim@ranef[[ran]][, , ran_type], 1, var), prob = c(0.025)))
                  q975 = c(q975, quantile(apply(bsim@ranef[[ran]][, , ran_type], 1, var), prob = c(0.975)))
                  pred = c(pred, paste(ran, i))
              }
          }
          # residual variance
          q050 = c(q050, quantile(bsim@sigma^2, prob = c(0.5)))
          q025 = c(q025, quantile(bsim@sigma^2, prob = c(0.025)))
          q975 = c(q975, quantile(bsim@sigma^2, prob = c(0.975)))
          pred = c(pred, "Residual")

          ri = data.table(type = "random", term = pred, estimate = round(100 * q050 / sum(q050)), lwr = round(100 * q025 / sum(q025)), upr = round(100 * q975 / sum(q975)), sd = "", N = nobs(model), name = label)

          ri[lwr > upr, lwr_rt := upr]
          ri[lwr > upr, upr_rt := lwr]
          ri[!is.na(lwr_rt), lwr := lwr_rt]
          ri[!is.na(upr_rt), upr := upr_rt]
          ri$lwr_rt = ri$upr_rt = NULL

          ri[, estimate := paste0(estimate, "%")]
          ri[, lwr := paste0(lwr, "%")]
          ri[, upr := paste0(upr, "%")]

          x = rbind(
                o[, .(term, type, estimate, lwr, upr, sd, N, name)],
                ri[, .(term, type, estimate, lwr, upr, sd, N, name)]
          )

          if(R2){
          rro = data.table(term = c("R²full", "R²fixef"), type = "", estimate = paste0(round(c(rr2::R2_resid(model), rr2::R2_resid(model, update(model, onlyBars(formula(model))))), r_r) * 100, "%"), lwr = "", upr = "", sd = "", N = nobs(model), name = label)
          x = rbind(x, rro)
          }

          x <- merge(x, Parameter_names, all.x = TRUE, sort = FALSE)
          x[is.na(Parameter), Parameter := term][, term := NULL]
          setnames(x, old = "Parameter", new = "term")

          if (!is.null(file_name)) {
            dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
            saveRDS(x, file_path)
          }

          return(x)
      }

    theme_here <- function(legend,
                           yaxis = TRUE,
                           mar = c(0, 5, 5, 5),
                           base_size = 14,
                           base_family = "helvetica") {
        tt <- (ggthemes::theme_foundation(base_size = base_size, base_family = base_family)
        + theme(
                plot.title = element_text(
                    face = "bold",
                    size = rel(1.2), hjust = 0.5
                ),
                text = element_text(),
                panel.background = element_rect(colour = NA),
                plot.background = element_rect(colour = NA),
                panel.border = element_rect(colour = NA),
                axis.title = element_text(face = "bold", size = rel(1)),
                axis.title.y = element_text(angle = 90, vjust = 2),
                axis.title.x = element_text(vjust = -0.2),
                axis.text = element_text(),
                axis.line = element_line(colour = "black"),
                axis.ticks = element_line(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.key = element_rect(colour = NA),
                legend.position = if (missing(legend) || is.na(legend)) "none" else legend,
                legend.direction = "horizontal",
                legend.key.size = unit(0.5, "cm"),
                legend.margin = margin(0, 0, 0, 0, "mm"),
                legend.title = element_text(face = "italic"),
                plot.margin = unit(mar, "mm"),
                strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
                strip.text = element_text(face = "bold")
            ))

        if (!yaxis) {
            tt <- tt +
                theme(
                    axis.text.y = element_blank(),
                    axis.title.y = element_blank()
                )
        }

        tt
    }

est_clade <- function(model = m, nsim = 5000) {
    n <- length(rownames(coef(summary(model)))) / 2
    bsim <- sim(model, n.sim = 5000)
    dt <- data.table(bsim@fixef)

    for (j in 1:n + 1) {
        # j = 2
        if (j == 2) {
           # print(names(dt)[j])
            dt[, (j) := dt[, 2] + dt[, 1]]
        } else {
            # j = 3
            #print(names(dt)[j])
            cols <- c(colnames(dt)[j], colnames(dt)[j + n - 1])
            dti <- dt[, ..cols]
            dt[, (cols[2]) := dti[, 2] + dti[, 1]] # dt[, ..cols_2]
        }
    }

    v <- apply(dt, 2, quantile, prob = c(0.5))
    ci_l <- apply(dt, 2, quantile, prob = c(0.025))
    ci_u <- apply(dt, 2, quantile, prob = c(0.975))

    o <- data.table(term = rownames(coef(summary(model))), estimate = v, lwr = ci_l, upr = ci_u, N = nobs(model))

    o <- merge(o, Parameter_names, all.x = TRUE, sort = FALSE)
    o[is.na(Parameter), Parameter := term][, term := NULL]
    setnames(o, old = "Parameter", new = "term")

    o[grepl("S r O", term, fixed = TRUE), clade := "Suboscines"]
    o[!grepl("S r O", term, fixed = TRUE), clade := "Oscines"]
    o[, term := str_replace(term, " S r O", "")]

    return(o)
}

  # model assumption function
m_ass = function(name = 'define', mo = m0, dat = d, fixed = NULL, categ = NULL, trans_ = "none", spatial = TRUE, temporal = TRUE, PNG = TRUE, outdir = 'outdir', n_col=7, width_ = 10){
    l=data.frame(summary(mo)$varcor)
    l = l[is.na(l$var2),]
    nt = if(temporal==TRUE){1}else{0}
    ns = if(spatial==TRUE){3}else{0}
    n = 3+nrow(l)-1+length(fixed)+length(categ) +  nt +  ns

    if(PNG == TRUE){
    png(paste(outdir, name, ".png", sep = ""), width = width_, height = 1.75*ceiling(n / n_col), units = "in", res = 300) # width = 6; res = 150 ok for html
    par(mfrow=c(ceiling(n / n_col), n_col),tcl = -0.08, cex = 0.5, cex.main = 0.9,#ceiling(n/n_col),n_col)
        oma = c(1,1,4,1),
        mar = c(2, 2, 3.5, 1), mgp=c(1,0,0)
        )
        }else{
        dev.new(width = width_, height = 1.75 * ceiling(n / n_col))
        par(mfrow=c(ceiling(n / n_col),n_col), tcl = -0.08, cex = 0.5, cex.main = 0.9,#ceiling(n/n_col),n_col)
        oma = c(1,1,4,1),
        mar = c(2, 2, 3.5, 1), mgp=c(1,0,0)
        )
    }

    scatter.smooth(fitted(mo),resid(mo),col='grey');abline(h=0, lty=2, col ='red')
    scatter.smooth(fitted(mo),sqrt(abs(resid(mo))), col='grey')
    qqnorm(resid(mo), main=list("Normal Q-Q Plot: residuals"),col='grey');qqline(resid(mo), col = 'red')
    #unique(l$grp[l$grp!="Residual"])
    for(i in unique(l$grp[l$grp!="Residual"])){
        #i = "mean_year"
        #i =unique(l$grp[l$grp!="Residual"])[1]
        #name_i = gsub("[(]", "", i)
        #name_i = gsub("[)]", "", name_i)
        name_i = i
        if(nchar(name_i)>10){
        stri_sub(name_i, nchar(name_i)/2+1, nchar(name_i)/2) <-'\n'
        }

        ll=ranef(mo)[names(ranef(mo))==i][[1]]
        if(ncol(ll)==1){
            qqnorm(ll[, 1], main = paste(name_i, names(ll)[1], sep = "\n"), col = "grey", )
            qqline(ll[, 1], col = "red")
            }else{
            qqnorm(ll[, 1], main = paste(name_i, names(ll)[1], sep = "\n"), col = "grey")
            qqline(ll[, 1], col = "red")
            qqnorm(ll[, 2], main = paste(name_i, names(ll)[2], sep = "\n"), col = "grey")
            qqline(ll[, 2], col = "red")
            }
    }

    # variables
        scatter={}
        for (i in rownames(summary(mo)$coef)) {
            # i = "lat_abs" #i = rownames(summary(mo)$coef)[9]
        j=sub("\\).*", "", sub(".*\\(", "",i))
        scatter[length(scatter)+1]=j
        }
        scatter = unique(scatter)
        x = data.frame(scatter=unique(scatter)[2:length(unique(scatter))],
                        log_ = grepl("log",rownames(summary(mo)$coef)[2:length(unique(scatter))]), stringsAsFactors = FALSE)
        if(length(fixed)!=0){
        for (i in 1:length(fixed)){
            jj =fixed[i] # jj = fixed[1]
            #print(jj)
            variable=dat[, ..jj][[1]]
            if(trans_[i]=='log'){
            scatter.smooth(resid(mo)~log(variable),xlab=paste('log(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
            }else if(trans_[i]=='abs'){
            scatter.smooth(resid(mo)~abs(variable),xlab=paste('abs(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
            }else if(trans_[i]=='sin'){scatter.smooth(resid(mo)~sin(variable),xlab=paste('sin(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
            }else if(trans_[i]=='cos'){scatter.smooth(resid(mo)~cos(variable),xlab=paste('cos(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
            }else{
            scatter.smooth(resid(mo)~variable,xlab=jj,col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
            }
        }
        }

        if(length(categ)!=0){
        for(i in categ){
            variable=dat[, ..i][[1]]
            boxplot(resid(mo)~variable, medcol='grey', whiskcol='grey', staplecol='grey', boxcol='grey', outcol='grey', xlab = i, col = 'white');abline(h=0, lty=3, lwd=1, col = 'red')
            }
        }

    if(temporal == TRUE){
        acf(resid(mo), type="p", main=list("Temporal autocorrelation:\npartial series residual"))
        }

    if(spatial == TRUE){
        spdata=data.frame(resid=resid(mo), x=dat$lon, y=dat$lat)
        spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
        #cex_=c(1,2,3,3.5,4)
        cex_=c(1,1.5,2,2.5,3)
        spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
        plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8), xlab = 'Longitude', ylab = 'Latitude')
        legend("topright", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)))
        plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('residuals <0'), xlab = 'Longitude', ylab = 'Latitude')
        plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('residual >=0'), xlab = 'Longitude', ylab = 'Latitude')
    }

    mtext(stringr::str_wrap(paste(paste0(name," model: "), slot(mo,"call")[1],'(',slot(mo,"call")[2],sep=''), width = ceiling(nchar(paste(slot(mo,"call")[1],'(',slot(mo,"call")[2],sep=''))/2)+10), side = 3, line = 1, cex=0.5,outer = TRUE, col = 'darkblue') #ceiling(nchar(paste(slot(mo,"call")[1],'(',slot(mo,"call")[2],sep=''))/2)

    if(PNG==TRUE){invisible(dev.off())}

}

# function (from https://stackoverflow.com/questions/56728644/how-to-remove-significance-stars-from-chart-correlation-function)
    chart.Correlation.nostars <- function (R, histogram = TRUE, method = c("pearson", "kendall", "spearman"), ...)
    {
      x = checkData(R, method = "matrix")
      if (missing(method))
        method = method[1]
      panel.cor <- function(x, y, digits = 2, prefix = "",
                            use = "pairwise.complete.obs", method = "pearson",
                            cex.cor, ...) {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- cor(x, y, use = use, method = method)
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste(prefix, txt, sep = "")
        if (missing(cex.cor))
          cex <- 0.8/strwidth(txt)
        test <- cor.test(as.numeric(x), as.numeric(y), method = method)
        # Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
        #                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***",
        #                                                                           "**", "*", ".", " "))
        text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.3)/1.3)
        # text(0.8, 0.8, Signif, cex = cex, col = 2)
      }
      f <- function(t) {
        dnorm(t, mean = mean(x), sd = sd.xts(x))
      }
      dotargs <- list(...)
      dotargs$method <- NULL
      rm(method)
      hist.panel = function(x, ... = NULL) {
        par(new = TRUE)
        hist(x, col = "light gray", probability = TRUE,
             axes = FALSE, main = "", breaks = "FD")
        lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
        rug(x)
      }
      if (histogram)
        pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor,
              diag.panel = hist.panel)
      else pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor)
    }
  # modified pairs_panels - adjusting coefficient size, line colors and using loess also when no CIs are drawn
   pairs.panels_MB<- function (x, smooth = TRUE, scale = TRUE, density = TRUE, ellipses = FALSE,
    digits = 2, method = "pearson", pch = 20, lm = FALSE, cor = TRUE,
    jiggle = FALSE, factor = 2, hist.col = "light gray",dens.col = 'red',smooth.col ='red', show.points = TRUE,
    rug = TRUE, breaks = "Sturges", cex.cor = 1, wt = NULL, smoother = FALSE,
    stars = FALSE, ci = FALSE, alpha = 0.05, ...)
    {
    "panel.hist.density" <- function(x, ...) {
        usr <- par("usr")
        on.exit(par("usr"))
        par(usr = c(usr[1], usr[2], 0, 1.5), tck = -0.03, mgp=c(2,0.4,0), las = 1)
        tax <- table(x)
        if (length(tax) < 11) {
            breaks <- as.numeric(names(tax))
            y <- tax/max(tax)
            interbreak <- min(diff(breaks)) * (length(tax) -
                1)/41
            rect(breaks - interbreak, 0, breaks + interbreak,
                y, col = hist.col)
        }
        else {
            h <- hist(x, breaks = breaks, plot = FALSE)
            breaks <- h$breaks
            nB <- length(breaks)
            y <- h$counts
            y <- y/max(y)
            rect(breaks[-nB], 0, breaks[-1], y, col = hist.col)
        }
        if (density) {
            tryd <- try(d <- density(x, na.rm = TRUE, bw = "nrd",
                adjust = 1.2), silent = TRUE)
            if (!inherits(tryd, "try-error")) {
                d$y <- d$y/max(d$y)
                lines(d, col=dens.col)
            }
        }
        if (rug)
            rug(x)
    }
    "panel.cor" <- function(x, y, prefix = "", ...) {
        usr <- par("usr")
        on.exit(par("usr"))
        par(usr = c(0, 1, 0, 1))
        if (is.null(wt)) {
            r <- cor(x, y, use = "pairwise", method = method)
        }
        else {
            r <- cor.wt(data.frame(x, y), w = wt[, c(1:2)])$r[1,
                2]
        }
        txt <- format(c(round(r, digits), 0.123456789), digits = digits)[1]
        txt <- paste(prefix, txt, sep = "")
        if (stars) {
            pval <- r.test(sum(!is.na(x * y)), r)$p
            symp <- symnum(pval, corr = FALSE, cutpoints = c(0,
                0.001, 0.01, 0.05, 1), symbols = c("***", "**",
                "*", " "), legend = FALSE)
            txt <- paste0(txt, symp)
        }
        cex <- cex.cor * 0.8/(max(strwidth("0.12***"), strwidth(txt)))
        if (scale) {
            cex1 <- cex * (abs(r) + 0.6)/1.3# changed by MB from cex1 <- cex * abs(r)
            if (cex1 < 0.25)
                cex1 <- 0.25
            text(0.5, 0.5, txt, cex = cex1)
        }
        else {
            text(0.5, 0.5, txt, cex = cex)
        }
    }
    "panel.smoother" <- function(x, y, pch = par("pch"), col.smooth = smooth.col,
        span = 2/3, iter = 3, ...) {
        xm <- mean(x, na.rm = TRUE)
        ym <- mean(y, na.rm = TRUE)
        xs <- sd(x, na.rm = TRUE)
        ys <- sd(y, na.rm = TRUE)
        r = cor(x, y, use = "pairwise", method = method)
        if (jiggle) {
            x <- jitter(x, factor = factor)
            y <- jitter(y, factor = factor)
        }
        if (smoother) {
            smoothScatter(x, y, add = TRUE, nrpoints = 0)
        }
        else {
            if (show.points)
                points(x, y, pch = pch, ...)
        }
        ok <- is.finite(x) & is.finite(y)
        if (any(ok)) {
            if (smooth & ci) {
                lml <- loess(y ~ x, degree = 1, family = "symmetric")
                tempx <- data.frame(x = seq(min(x, na.rm = TRUE),
                  max(x, na.rm = TRUE), length.out = 47))
                pred <- predict(lml, newdata = tempx, se = TRUE)
                if (ci) {
                  upperci <- pred$fit + confid * pred$se.fit
                  lowerci <- pred$fit - confid * pred$se.fit
                  polygon(c(tempx$x, rev(tempx$x)), c(lowerci,
                    rev(upperci)), col = adjustcolor(smooth.col,
                    alpha.f = 0.5), border = NA)
                }
                lines(tempx$x, pred$fit, col = smooth.col, ...)
            }
            else {
                if (smooth)
                lml <- loess(y ~ x, degree = 1, family = "symmetric")
                tempx <- data.frame(x = seq(min(x, na.rm = TRUE),
                 max(x, na.rm = TRUE), length.out = 47))
                pred <- predict(lml, newdata = tempx, se = TRUE)
                lines(tempx$x, pred$fit, col = smooth.col, ...)
                  # original below
                  #lines(stats::lowess(x[ok], y[ok], f = span,
                    #iter = iter), col = "pink")
            }
        }
        if (ellipses)
            draw.ellipse(xm, ym, xs, ys, r, col.smooth = col.smooth,
                ...)
    }
    "panel.lm" <- function(x, y, pch = par("pch"), col.lm = "red",
        ...) {
        ymin <- min(y)
        ymax <- max(y)
        xmin <- min(x)
        xmax <- max(x)
        ylim <- c(min(ymin, xmin), max(ymax, xmax))
        xlim <- ylim
        if (jiggle) {
            x <- jitter(x, factor = factor)
            y <- jitter(y, factor = factor)
        }
        if (smoother) {
            smoothScatter(x, y, add = TRUE, nrpoints = 0)
        }
        else {
            if (show.points) {
                points(x, y, pch = pch, ylim = ylim, xlim = xlim,
                  ...)
            }
        }
        ok <- is.finite(x) & is.finite(y)
        if (any(ok)) {
            lml <- lm(y ~ x)
            if (ci) {
                tempx <- data.frame(x = seq(min(x, na.rm = TRUE),
                  max(x, na.rm = TRUE), length.out = 47))
                pred <- predict.lm(lml, newdata = tempx, se.fit = TRUE)
                upperci <- pred$fit + confid * pred$se.fit
                lowerci <- pred$fit - confid * pred$se.fit
                polygon(c(tempx$x, rev(tempx$x)), c(lowerci,
                  rev(upperci)), col = adjustcolor("light grey",
                  alpha.f = 0.8), border = NA)
            }
            if (ellipses) {
                xm <- mean(x, na.rm = TRUE)
                ym <- mean(y, na.rm = TRUE)
                xs <- sd(x, na.rm = TRUE)
                ys <- sd(y, na.rm = TRUE)
                r = cor(x, y, use = "pairwise", method = method)
                draw.ellipse(xm, ym, xs, ys, r, col.smooth = col.lm,
                  ...)
            }
            abline(lml, col = col.lm, ...)
        }
    }
    "draw.ellipse" <- function(x = 0, y = 0, xs = 1, ys = 1,
        r = 0, col.smooth, add = TRUE, segments = 51, ...) {
        angles <- (0:segments) * 2 * pi/segments
        unit.circle <- cbind(cos(angles), sin(angles))
        if (!is.na(r)) {
            if (abs(r) > 0)
                theta <- sign(r)/sqrt(2)
            else theta = 1/sqrt(2)
            shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta,
                theta, -theta, theta), ncol = 2, byrow = TRUE)
            ellipse <- unit.circle %*% shape
            ellipse[, 1] <- ellipse[, 1] * xs + x
            ellipse[, 2] <- ellipse[, 2] * ys + y
            if (show.points)
                points(x, y, pch = 19, col = col.smooth, cex = 1.5)
            lines(ellipse, ...)
        }
    }
    "panel.ellipse" <- function(x, y, pch = par("pch"), col.smooth = "red",
        ...) {
        segments = 51
        usr <- par("usr")
        on.exit(par("usr"))
        par(usr = c(usr[1] - abs(0.05 * usr[1]), usr[2] + abs(0.05 *
            usr[2]), 0, 1.5))
        xm <- mean(x, na.rm = TRUE)
        ym <- mean(y, na.rm = TRUE)
        xs <- sd(x, na.rm = TRUE)
        ys <- sd(y, na.rm = TRUE)
        r = cor(x, y, use = "pairwise", method = method)
        if (jiggle) {
            x <- jitter(x, factor = factor)
            y <- jitter(y, factor = factor)
        }
        if (smoother) {
            smoothScatter(x, y, add = TRUE, nrpoints = 0)
        }
        else {
            if (show.points) {
                points(x, y, pch = pch, ...)
            }
        }
        angles <- (0:segments) * 2 * pi/segments
        unit.circle <- cbind(cos(angles), sin(angles))
        if (!is.na(r)) {
            if (abs(r) > 0)
                theta <- sign(r)/sqrt(2)
            else theta = 1/sqrt(2)
            shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta,
                theta, -theta, theta), ncol = 2, byrow = TRUE)
            ellipse <- unit.circle %*% shape
            ellipse[, 1] <- ellipse[, 1] * xs + xm
            ellipse[, 2] <- ellipse[, 2] * ys + ym
            points(xm, ym, pch = 19, col = col.smooth, cex = 1.5)
            if (ellipses)
                lines(ellipse, ...)
        }
    }
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    if (missing(cex.cor))
        cex.cor <- 1
    for (i in 1:ncol(x)) {
        if (is.character(x[[i]])) {
            x[[i]] <- as.numeric(as.factor(x[[i]]))
            colnames(x)[i] <- paste(colnames(x)[i], "*", sep = "")
        }
    }
    n.obs <- nrow(x)
    confid <- qt(1 - alpha/2, n.obs - 2)
    if (!lm) {
        if (cor) {
            pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor,
                lower.panel = panel.smoother, pch = pch, ...)
        }
        else {
            pairs(x, diag.panel = panel.hist.density, upper.panel = panel.smoother,
                lower.panel = panel.smoother, pch = pch, ...)
        }
    }
    else {
        if (!cor) {
            pairs(x, diag.panel = panel.hist.density, upper.panel = panel.lm,
                lower.panel = panel.lm, pch = pch, ...)
        }
        else {
            pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor,
                lower.panel = panel.lm, pch = pch, ...)
        }
    }
    }

