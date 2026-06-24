# ==========================================================================
# ❗ The script runs relative to the project's root directory,
# (a) loads `song_complexity_repeatability.txt`and estimates between
# observer repeatability, and (b) loads 'Data/song_raw.csv' and estimates
# within species repeatability in song complexity, while exporting both into
# Data/DAT_repeatability.csv
# ==========================================================================

# PACKAGES, SETTINGS
source(here::here("R/__init__.R"))

require(data.table) # install.packages("data.table")
require(magrittr)
require("rptR") # install.packages("rptR")
require("qs") # install.packages("qs")

n = 1000

CHECK WHAT is wrong with the code
# BETWEEN observers
  b <- fread(here::here("Data/song_complexity_repeatability.txt"))
  bb <- melt(
    b,
    id.vars = "Song_number",
    measure.vars = c(
      "Reference_level", "Vol1", "Vol2", "Vol3",
      "Vol4", "Vol5", "Vol6", "Vol7"
    ),
    variable.name = "observer",
    value.name = "complexity"
  )

  ro1 <- rpt(log10(complexity) ~ (1 | Song_number),
    grname = "Song_number",
    data = bb, datatype = "Gaussian",
    nboot = n,
    npermut = 0
  )

  r = ro1
  RR <- data.table(merge(data.frame(complexity = "original", scale = "log10"), paste0(round(r$R * 100, 1), "%"))) %>% setnames(new = c("complexity", "scale", "repeatability"))
  RR[, CI := paste0(paste(round(r$CI_emp * 100, 1)[1], round(r$CI_emp * 100, 1)[2], sep = "-"), "%")]
  RR[, pred := 100 * r$R]
  RR[, lwr := 100 * r$CI_emp[1]]
  RR[, upr := 100 * r$CI_emp[2]]
  RR$type = "between observers"
  ro_l <- RR

  ro2 <- rpt(complexity ~ (1 | Song_number),
    grname = "Song_number",
    data = bb, datatype = "Gaussian",
    nboot = n,
    npermut = 0
  )
  r = ro2
  RR <- data.table(merge(data.frame(complexity = "original", scale = "original"), paste0(round(r$R * 100, 1), "%"))) %>% setnames(new = c("complexity", "scale", "repeatability"))
  RR[, CI := paste0(paste(round(r$CI_emp * 100, 1)[1], round(r$CI_emp * 100, 1)[2], sep = "-"), "%")]
  RR[, pred := 100 * r$R]
  RR[, lwr := 100 * r$CI_emp[1]]
  RR[, upr := 100 * r$CI_emp[2]]
  RR$type = "between observers"
  ro_o <- RR

# within species

  # data
    d <- fread(here::here("Data/song_raw.csv"))[!is.na(elements) & rec_quality_subj %in% c("A", "B")]

  # original values
    r1 <- rpt(log10(element_types) ~ (1 | scinam),
      grname = "scinam",
      data = d, datatype = "Gaussian",
      nboot = n,
      npermut = 0
    )
    r = r1
    RR <- data.table(merge(data.frame(complexity = "original", scale = "log10"), paste0(round(r$R * 100, 1), "%"))) %>% setnames(new = c("complexity", "scale", "repeatability"))
    RR[, CI := paste0(paste(round(r$CI_emp * 100, 1)[1], round(r$CI_emp * 100, 1)[2], sep = "-"), "%")]
    RR[, pred := 100 * r$R]
    RR[, lwr := 100 * r$CI_emp[1]]
    RR[, upr := 100 * r$CI_emp[2]]
    RR$type = 'within species'
    r_l <- RR

    r2 <- rpt(element_types ~  (1 | scinam),
      grname = "scinam",
      data = d, datatype = "Gaussian",
      nboot = 100,
      npermut = 0
    )
    r = r2
    RR <- data.table(merge(data.frame(complexity = "original", scale = "original"), paste0(round(r$R * 100, 1), "%"))) %>% setnames(new = c("complexity", "scale", "repeatability"))
    RR[, CI := paste0(paste(round(r$CI_emp * 100, 1)[1], round(r$CI_emp * 100, 1)[2], sep = "-"), "%")]
    RR[, pred := 100 * r$R]
    RR[, lwr := 100 * r$CI_emp[1]]
    RR[, upr := 100 * r$CI_emp[2]]
    RR$type = "within species"
    r_o <- RR

  # extrapolated values
  # add extrapolated number of element types (see Methods for details)
    d[, element_types_extrapol := 10^(log10(element_types) + 0.2644431 * (log10(50) - log10(elements)))]
    r3 <- rpt(log10(element_types_extrapol) ~ (1 | scinam),
      grname = "scinam",
      data = d, datatype = "Gaussian",
      nboot = n,
      npermut = 0
    )
    r = r3
    RR <- data.table(merge(data.frame(complexity = "extrapolated", scale = "log10"), paste0(round(r$R * 100, 1), "%"))) %>% setnames(new = c("complexity", "scale", "repeatability"))
    RR[, CI := paste0(paste(round(r$CI_emp * 100, 1)[1], round(r$CI_emp * 100, 1)[2], sep = "-"), "%")]
    RR[, pred := 100 * r$R]
    RR[, lwr := 100 * r$CI_emp[1]]
    RR[, upr := 100 * r$CI_emp[2]]
    RR$type = "within species"
    e_l <- RR

    r4 <- rpt(element_types_extrapol ~ (1 | scinam),
      grname = "scinam",
      data = d, datatype = "Gaussian",
      nboot = n,
      npermut = 0
    )
    r = r4
    RR <- data.table(merge(data.frame(complexity = "extrapolated", scale = "original"), paste0(round(r$R * 100, 1), "%"))) %>% setnames(new = c("complexity", "scale", "repeatability"))
    RR[, CI := paste0(paste(round(r$CI_emp * 100, 1)[1], round(r$CI_emp * 100, 1)[2], sep = "-"), "%")]
    RR[, pred := 100 * r$R]
    RR[, lwr := 100 * r$CI_emp[1]]
    RR[, upr := 100 * r$CI_emp[2]]
    RR$type = "within species"
    e_o <- RR

# EXPORT
  fwrite(file = here::here('Data/DAT_repeatability.csv'), rbind(ro_o, ro_l, r_o, r_l, e_o, e_l))

# sessionInfo

#R version 4.5.2 (2025-10-31)
#Platform: aarch64-apple-darwin25.0.0
#Running under: macOS Tahoe 26.5.1

#Matrix products: default
#BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
#LAPACK: /opt/homebrew/Cellar/r/4.5.2_1/lib/R/lib/libRlapack.dylib;  LAPACK version 3.12.1

#locale:
#[1] C.UTF-8/C.UTF-8/C.UTF-8/C/C.UTF-8/C.UTF-8

#time zone: Europe/Prague
#tzcode source: internal

#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base

#other attached packages:
#[1] magrittr_2.0.4      rptR_0.9.23         qs_0.27.3           data.table_1.18.2.1

#loaded via a namespace (and not attached):
# [1] nlme_3.1-169          cli_3.6.6             import_1.3.4          rlang_1.2.0           otel_0.2.0            reformulas_0.4.4      DBI_1.3.0             minqa_1.2.8           RcppParallel_5.1.11-2 rprojroot_2.1.1
#[11] RApiSerialize_0.1.4   stringfish_0.18.0     lme4_2.0-1            grid_4.5.2            MASS_7.3-65           compiler_4.5.2        Rcpp_1.1.1            here_1.0.2            pbapply_1.7-4         lattice_0.22-9
#[21] nloptr_2.2.1          Rdpack_2.6.6          parallel_4.5.2        splines_4.5.2         rbibutils_2.4.1       Matrix_1.7-4          tools_4.5.2           boot_1.3-32
