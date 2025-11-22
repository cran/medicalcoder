## ----label = "setup", include = FALSE-----------------------------------------
# IMPORTANT SYNTAX NOTE:
#
# DO NOT USE the pipeOp `|>`
#
# While convenient, that is a R 4.1.0 feature at a minimum. Notable improvements
# to the pipeOp come in 4.2.0 and 4.2.1.  To keep this package dependent on R >=
# 3.5.0 do not use the pipeOp.

library(kableExtra)
knitr::opts_chunk$set(collapse = TRUE, fig.align = "center")
options(qwraps2_markup = "markdown")

## ----label = 'medicalcoder-namespace'-----------------------------------------
library(medicalcoder)
packageVersion("medicalcoder")

## ----label = "valid-methods"--------------------------------------------------
medicalcoder:::comorbidities_methods()

## ----echo = TRUE, eval = FALSE------------------------------------------------
# vignette(topic = "pccc",       package = "medicalcoder")
# vignette(topic = "charlson",   package = "medicalcoder")
# vignette(topic = "elixhauser", package = "medicalcoder")

## ----label = 'test-that-the-args-have-not-changed', include = FALSE-----------
# IF THIS FAILS YOU NEED TO MAKE SURE THE DOCUMENTATION IN THIS VIGNETTE IS
# UPTO DATE
# dput(capture.output(args(comorbidities)))
stopifnot(
  capture.output(args(comorbidities))  ==
    c("function (data, icd.codes, method, id.vars = NULL, icdv.var = NULL, ",
      "    icdv = NULL, dx.var = NULL, dx = NULL, poa.var = NULL, poa = NULL, ",
      "    age.var = NULL, primarydx.var = NULL, primarydx = NULL, flag.method = c(\"current\", ",
      "        \"cumulative\"), full.codes = TRUE, compact.codes = TRUE, ",
      "    subconditions = FALSE) ", "NULL")
)

## ----label = "read-the-manual", eval = FALSE----------------------------------
# help(topic = "comorbidities", package = "medicalcoder")

## ----label = "comorbidities-args"---------------------------------------------
args(comorbidities)

## ----label = "example-data"---------------------------------------------------
head(mdcr)
head(mdcr_longitudinal)

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
str(get_elixhauser_poa())

## ----label = "example-setup", include = FALSE---------------------------------
lookup_icd_codes(c("C78.4", "I50.40"))

codes <- c("C78.4", "I50.40")
cols  <- c("icdv", "dx", "code", "full_code", "condition")

subset(
  get_pccc_codes(),
  subset = full_code %in% codes,
  select = c(cols, "pccc_v3.0")
)

subset(
  get_charlson_codes(),
  subset = full_code %in% codes,
  select = c(cols, "charlson_quan2011")
)

subset(
  get_elixhauser_codes(),
  subset = full_code %in% codes & elixhauser_ahrq2025 == 1L,
  select = c(cols, "elixhauser_ahrq2025")
)

record <-
  structure(
    list(
      patid = c("A", "A", "A", "A", "A", "A", "A"),
      encid = c(1L, 2L, 3L, 4L, 5L, 5L, 6L),
      code = c(NA, "C78.4", "I50.40", NA, "C78.4", "I50.40", NA),
      poa = c(NA, 0L, 1L, NA, 1L, 0L, NA)),
    row.names = c(NA, -7L),
    class = "data.frame"
  )

## ----include = FALSE----------------------------------------------------------
data.table::setDT(record)
args <-
  list(data = record,
       icd.codes = "code",
       id.vars = c("patid", "encid"),
       icdv = 10L,
       dx = 1
  )
args_current_poa0    <- c(args, poa = 0L,        flag.method = "current")
args_current_poa1    <- c(args, poa = 1L,        flag.method = "current")
args_current_poav    <- c(args, poa.var = "poa", flag.method = "current")
args_cumulative_poa0 <- c(args, poa = 0L,        flag.method = "cumulative")
args_cumulative_poa1 <- c(args, poa = 1L,        flag.method = "cumulative")
args_cumulative_poav <- c(args, poa.var = "poa", flag.method = "cumulative")

left_cols <-
  cbind(encid = 1L:6L,
        ICD = c("", "C78.4", paste0("I50.40", footnote_marker_symbol(1L)),
                "", paste0("C78.4", footnote_marker_symbol(1L), "; I50.40"), ""))

rtn <-
  rbind(
    do.call(cbind,
            list(
              left_cols,
              do.call(comorbidities, c(args_current_poa0,                 method = "pccc_v3.0"))[,           .(CVD = cvd_dxpr_or_tech, CANCER = malignancy_dxpr_or_tech)],
              do.call(comorbidities, c(args_current_poa0, primarydx = 0L, method = "charlson_quan2011"))[,   .(CVD = chf,              CANCER = mst)],
              do.call(comorbidities, c(args_current_poa0, primarydx = 0L, method = "elixhauser_ahrq2025"))[, .(CVD = HF,               CANCER = CANCER_METS)],
              do.call(comorbidities, c(args_current_poa1,                 method = "pccc_v3.0"))[,           .(CVD = cvd_dxpr_or_tech, CANCER = malignancy_dxpr_or_tech)],
              do.call(comorbidities, c(args_current_poa1, primarydx = 0L, method = "charlson_quan2011"))[,   .(CVD = chf,              CANCER = mst)],
              do.call(comorbidities, c(args_current_poa1, primarydx = 0L, method = "elixhauser_ahrq2025"))[, .(CVD = HF,               CANCER = CANCER_METS)],
              do.call(comorbidities, c(args_current_poav,                 method = "pccc_v3.0"))[,           .(CVD = cvd_dxpr_or_tech, CANCER = malignancy_dxpr_or_tech)],
              do.call(comorbidities, c(args_current_poav, primarydx = 0L, method = "charlson_quan2011"))[,   .(CVD = chf,              CANCER = mst)],
              do.call(comorbidities, c(args_current_poav, primarydx = 0L, method = "elixhauser_ahrq2025"))[, .(CVD = HF,               CANCER = CANCER_METS)]
    ))
  ,
    do.call(cbind,
            list(
              left_cols,
              do.call(comorbidities, c(args_cumulative_poa0,                 method = "pccc_v3.0"))[,           .(CVD = cvd_dxpr_or_tech, CANCER = malignancy_dxpr_or_tech)],
              do.call(comorbidities, c(args_cumulative_poa0, primarydx = 0L, method = "charlson_quan2011"))[,   .(CVD = chf,              CANCER = mst)],
              do.call(comorbidities, c(args_cumulative_poa0, primarydx = 0L, method = "elixhauser_ahrq2025"))[, .(CVD = HF,               CANCER = CANCER_METS)],
              do.call(comorbidities, c(args_cumulative_poa1,                 method = "pccc_v3.0"))[,           .(CVD = cvd_dxpr_or_tech, CANCER = malignancy_dxpr_or_tech)],
              do.call(comorbidities, c(args_cumulative_poa1, primarydx = 0L, method = "charlson_quan2011"))[,   .(CVD = chf,              CANCER = mst)],
              do.call(comorbidities, c(args_cumulative_poa1, primarydx = 0L, method = "elixhauser_ahrq2025"))[, .(CVD = HF,               CANCER = CANCER_METS)],
              do.call(comorbidities, c(args_cumulative_poav,                 method = "pccc_v3.0"))[,           .(CVD = cvd_dxpr_or_tech, CANCER = malignancy_dxpr_or_tech)],
              do.call(comorbidities, c(args_cumulative_poav, primarydx = 0L, method = "charlson_quan2011"))[,   .(CVD = chf,              CANCER = mst)],
              do.call(comorbidities, c(args_cumulative_poav, primarydx = 0L, method = "elixhauser_ahrq2025"))[, .(CVD = HF,               CANCER = CANCER_METS)]
    ))
  )

## ----echo = FALSE, results = "asis"-------------------------------------------
tab <-
  kbl(
    rtn,
    row.names = FALSE,
    escape = FALSE,
    align = rep("c", nrow(rtn)),
    caption = "Indicators for when a comorbidity is flagged based on the algorithm, present on admission (poa), and flag.method. The two ICD codes, C78.4 and I50.40, map to cancer and cardiovascular disease respectively."
  )
tab <-
  footnote(
    tab,
    symbol = c("Present on Admission"),
    general = "C78.4 does not need to be POA to count for Elixhauser. I50.40 does need to be POA to count for Elixhauser."
  )

tab <- kable_styling(tab, bootstrap_options = c("striped", "bordered"), full_width = FALSE, font_size = 8)
#tab <- scroll_box(tab, height = "600px")
tab <- pack_rows(tab, "flag.method = 'current'", 1L, 6L)
tab <- pack_rows(tab, "flag.method = 'cumulative'", 7L, 12L)
tab <- add_header_above(tab, c(" " = 2L, rep(c("PCCC" = 2L, "Charlson" = 2L, "Elixhauser" = 2L), 3L)))
tab <- add_header_above(tab, c(" " = 2L, c("POA = 0" = 6L, "POA = 1" = 6L, "poa.var = 'poa'" = 6L)))
tab

## ----label = "get-codes"------------------------------------------------------
str(get_pccc_codes())
str(get_charlson_codes())
str(get_elixhauser_codes())

## -----------------------------------------------------------------------------
cdmf_eg <-
  merge(x = mdcr,
        y = subset(get_charlson_codes(),
                   condition %in% c("aids", "hiv") &
                   charlson_cdmf2019 == 1),
        by = c("icdv", "dx", "code"))
data.table::setDT(cdmf_eg)

cdmf_eg <-
  data.table::dcast(data = cdmf_eg,
                    patid ~ condition,
                    value.var = "charlson_cdmf2019",
                    fun.aggregate = function(x) {as.integer(sum(x) > 0)})

cdmf_eg[, .N, keyby = .(hiv, aids)]

## -----------------------------------------------------------------------------
cmdf_mdcr <-
  comorbidities(data = mdcr,
                icd.codes = "code",
                id.vars = "patid",
                icdv.var = "icdv",
                dx.var = "dx",
                method = "charlson_cdmf2019",
                flag.method = "current",
                primarydx = 0L,
                poa = 1L)
data.table::setDT(cmdf_mdcr)

cmdf_mdcr[, .N, keyby = .(hiv, aids)]

