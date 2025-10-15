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
# vignette(topic = "pccc", package = "medicalcoder")
# vignette(topic = "charlson", package = "medicalcoder")
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

## ----label = "other-vignettes", eval = FALSE----------------------------------
# vignette(package = "medicalcoder")$results

## -----------------------------------------------------------------------------
str(get_elixhauser_poa())

## ----label = "example-setup"--------------------------------------------------
lookup_icd_codes(c("C78.4", "I50.40"))

subset(get_pccc_codes(),
       subset = full_code %in% c("C78.4", "I50.40"),
       select = c("icdv", "dx", "code", "full_code", "condition", "pccc_v3.0"))

subset(get_charlson_codes(),
       subset = full_code %in% c("C78.4", "I50.40"),
       select = c("icdv", "dx", "code", "full_code", "condition", "charlson_quan2011"))

subset(get_elixhauser_codes(),
       subset = full_code %in% c("C78.4", "I50.40") & elixhauser_ahrq2025 == 1L,
       select = c("icdv", "dx", "code", "full_code", "condition", "elixhauser_ahrq2025"))

record <-
data.table::fread(text = "
patid | encid | code   | poa
A     | 1     | NA     | NA
A     | 2     | C78.4  | 0
A     | 3     | I50.40 | 1
A     | 4     | NA     | NA
A     | 5     | C78.4  | 1
A     | 5     | I50.40 | 0
A     | 6     | NA     | NA
")

## ----include = FALSE----------------------------------------------------------
args <-
  list(data = record,
       icd.codes = "code",
       id.vars = c("patid", "encid"),
       icdv = 10L,
       dx = 1,
       primarydx = 0L
  )
args_current_poa0 <- c(args, poa = 0L,        flag.method = "current")
args_current_poa1 <- c(args, poa = 1L,        flag.method = "current")
args_current_poav <- c(args, poa.var = "poa", flag.method = "current")
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
              do.call(comorbidities, c(args_current_poa0, method = "pccc_v3.0"))[,           .(CVD = cvd_dxpr_or_tech, CANCER = malignancy_dxpr_or_tech)],
              do.call(comorbidities, c(args_current_poa0, method = "charlson_quan2011"))[,   .(CVD = chf,              CANCER = mst)],
              do.call(comorbidities, c(args_current_poa0, method = "elixhauser_ahrq2025"))[, .(CVD = HF,               CANCER = CANCER_METS)],
              do.call(comorbidities, c(args_current_poa1, method = "pccc_v3.0"))[,           .(CVD = cvd_dxpr_or_tech, CANCER = malignancy_dxpr_or_tech)],
              do.call(comorbidities, c(args_current_poa1, method = "charlson_quan2011"))[,   .(CVD = chf,              CANCER = mst)],
              do.call(comorbidities, c(args_current_poa1, method = "elixhauser_ahrq2025"))[, .(CVD = HF,               CANCER = CANCER_METS)],
              do.call(comorbidities, c(args_current_poav, method = "pccc_v3.0"))[,           .(CVD = cvd_dxpr_or_tech, CANCER = malignancy_dxpr_or_tech)],
              do.call(comorbidities, c(args_current_poav, method = "charlson_quan2011"))[,   .(CVD = chf,              CANCER = mst)],
              do.call(comorbidities, c(args_current_poav, method = "elixhauser_ahrq2025"))[, .(CVD = HF,               CANCER = CANCER_METS)]
    ))
  ,
    do.call(cbind,
            list(
              left_cols,
              do.call(comorbidities, c(args_cumulative_poa0, method = "pccc_v3.0"))[,           .(CVD = cvd_dxpr_or_tech, CANCER = malignancy_dxpr_or_tech)],
              do.call(comorbidities, c(args_cumulative_poa0, method = "charlson_quan2011"))[,   .(CVD = chf,              CANCER = mst)],
              do.call(comorbidities, c(args_cumulative_poa0, method = "elixhauser_ahrq2025"))[, .(CVD = HF,               CANCER = CANCER_METS)],
              do.call(comorbidities, c(args_cumulative_poa1, method = "pccc_v3.0"))[,           .(CVD = cvd_dxpr_or_tech, CANCER = malignancy_dxpr_or_tech)],
              do.call(comorbidities, c(args_cumulative_poa1, method = "charlson_quan2011"))[,   .(CVD = chf,              CANCER = mst)],
              do.call(comorbidities, c(args_cumulative_poa1, method = "elixhauser_ahrq2025"))[, .(CVD = HF,               CANCER = CANCER_METS)],
              do.call(comorbidities, c(args_cumulative_poav, method = "pccc_v3.0"))[,           .(CVD = cvd_dxpr_or_tech, CANCER = malignancy_dxpr_or_tech)],
              do.call(comorbidities, c(args_cumulative_poav, method = "charlson_quan2011"))[,   .(CVD = chf,              CANCER = mst)],
              do.call(comorbidities, c(args_cumulative_poav, method = "elixhauser_ahrq2025"))[, .(CVD = HF,               CANCER = CANCER_METS)]
    ))
  )

## ----echo = FALSE, results = "asis"-------------------------------------------
tab <-
  kbl(
    rtn,
    row.names = FALSE,
    escape = FALSE,
    caption = "Indicators for when a comorbidity is flagged based on the algorithm, present on admission (poa), and flag.method. The two ICD codes,C78.4 and I50.40, map to cancer and cardiovascular disease respectively."
  )
tab <-
  footnote(
    tab,
    symbol = c("Present on Admission"),
    general = "C78.4 does not need to be POA to count for Elixhauser. I50.40 does need to be POA to count for Elixhauser."
  )

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
                poa = 1)
data.table::setDT(cmdf_mdcr)

cmdf_mdcr[, .N, keyby = .(hiv, aids)]

