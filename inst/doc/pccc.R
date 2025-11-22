## ----label = "setup", include = FALSE-----------------------------------------
# IMPORTANT SYNTAX NOTE:
#
# DO NOT USE the pipeOp `|>`
#
# While convenient, that is a R 4.1.0 feature at a minimum. Notable improvements
# to the pipeOp come in 4.2.0 and 4.2.1.  To keep this package dependent on R >=
# 3.5.0 do not use the pipeOp.

library(kableExtra)
options(qwraps2_markup = "markdown")
options(knitr.kable.NA = '')
knitr::opts_chunk$set(collapse = TRUE, fig.align = "center")

## ----label = 'medicalcoder-namespace'-----------------------------------------
library(medicalcoder)

## ----label = "tbl-syntactically-valid-conditions", echo = FALSE, results = "asis"----
CNDS <- subset(get_pccc_conditions(), select = c("condition", "condition_label"))
data.table::setDT(CNDS)
data.table::setkey(CNDS, condition)
CNDS <- unique(CNDS)

tab <-
  kbl(
    CNDS,
    caption = "Syntactically valid names for complex chronic conditions",
    row.names = TRUE
  )
tab <- kable_styling(tab, bootstrap_options = c("striped"), full_width = FALSE, font_size = 10)
tab

## ----label = "get-pccc-codes"-------------------------------------------------
pccc_codes <- get_pccc_codes()
str(pccc_codes)

## ----label = "define-pat1"----------------------------------------------------
pat1 <-
  data.frame(
    dx = c(1, 1, 1, 1, 0, 0),
    icdv = 9L,
    code = c("34590", "78065", "3432", "78065", "9929", "8606")
  )

## ----label = "inner-join-pat1-pccc-codes"-------------------------------------
merge(x = pccc_codes, y = pat1, all = FALSE, by = c("icdv", "dx", "code"))

## ----label = "pat1-pccc-v2"---------------------------------------------------
pat1_pccc_v2.0 <-
  comorbidities(
    data = pat1,
    icd.codes = "code",
    dx.var = "dx",
    icdv = 9,
    method = "pccc_v2.0",
    flag.method = "current", # default
    poa = 1                  # default for flag.method = 'current'
  )

pat1_pccc_v2.1 <-
  comorbidities(
    data = pat1,
    icd.codes = "code",
    dx.var = "dx",
    icdv = 9,
    method = "pccc_v2.1",
    flag.method = "current",
    poa = 1
  )

all.equal(pat1_pccc_v2.0, pat1_pccc_v2.1, check.attributes = FALSE)
pat1_pccc_v2.1

## ----label = "pat1-pccc-v3"---------------------------------------------------
pat1_pccc_v3.0 <-
  comorbidities(
    data = pat1,
    icd.codes = "code",
    dx.var = "dx",
    icdv = 9,
    method = "pccc_v3.0",
    flag.method = 'current',
    poa = 1
  )

pat1_pccc_v3.1 <-
  comorbidities(
    data = pat1,
    icd.codes = "code",
    dx.var = "dx",
    icdv = 9,
    method = "pccc_v3.1",
    flag.method = 'current',
    poa = 1
  )

all.equal(pat1_pccc_v3.0, pat1_pccc_v3.1, check.attributes = FALSE)

# retain the needed columns, there are four columns for each condition in v3
pat1_pccc_v3.0[, grep("^(cmrb_flag|num_cmrb|neuromus|metabolic|tech_dep_flag)", names(pat1_pccc_v3.0))]

## ----echo = FALSE, results = "asis"-------------------------------------------
tab <- data.table::fread(colClass = character(), text = "
cmrb_flag | num_cmrb | <condition>_dxpr_or_tech | <condition>_dxpr_only | <condition>_tech_only | <condition>_dxpr_and_tech | <other condition(s)>_dxpr_or_tech
0         | 0        | 0                        | 0                     | 0                     | 0                         | 0
1         | 1        | 0                        | 0                     | 0                     | 0                         | 1
1         | 1        | 1                        | 1                     | 0                     | 0                         | 0
1         | 1        | 1                        | 1                     | 1                     | 1                         | 0
1         | >1       | 0                        | 0                     | 0                     | 0                         | 1
1         | >1       | 1                        | 0                     | 1                     | 0                         | 1
1         | >1       | 1                        | 1                     | 0                     | 0                         | 1
1         | >1       | 1                        | 1                     | 1                     | 1                         | 1                                  ")

tab <-
  kbl(
    x = tab,
    align = rep("c", ncol(tab)),
    col.names = c("cmrb flag", "num cmrb", "_dxpr_or_tech", "_dxpr_only", "_tech_only", "_dxpr_and_tech", "_dxpr_or_tech")
  )
tab <- kable_styling(tab, bootstrap_options = c("striped"), font_size = 10)
tab <- add_header_above(tab, c("", "", "<condition>" = 4, "<other condition(s)>" = 1))
tab

## ----label = 'define-pat2'----------------------------------------------------
pat2 <- subset(pat1, code != "3432")

## ----label = "pat2-pccc-v2"---------------------------------------------------
pat2_pccc_v2.1 <-
  comorbidities(
    data = pat2,
    icd.codes = "code",
    dx.var = "dx",
    icdv = 9,
    method = "pccc_v2.1",
    flag.method = 'current',
    poa = 1
  )
Filter(f = function(x) x > 0, pat2_pccc_v2.1)

## ----label = "pat2-pccc-v3"---------------------------------------------------
pat2_pccc_v3.1 <-
  comorbidities(
    data = pat2,
    icd.codes = "code",
    dx.var = "dx",
    icdv = 9,
    method = "pccc_v3.1",
    flag.method = 'current',
    poa = 1
  )
Filter(f = function(x) x > 0, pat2_pccc_v3.1)

## ----label = "mdcr-data"------------------------------------------------------
head(mdcr)
str(mdcr)

## ----label = "mdcr-results-01"------------------------------------------------
mdcr_results_v2.1_01 <-
  comorbidities(
    data = mdcr,
    icd.codes = "code",
    id.vars = "patid",
    poa = 1,
    flag.method = 'current',
    method = "pccc_v2.1"
  )

mdcr_results_v3.1_01 <-
  comorbidities(
    data = mdcr,
    icd.codes = "code",
    id.vars = "patid",
    poa = 1,
    flag.method = 'current',
    method = "pccc_v3.1"
  )

## ----label = "comorbidities-summary-table-str"--------------------------------
str(summary(mdcr_results_v2.1_01))
str(summary(mdcr_results_v3.1_01))

## ----label = "mdcr-results-01-summary"----------------------------------------
x <-
  merge(
    summary(mdcr_results_v2.1_01),
    summary(mdcr_results_v3.1_01),
    all = TRUE,
    by = c("condition", "label"),
    sort = FALSE
  )
x[["condition"]] <- NULL

## ----label = "mdcr-results-01-summary-kable", echo = FALSE, results = "asis"----
tab <-
  kableExtra::kbl(
    x,
    digits = 1,
    col.names = c("", rep(c("count", "%"), times = 5)),
    caption = "Summary Table for `mdcr_results_v2.1_01` and `mdcr_results_v3.1_01`."
  )
tab <-
  kableExtra::kable_styling(tab, bootstrap_options = c("striped"), font_size = 10)
tab <-
  kableExtra::pack_rows(tab, group_label = c("Conditions"), start_row = 1, end_row = 11)
tab <-
  kableExtra::pack_rows(tab, group_label = c("Flags"), start_row = 12, end_row = 13)
tab <-
  kableExtra::pack_rows(tab, group_label = c("Total Conditions"), start_row = 14, end_row = 24)
tab <-
  kableExtra::add_header_above(tab, c("", "v2.1" = 2, "dxpr or tech" = 2, "dxpr only" = 2, "tech only" = 2, "dxpr and tech" = 2))
tab <-
  kableExtra::add_header_above(tab, c("", "", "", "v3.1" = 8))
tab

## ----label = "note-dx-pr-in-mdcr"---------------------------------------------
pccc_codes[pccc_codes$code == "3321", ]
table(mdcr[mdcr$code == "3321", "dx"])

## ----label = "mdcr-results-02"------------------------------------------------
mdcr_results_v2.1_02 <-
  comorbidities(
    data = mdcr,
    id.vars = "patid",
    icd.codes = "code",
    dx.var = "dx",
    flag.method = 'current',
    poa = 1,
    method = "pccc_v2.1"
  )

mdcr_results_v3.1_02 <-
  comorbidities(
    data = mdcr,
    id.vars = "patid",
    icd.codes = "code",
    dx.var = "dx",
    flag.method = 'current',
    poa = 1,
    method = "pccc_v3.1"
  )

## -----------------------------------------------------------------------------
# verify that the cmrb_flag and number of conditions is the same or less after
# accounting for the diagnostic/procedure flag in the comorbidities call
stopifnot(all(mdcr_results_v2.1_02$cmrb_flag <= mdcr_results_v2.1_01$cmrb_flag))
stopifnot(all(mdcr_results_v2.1_02$num_cmrb  <= mdcr_results_v2.1_01$num_cmrb))

sum(mdcr_results_v2.1_02$cmrb_flag != mdcr_results_v2.1_01$cmrb_flag)
sum(mdcr_results_v2.1_02$num_cmrb  != mdcr_results_v2.1_01$num_cmrb)

stopifnot(all(mdcr_results_v3.1_02$cmrb_flag <= mdcr_results_v3.1_01$cmrb_flag))
stopifnot(all(mdcr_results_v3.1_02$num_cmrb  <= mdcr_results_v3.1_01$num_cmrb))

sum(mdcr_results_v3.1_02$cmrb_flag != mdcr_results_v3.1_01$cmrb_flag)
sum(mdcr_results_v3.1_02$num_cmrb  != mdcr_results_v3.1_01$num_cmrb)

## ----include = FALSE----------------------------------------------------------
# this chunk is not included, just test that the results for patient 87420 has
# not changed.
#
# NOTE: vignettes are built in one R session.  As a result, in a prior version
# of this package, mdcr was modified in the icd.Rmd script. Those changes
# persisted into this vignette and resulted in an error.  The fix was to use an
# object in the icd.Rmd vignette called mdcr_copy.
subset(mdcr, patid %in% mdcr[mdcr$code == "5641" & mdcr$dx == 1, "patid"])
pat87420 <- subset(mdcr, patid == 87420)
stopifnot(
  isTRUE(
    all.equal(
      pat87420,
      structure(list(patid = c(87420L, 87420L), icdv = c(9L, 9L), code = c("78321", "5641"), dx = c(1L, 1L)), row.names = 4073:4074, class = "data.frame"),
      check.attributes = FALSE
    )
  )
)

## -----------------------------------------------------------------------------
subset(mdcr, patid == "87420")
subset(get_pccc_codes(), code %in% c("78321", "5641"))

## -----------------------------------------------------------------------------
subset(mdcr_results_v2.1_01, patid == "87420", select = c("cmrb_flag", "renal"))
subset(mdcr_results_v2.1_02, patid == "87420", select = c("cmrb_flag", "renal"))

subset(mdcr_results_v3.1_01, patid == "87420", select = c("cmrb_flag", "renal_dxpr_or_tech"))
subset(mdcr_results_v3.1_02, patid == "87420", select = c("cmrb_flag", "renal_dxpr_or_tech"))

subset(get_icd_codes(with.descriptions = TRUE), full_code %in% c("56.41", "564.1"))

## -----------------------------------------------------------------------------
merge(
  x = subset(mdcr, patid == "87420"),
  y = pccc_codes,
  by.x = c("code"),
  by.y = c("code"),
  suffixes = c(".mdcr", ".pccc_codes")
)

## -----------------------------------------------------------------------------
DF <-
  data.frame(
    id = c("full dx", "full pr", "compact dx", "compact pr"),
    code = c("564.1", "56.41", "5641", "5641"),
    dx = c(1, 0, 1, 0)
  )

# ideal: using the dx/pr status and matching on full and compact codes.
comorbidities(
  data = DF,
  id.vars = "id",
  dx.var = "dx",
  icd.codes = "code",
  poa = 1,
  method = "pccc_v3.1"
)[, c("id", "cmrb_flag", "renal_dxpr_or_tech")]

# false positive for the compact dx
comorbidities(
  data = DF,
  id.vars = "id",
  icd.codes = "code",
  poa = 1,
  method = "pccc_v3.1"
)[, c("id", "cmrb_flag", "renal_dxpr_or_tech")]

# false negative for compact pr
comorbidities(
  data = DF,
  id.vars = "id",
  icd.codes = "code",
  poa = 1,
  full.code = TRUE,
  compact.codes = FALSE,
  method = "pccc_v3.1"
)[, c("id", "cmrb_flag", "renal_dxpr_or_tech")]

# false positive for compact dx
comorbidities(
  data = DF,
  id.vars = "id",
  icd.codes = "code",
  poa = 1,
  full.code = FALSE,
  compact.codes = TRUE,
  method = "pccc_v3.1"
)[, c("id", "cmrb_flag", "renal_dxpr_or_tech")]

# false negatives for compact and full pr
comorbidities(
  data = DF,
  id.vars = "id",
  icd.codes = "code",
  dx.var = "dx",
  poa = 1,
  full.code = FALSE,
  compact.codes = TRUE,
  method = "pccc_v3.1"
)[, c("id", "cmrb_flag", "renal_dxpr_or_tech")]

## ----label = "patid95471"-----------------------------------------------------
subset(mdcr, patid == "95471")

# no flag becuse icdv = 9 which treats all input codes as ICD-9
comorbidities(
  data = subset(mdcr, patid == "95471"),
  icd.codes = "code",
  id.vars = 'patid',
  dx.var = "dx",
  icdv = 9L,
  poa = 1,
  method = "pccc_v3.1"
)[, c('patid', 'cmrb_flag')]

# flag because icdv = 10 - same as using `icdv.var = "icdv"`
comorbidities(
  data = subset(mdcr, patid == "95471"),
  icd.codes = "code",
  id.vars = 'patid',
  dx.var = "dx",
  icdv = 10L,
  poa = 1,
  method = "pccc_v3.1"
)[, c('patid', 'cmrb_flag')]

comorbidities(
  data = subset(mdcr, patid == "95471"),
  icd.codes = "code",
  id.vars = 'patid',
  dx.var = "dx",
  icdv.var = "icdv",
  poa = 1,
  method = "pccc_v3.0"
)[, c('patid', 'cmrb_flag')]

## -----------------------------------------------------------------------------
lookup_icd_codes("E030")
data <- data.frame(id = c("Ambiguous compact code", "Full ICD-9 code", "Full ICD-10 code"),
                   code  = c("E030", "E030", "E03.0"))
data

args <-
  list(
    data = data,
    id.vars = "id",
    icd.codes = "code",
    poa = 1,
    method = "pccc_v3.1"
  )

default <-
  do.call(comorbidities, c(args, list(full.codes = TRUE,  compact.codes = TRUE )))
full_only <-
  do.call(comorbidities, c(args, list(full.codes = TRUE,  compact.codes = FALSE)))
compact_only <-
  do.call(comorbidities, c(args, list(full.codes = FALSE, compact.codes = TRUE )))

default[,       c("id", "cmrb_flag")]
full_only[,     c("id", "cmrb_flag")]
compact_only[,  c("id", "cmrb_flag")]

## -----------------------------------------------------------------------------
head(mdcr_longitudinal)

## ----results = 'asis'---------------------------------------------------------
longitudinal_v2_patid <-
  comorbidities(
    data = mdcr_longitudinal,
    icd.codes = "code",
    id.vars = c("patid"),
    icdv.var = "icdv",
    method = "pccc_v2.1",
    flag.method = "current",
    poa = 1
  )
tab <- kableExtra::kbl(longitudinal_v2_patid)
tab <- kableExtra::kable_styling(tab, bootstrap_options = c("striped"), font_size = 10)
tab

## -----------------------------------------------------------------------------
longitudinal_v2_patid_date <-
  comorbidities(data = mdcr_longitudinal,
    icd.codes = "code",
    id.vars = c("patid", "date"),
    icdv.var = "icdv",
    method = "pccc_v2.1",
    flag.method = "current",
    poa = 1
  )

## ----echo = FALSE, results = 'asis'-------------------------------------------
tab <-
  kableExtra::kbl(
    subset(longitudinal_v2_patid_date, patid == "9663901"),
    row.names = FALSE
  )
tab <- kableExtra::kable_styling(tab, bootstrap_options = c("striped"), font_size = 10)
tab

## -----------------------------------------------------------------------------
longitudinal_v2_patid_date_cumulative_poa0 <-
  comorbidities(
    data = mdcr_longitudinal,
    icd.codes = "code",
    id.vars = c("patid", "date"),
    icdv.var = "icdv",
    method = "pccc_v2.1",
    flag.method = "cumulative",
    poa = 0
  )

## ----echo = FALSE, results = 'asis'-------------------------------------------
tab <-
  kableExtra::kbl(
    subset(longitudinal_v2_patid_date_cumulative_poa0, patid == "9663901"),
    row.names = FALSE
  )
tab <- kableExtra::kable_styling(tab, bootstrap_options = c("striped"), font_size = 10)
tab

## -----------------------------------------------------------------------------
longitudinal_v2_patid_date_cumulative_poa1 <-
  comorbidities(
    data = mdcr_longitudinal,
    icd.codes = "code",
    id.vars = c("patid", "date"),
    icdv.var = "icdv",
    method = "pccc_v2.1",
    flag.method = "cumulative",
    poa = 1
  )

## ----echo = FALSE, results = 'asis'-------------------------------------------
tab <-
  kableExtra::kbl(
    subset(longitudinal_v2_patid_date_cumulative_poa1, patid == "9663901"),
    row.names = FALSE
  )
tab <- kableExtra::kable_styling(tab, bootstrap_options = c("striped"), font_size = 10)
tab

## -----------------------------------------------------------------------------
codes <- c("H49.811", "J84.111", "Z96.41")
subset(get_pccc_codes(), full_code %in% codes)

## -----------------------------------------------------------------------------
permutations <-
  data.table::data.table(
    permutation = rep(1:6, each = 7),
    encounter_id = rep(1:7, times = 6),
    code =
      codes[c(NA, 1, NA, 2, NA, 3, NA,
              NA, 1, NA, 3, NA, 2, NA,
              NA, 2, NA, 1, NA, 3, NA,
              NA, 2, NA, 3, NA, 1, NA,
              NA, 3, NA, 1, NA, 2, NA,
              NA, 3, NA, 2, NA, 1, NA)]
  )

permutations[, plabel := paste(na.omit(code), collapse = ", "), by = .(permutation)]
permutations[, plabel := paste0("Permutation ", permutation, ": ", plabel)]
str(permutations, vec.len = 1)

## ----echo = FALSE, results = "asis"-------------------------------------------
cat(paste("*", permutations[, unique(plabel)]), sep = "\n")

## -----------------------------------------------------------------------------
rtn <-
  comorbidities(
    data = permutations,
    icd.codes = "code",
    id.vars = c("permutation", "plabel", "encounter_id"),
    icdv = 10L,
    compact.codes = FALSE,
    method = "pccc_v3.1",
    flag.method = "cumulative",
    poa = 1
  )

## ----label = "setup-rtn-for-discussion", include = FALSE----------------------
rtn_wide <-
  data.table::dcast(
    encounter_id ~ plabel,
    data = rtn,
    value.var = c("metabolic_dxpr_or_tech", "metabolic_dxpr_only",
                  "metabolic_tech_only", "metabolic_dxpr_and_tech",
                  "respiratory_dxpr_or_tech", "respiratory_dxpr_only",
                  "respiratory_tech_only", "respiratory_dxpr_and_tech",
                  "cmrb_flag", "num_cmrb")
  )

## ----label = "define-pkbl", include = FALSE-----------------------------------
pkbl <- function(permutation = 1) {
  stopifnot(length(permutation) == 1L)

  x <- rtn_wide[
        ,
        .SD,
        .SDcols = c("encounter_id",
                    grep(paste0("Permutation ", permutation), names(rtn_wide), value = TRUE))
       ]

  pl <- rtn[["plabel"]][rtn$permutation == permutation][1]

  tab <-
    kableExtra::kbl(
      x,
      col.names = c("encounter_id", rep(c("dxpr or tech", "dxpr only", "tech only", "dxpr and tech"), times = 2), "ccc flag", "num ccc")
    )
  tab <- kableExtra::kable_styling(kable_input = tab, bootstrap_options = c("striped"), font_size = 10)
  tab <- kableExtra::add_header_above(kable_input = tab, header = c("", c("Metabolic" = 4, "Respiratory" = 4), "", ""))
  tab <- kableExtra::add_header_above(kable_input = tab, header = c("", setNames(10, pl)))
  tab
}

## ----echo = FALSE, results = "asis"-------------------------------------------
pkbl(1)

## ----echo = FALSE, results = "asis"-------------------------------------------
pkbl(2)

## ----echo = FALSE, results = "asis"-------------------------------------------
pkbl(3)

## ----echo = FALSE, results = "asis"-------------------------------------------
pkbl(4)

## ----echo = FALSE, results = "asis"-------------------------------------------
pkbl(5)

## ----echo = FALSE, results = "asis"-------------------------------------------
pkbl(6)

## ----"tbl-syntactically-valid-subconditions", echo = FALSE, results = "asis"----
SCNDS <- get_pccc_conditions()
data.table::setDT(SCNDS)
data.table::setkey(SCNDS, condition, subcondition)
SCNDS[, condition := paste(condition, condition_label, sep = ": ")]

tab <-
  kableExtra::kbl(SCNDS[, .(subcondition, subcondition_label)],
      capttion = "Syntactically valid names for subconditions",
      row.names = FALSE
  )
tab <-
  kableExtra::pack_rows(tab, index = table(SCNDS$condition))
tab <-
  kableExtra::kable_styling(tab, bootstrap_options = c('striped'), fixed_thead = TRUE, font_size = 10)
tab <-
  kableExtra::scroll_box(tab, height = "300px")
tab

## -----------------------------------------------------------------------------
without_subconditions <-
  comorbidities(
    data = mdcr,
    id.vars = "patid",
    icd.codes = "code",
    icdv.var = "icdv",
    dx.var = "dx",
    poa = 1,
    method = "pccc_v3.1",
    subconditions = FALSE
  )

with_subconditions <-
  comorbidities(
    data = mdcr,
    id.vars = "patid",
    icd.codes = "code",
    icdv.var = "icdv",
    dx.var = "dx",
    poa = 1,
    method = "pccc_v3.1",
    subconditions = TRUE
  )

## -----------------------------------------------------------------------------
with_subconditions

all.equal(
  with_subconditions$conditions,
  without_subconditions,
  check.attributes = FALSE
)

## -----------------------------------------------------------------------------
str(summary(with_subconditions))

## ----include = FALSE----------------------------------------------------------
args <-
  list(
    data = mdcr,
    id.vars = "patid",
    icd.codes = "code",
    icdv.var = "icdv",
    dx.var = "dx",
    poa = 1,
    subconditions = TRUE
  )
with_subconditions_v2.0 <- do.call(comorbidities, c(args, list(method = "pccc_v2.0")))
with_subconditions_v2.1 <- do.call(comorbidities, c(args, list(method = "pccc_v2.1")))
with_subconditions_v3.0 <- do.call(comorbidities, c(args, list(method = "pccc_v3.0")))
with_subconditions_v3.1 <- do.call(comorbidities, c(args, list(method = "pccc_v3.1")))

## ----include = FALSE----------------------------------------------------------
rslts <-
  merge(
    merge(
      summary(with_subconditions_v2.0),
      summary(with_subconditions_v2.1),
      by = c("condition", "subcondition"),
      suffixes = c("_v2.0", "_v2.1"),
      sort = FALSE
    ),
    merge(
      summary(with_subconditions_v3.0),
      summary(with_subconditions_v3.1),
      by = c("condition", "subcondition"),
      suffixes = c("_v3.0", "_v3.1"),
      sort = FALSE
    ),
    by = c("condition", "subcondition"),
    sort = FALSE
  )

rslts$idx <- 1:nrow(rslts)

rslts <-
  merge(rslts,
        unique(get_pccc_conditions()[c("condition", "condition_label")]),
        all = TRUE,
        by = "condition",
        sort = FALSE)
rslts <-
  merge(rslts,
        unique(get_pccc_conditions()[c("subcondition", "subcondition_label")]),
        all = TRUE,
        by = "subcondition",
        sort = FALSE)
rslts$lab <- rslts$subcondition_label
rslts$lab[is.na(rslts$subcondition)] <- rslts$condition_label[is.na(rslts$subcondition)]
rslts <- rslts[order(rslts$idx), ]

rslts <- split(rslts, f = rslts[["condition"]])

cols_to_keep <-
  c(
    "lab",
    "count_v2.0",
    "percent_of_cohort_v2.0",
    "percent_of_those_with_condition_v2.0",
    "count_v2.1",
    "percent_of_cohort_v2.1",
    "percent_of_those_with_condition_v2.1",
    "count_v3.0",
    "percent_of_cohort_v3.0",
    "percent_of_those_with_condition_v3.0",
    "count_v3.1",
    "percent_of_cohort_v3.1",
    "percent_of_those_with_condition_v3.1"
  )

tabs <- lapply(rslts, function(x) {x[, cols_to_keep]})

tabs <-
  lapply(tabs,
    kableExtra::kbl,
    col.names = c("", rep(c("count", "% of cohort", "% of those with condition"), 4)),
    row.names = FALSE,
    digits = 1
  )
tabs <-
  Map(
    f = function(t,r) {
          x <- kableExtra::column_spec(t, column = 1, bold = is.na(r$subcondition))
          kableExtra::add_indent(x, which(!is.na(r$subcondition)))
        },
    t = tabs,
    r = rslts
  )
tabs <-
  lapply(tabs, kableExtra::kable_styling, bootstrap_options = "striped", font_size = 10)
tabs <-
  lapply(tabs, kableExtra::add_header_above, header = c("", "v2.0" = 3, "v2.1" = 3, "v3.0" = 3, "v3.1" = 3))
tabs

## ----echo = FALSE, results = "asis"-------------------------------------------
tabs[["congeni_genetic"]]

## ----echo = FALSE, results = "asis"-------------------------------------------
tabs[["cvd"]]

## ----echo = FALSE, results = "asis"-------------------------------------------
tabs[["gi"]]

## ----echo = FALSE, results = "asis"-------------------------------------------
tabs[["hemato_immu"]]

## ----echo = FALSE, results = "asis"-------------------------------------------
tabs[["malignancy"]]

## ----echo = FALSE, results = "asis"-------------------------------------------
tabs[["metabolic"]]

## ----echo = FALSE, results = "asis"-------------------------------------------
tabs[["misc"]]

## ----echo = FALSE, results = "asis"-------------------------------------------
tabs[["neonatal"]]

## ----echo = FALSE, results = "asis"-------------------------------------------
tabs[["neuromusc"]]

## ----echo = FALSE, results = "asis"-------------------------------------------
tabs[["renal"]]

## ----echo = FALSE, results = "asis"-------------------------------------------
tabs[["respiratory"]]

## -----------------------------------------------------------------------------
rslts <-
  comorbidities(
    data = permutations,
    icd.codes = "code",
    id.vars = c("permutation", "plabel", "encounter_id"),
    icdv = 10L,
    compact.codes = FALSE,
    method = "pccc_v3.1",
    flag.method = "cumulative",
    poa = 1,
    subconditions = TRUE
  )

## -----------------------------------------------------------------------------
all(rslts$subconditions$respiratory$chronic_respiratory_diseases == 1)
sapply(rslts$subconditions$respiratory[, -(1:3)], max)

# which encounters flag for primary condition respiratory?
cnd <-
  rslts$conditions[
    respiratory_dxpr_or_tech == 1,
    .(cencid = paste(encounter_id, collapse = ", ")),
    by = .(plabel)
  ]


# which encounters flag for the subcondition chronic_respiratory_diseases?
scnd <-
  rslts$subconditions$respiratory[
    ,
    .(sencid = paste(encounter_id, collapse = ", ")),
    by = .(plabel)
  ]

## ----echo = FALSE, results = "asis"-------------------------------------------
tab <-
  kableExtra::kbl(
    merge(cnd, scnd, all = TRUE, by = "plabel"),
    caption = "Encounters flagging for respiratory condition and the chronic respiratory disease subcondition.",
    col.names = c("", "Condition", "Subcondition")
  )
tab <- kableExtra::kable_styling(tab, bootstrap_options = c("striped"), font_size = 10)
tab <- kableExtra::add_header_above(tab, c("", "Encounters" = 2))
tab

## -----------------------------------------------------------------------------
# which encounters flag for primary condition metabolic?
cnd <-
  rslts$conditions[
    metabolic_dxpr_or_tech == 1,
    .(cencid = paste(encounter_id, collapse = ", ")),
    by = .(plabel)
  ]

# which encounters flag for the subconditions?
scnd <-
  data.table::melt(
    rslts$subconditions$metabolic,
    id.vars = c("plabel", "encounter_id"),
    measure.vars = c("device_and_technology_use", "other_metabolic_disorders"),
    variable.factor = FALSE,
    variable.name = "subcondition"
  )
scnd <- scnd[value == 1]
scnd <-
  scnd[
    ,
    .(sencid = paste(encounter_id, collapse = ", ")),
    by = .(plabel, subcondition)
  ]

scnd <-
  data.table::dcast(
    scnd,
    plabel ~ subcondition,
    value.var = "sencid"
  )

## ----echo = FALSE, results = "asis"-------------------------------------------
tab <-
  kableExtra::kbl(
    x = merge(cnd, scnd, all = TRUE, by = "plabel"),
    caption = "Encounters flagging for a metabolic condition and the encounters flagging for subconidtions device and technology use and/or other metabolic disorders.",
    col.names = c("", "Condition", "Device and Technology Use", "Other Metabolic Disorders")
  )
tab <- kableExtra::kable_styling(tab, bootstrap_options = c("striped"), font_size = 10)
tab <-
  kableExtra::add_header_above(tab, c("", "Encounters" = 3))
tab

