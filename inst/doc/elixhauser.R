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
packageVersion("medicalcoder")

## -----------------------------------------------------------------------------
str(get_elixhauser_codes())
str(get_elixhauser_index_scores())
str(get_elixhauser_poa())

## -----------------------------------------------------------------------------
# will warn because primarydx and primarydx.var are both NULL
mdcr_results0 <-
  comorbidities(
    data = mdcr,
    id.vars = "patid",
    icdv.var = "icdv",
    icd.codes = "code",
    dx.var = "dx",
    flag.method = "current",
    poa = 1,
    method = "elixhauser_ahrq2025"
  )

# no warning
mdcr_results <-
  comorbidities(
    data = mdcr,
    id.vars = "patid",
    icdv.var = "icdv",
    icd.codes = "code",
    dx.var = "dx",
    flag.method = "current",
    poa = 1,
    method = "elixhauser_ahrq2025",
    primarydx = 0
  )

identical(mdcr_results, mdcr_results0)

## -----------------------------------------------------------------------------
str(mdcr_results)

## -----------------------------------------------------------------------------
summary(mdcr_results)

## ----results = "asis"---------------------------------------------------------
x <- summary(mdcr_results)$conditions
tab <-
  kableExtra::kbl(
    x = x,
    format = "html",
    caption = "Counts and percentages of patients in the mdcr example data sets with the Elixhauser @quan2005 comorbidities.",
    col.names = c("", "Count", "Percentage"),
    digits = 3
  )
tab <- kableExtra::pack_rows(tab, group_label = "Comorbidity", start_row = 1, end_row = 39)
tab <- kableExtra::pack_rows(tab, group_label = "Total Comorbidities", start_row = 39, end_row = nrow(x))
tab

