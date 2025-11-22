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
str(get_charlson_codes())
str(get_charlson_index_scores())

## -----------------------------------------------------------------------------
mdcr_results <-
  comorbidities(
    data = mdcr,
    id.vars = "patid",
    icdv.var = "icdv",
    icd.codes = "code",
    dx.var = "dx",
    flag.method = "current",
    poa = 1,
    primarydx = 0,
    method = "charlson_quan2005"
  )

## -----------------------------------------------------------------------------
str(mdcr_results)

## -----------------------------------------------------------------------------
str(summary(mdcr_results))

## ----echo = FALSE, results = "asis"-------------------------------------------
x <- summary(mdcr_results)$conditions[, c("condition_description", "count", "percent")]
tab <-
  kableExtra::kbl(
    x = x,
    format = "html",
    caption = "Counts and percentages of patients in the mdcr example data sets with the @quan2005 comorbidities.",
    col.names = c("", "Count", "Percentage"),
    digits = 3
  )
tab <- kableExtra::kable_styling(tab, bootstrap_options = c("striped"), font_size = 10)
tab <- kableExtra::pack_rows(tab, group_label = "Comorbidity", start_row = 1, end_row = 17)
tab <- kableExtra::pack_rows(tab, group_label = "Total Comorbidities", start_row = 18, end_row = nrow(x))
tab

