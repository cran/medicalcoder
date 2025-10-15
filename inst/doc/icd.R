## ----label = "setup", include = FALSE-----------------------------------------
knitr::opts_chunk$set(collapse = TRUE, fig.align = "center")

## ----label = "medicalcoder-url"-----------------------------------------------
cat(packageDescription('medicalcoder')$URL)

## ----label = "get-icd-codes"--------------------------------------------------
library(medicalcoder)
icd_codes <- get_icd_codes()
str(icd_codes)

## ----label = "example-for-assignable", include = FALSE------------------------
d4 <- lookup_icd_codes(x = "^C84\\.6", regex = TRUE, compact.codes = FALSE)
d4 <- subset(d4, src %in% c("cms", "who"), select = c("src", "full_code"))
d4 <- unique(d4)
d4

## ----label = "icd-codes-with-descriptions"------------------------------------
str(get_icd_codes(with.descriptions = TRUE))

## ----label = "deltas-in-desc", results = "hide"-------------------------------
delta_in_desc <-
  subset(get_icd_codes(with.descriptions = TRUE),
         subset = full_code %in% c("Z88.7", "010.93", "V76.49"),
         select = c("full_code", "src", "desc", "desc_start", "desc_end"))

## ----label = "deltas-in-desc-show", echo = FALSE, results = "asis"------------
knitr::kable(delta_in_desc, row.names = FALSE)

## ----label = "get-icd-descs-with-heirarchy"-----------------------------------
str(get_icd_codes(with.hierarchy = TRUE))

## ----label = "lookup-icd-code-example"----------------------------------------
codes <- c("0011", "7329", "732", "73291", "not a code", "001.1", "A9248", "A924", "Z00")
knitr::kable(lookup_icd_codes(codes), row.names = FALSE)

## ----label = "lookup-compact-icd-codes"---------------------------------------
knitr::kable(
  lookup_icd_codes(codes, full.codes = FALSE),
  row.names = FALSE
)

## ----label = "lookup-full-icd-codes"------------------------------------------
knitr::kable(
  lookup_icd_codes(codes, compact.codes = FALSE),
  row.names = FALSE
)

## ----label = "lookup-icd-code-by-regex"---------------------------------------
knitr::kable(
  lookup_icd_codes(x = "^C84\\.6[0-1A-Z]", regex = TRUE),
  row.names = FALSE
)

## ----label = "icd-7993"-------------------------------------------------------
is_icd(x = "7993")
is_icd(x = "7993", icdv =  9, dx = 1)
is_icd(x = "7993", icdv =  9, dx = 0)
is_icd(x = "7993", icdv = 10, dx = 1)
is_icd(x = "7993", icdv = 10, dx = 0)
lookup_icd_codes("7993")

## -----------------------------------------------------------------------------
x <- c("7993", "A924", "7993", "A924", "no", "A92", "516", "5163", "51631", "A00")
is_icd(x)

## -----------------------------------------------------------------------------
x <- c("7993",  # valid dx and pr code
       ".7993", # not a valid code
       "7.993", # not a valid code
       "79.93", # invalid dx code; valid pr code
       "799.3", # valid dx code; invalid pr code
       "7993.") # not a valid code
data.frame(x = x,
           icd9_dx = is_icd(x, icdv = 9, dx = 1, warn.ambiguous = FALSE),
           icd9_pr = is_icd(x, icdv = 9, dx = 0, warn.ambiguous = FALSE))

## ----results = "asis"---------------------------------------------------------
x <- paste0("516.3", c("", 0:9))
tab <-
  data.frame(
    code       = x,
    default    = is_icd(x, icdv = 9, dx = 1),
    assignable_1997 = is_icd(x, src = "cms", icdv = 9, dx = 1, year = 1997),
    assignable_2010 = is_icd(x, src = "cms", icdv = 9, dx = 1, year = 2010),
    assignable_2011 = is_icd(x, src = "cms", icdv = 9, dx = 1, year = 2011),
    assignable_2012 = is_icd(x, src = "cms", icdv = 9, dx = 1, year = 2012),
    assignable_2013 = is_icd(x, src = "cms", icdv = 9, dx = 1, year = 2013),
    assignable_2016 = is_icd(x, src = "cms", icdv = 9, dx = 1, year = 2016),
    assignable_ever = is_icd(x, src = "cms", icdv = 9, dx = 1, ever.assignable = TRUE)
  )
knitr::kable(tab)

## ----results = "asis"---------------------------------------------------------
knitr::kable(lookup_icd_codes(x))

## -----------------------------------------------------------------------------
x <- c("516", "5163", "51631", "A00")
tab <-
  data.frame(
    code     = x,
    default  = is_icd(x, icdv = 9, dx = 1, src = "cms", headerok = FALSE, ever.assignable = FALSE, warn.ambiguous = FALSE),
    ever     = is_icd(x, icdv = 9, dx = 1, src = "cms", headerok = FALSE, ever.assignable = TRUE,  warn.ambiguous = FALSE),
    headerok = is_icd(x, icdv = 9, dx = 1, src = "cms", headerok = TRUE,                           warn.ambiguous = FALSE)
  )
knitr::kable(tab)

## ----label="7197"-------------------------------------------------------------
x <- paste0("719.7", c("", "0", 5:9))
tab <-
  data.frame(
    code            = x,
    default         = is_icd(x, src = "cms", icdv = 9, dx = 1),
    assignable_2002 = is_icd(x, src = "cms", icdv = 9, dx = 1, year = 2002),
    assignable_2003 = is_icd(x, src = "cms", icdv = 9, dx = 1, year = 2003),
    assignable_2004 = is_icd(x, src = "cms", icdv = 9, dx = 1, year = 2004),
    assignable_2005 = is_icd(x, src = "cms", icdv = 9, dx = 1, year = 2005),
    assignable_ever = is_icd(x, src = "cms", icdv = 9, dx = 1, ever.assignable = TRUE)
  )
knitr::kable(tab)

## -----------------------------------------------------------------------------
icd_compact_to_full("E1234", icdv =  9, dx = 1)
icd_compact_to_full("E1234", icdv = 10, dx = 1)

lookup_icd_codes(c("E1234", "E123.4", "E12.34"))[, c("input_code", "match_type")]

## -----------------------------------------------------------------------------
icd_compact_to_full("E1234", icdv =  9, dx = 0)
icd_compact_to_full("E1234", icdv = 10, dx = 0)

