# Create Time-to-Event Summary Table

Generates a standard TTE summary table with median survival, confidence
intervals, hazard ratios, and optional landmark estimates for efficacy
endpoints like OS, PFS, EFS.

## Usage

``` r
create_tte_summary_table(
  data,
  time_var = "AVAL",
  event_var = "CNSR",
  trt_var = "TRT01P",
  ref_group = NULL,
  conf_level = 0.95,
  check_ph = TRUE,
  landmarks = NULL,
  time_unit = "months",
  title = "Time-to-Event Summary",
  footnotes = character(),
  autofit = TRUE,
  ...
)
```

## Arguments

- data:

  An ADaMData object (with domain "ADTTE") or an ADTTE data frame.
  Required columns include: time variable (default: "AVAL"), event
  variable (default: "CNSR"), and the treatment variable (default:
  "TRT01P" for data frames, or @trt_var for ADaMData).

- time_var:

  Time variable name (default: "AVAL")

- event_var:

  Event indicator variable. If "CNSR" (ADaM censoring flag), it will be
  automatically inverted (0=event becomes 1=event). Otherwise expects
  1=event, 0=censor. Default: "CNSR"

- trt_var:

  Treatment variable name (default: "TRT01P"). Ignored for ADaMData
  objects which use their own trt_var property.

- ref_group:

  Reference group for HR calculation. If NULL, uses first level of
  treatment variable.

- conf_level:

  Confidence level for intervals (default: 0.95)

- check_ph:

  Logical. Whether to test proportional hazards assumption and warn on
  violations (default: TRUE)

- landmarks:

  Numeric vector of timepoints for landmark survival estimates (e.g.,
  c(12, 24) for 12 and 24 month rates). NULL for none.

- time_unit:

  Character string for time unit display (default: "months")

- title:

  Table title

- footnotes:

  Character vector of footnotes to append to the table.

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

- ...:

  Additional arguments passed to
  [`create_clinical_table`](https://sims1253.github.io/pharmhand/branch/dev/reference/create_clinical_table.md)

## Value

A ClinicalTable object with TTE summary statistics

## Examples

``` r
# Create time-to-event summary with data frame
adtte <- data.frame(
  USUBJID = sprintf("SUBJ%02d", 1:40),
  TRT01P = rep(c("Placebo", "Active"), each = 20),
  AVAL = c(rexp(20, 0.05), rexp(20, 0.03)),
  CNSR = sample(0:1, 40, replace = TRUE, prob = c(0.7, 0.3))
)
table <- create_tte_summary_table(adtte)
#> Automatically wrapping data.frame in ADaMData object
print(table)
#> <pharmhand::ClinicalTable>
#>  @ type        : chr "tte_summary"
#>  @ title       : chr "Time-to-Event Summary"
#>  @ metadata    :List of 7
#>  .. $ theme    : chr "hta"
#>  .. $ km_fit   :List of 18
#>  ..  ..$ n        : int [1:2] 20 20
#>  ..  ..$ time     : num [1:40] 11 11.2 12.5 13 21.3 ...
#>  ..  ..$ n.risk   : num [1:40] 20 19 18 17 16 15 14 13 12 11 ...
#>  ..  ..$ n.event  : num [1:40] 1 1 1 0 0 1 1 1 1 1 ...
#>  ..  ..$ n.censor : num [1:40] 0 0 0 1 1 0 0 0 0 0 ...
#>  ..  ..$ surv     : num [1:40] 0.95 0.9 0.85 0.85 0.85 ...
#>  ..  ..$ std.err  : num [1:40] 0.0513 0.0745 0.0939 0.0939 0.0939 ...
#>  ..  ..$ cumhaz   : num [1:40] 0.05 0.103 0.158 0.158 0.158 ...
#>  ..  ..$ std.chaz : num [1:40] 0.05 0.0726 0.0914 0.0914 0.0914 ...
#>  ..  ..$ strata   : Named int [1:2] 20 20
#>  ..  .. ..- attr(*, "names")= chr [1:2] "TRT01P=Active" "TRT01P=Placebo"
#>  ..  ..$ type     : chr "right"
#>  ..  ..$ logse    : logi TRUE
#>  ..  ..$ conf.int : num 0.95
#>  ..  ..$ conf.type: chr "log"
#>  ..  ..$ lower    : num [1:40] 0.859 0.778 0.707 0.707 0.707 ...
#>  ..  ..$ upper    : num [1:40] 1 1 1 1 1 ...
#>  ..  ..$ t0       : num 0
#>  ..  ..$ call     : language survfit(formula = surv_formula, data = df, conf.int = conf_level)
#>  ..  ..- attr(*, "class")= chr "survfit"
#>  .. $ cox_fit  :List of 21
#>  ..  ..$ coefficients     : Named num 1.2
#>  ..  .. ..- attr(*, "names")= chr "TRT01PPlacebo"
#>  ..  ..$ var              : num [1, 1] 0.209
#>  ..  ..$ loglik           : num [1:2] -76.4 -72.8
#>  ..  ..$ score            : num 7.69
#>  ..  ..$ iter             : int 4
#>  ..  ..$ linear.predictors: num [1:40] 1.2 1.2 1.2 1.2 1.2 ...
#>  ..  ..$ residuals        : Named num [1:40] -0.566 -1.378 0.502 0.962 -0.995 ...
#>  ..  .. ..- attr(*, "names")= chr [1:40] "1" "2" "3" "4" ...
#>  ..  ..$ means            : Named num 0
#>  ..  .. ..- attr(*, "names")= chr "TRT01PPlacebo"
#>  ..  ..$ method           : chr "efron"
#>  ..  ..$ n                : int 40
#>  ..  ..$ nevent           : num 27
#>  ..  ..$ terms            :Classes 'terms', 'formula'  language survival::Surv(AVAL, event) ~ TRT01P
#>  ..  .. .. ..- attr(*, "variables")= language list(survival::Surv(AVAL, event), TRT01P)
#>  ..  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
#>  ..  .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. ..$ : chr [1:2] "survival::Surv(AVAL, event)" "TRT01P"
#>  ..  .. .. .. .. ..$ : chr "TRT01P"
#>  ..  .. .. ..- attr(*, "term.labels")= chr "TRT01P"
#>  ..  .. .. ..- attr(*, "specials")=Dotted pair list of 5
#>  ..  .. .. .. ..$ strata : NULL
#>  ..  .. .. .. ..$ tt     : NULL
#>  ..  .. .. .. ..$ frailty: NULL
#>  ..  .. .. .. ..$ ridge  : NULL
#>  ..  .. .. .. ..$ pspline: NULL
#>  ..  .. .. ..- attr(*, "order")= int 1
#>  ..  .. .. ..- attr(*, "intercept")= num 1
#>  ..  .. .. ..- attr(*, "response")= int 1
#>  ..  .. .. ..- attr(*, ".Environment")=<environment: 0x555a718d8a78> 
#>  ..  .. .. ..- attr(*, "predvars")= language list(survival::Surv(AVAL, event), TRT01P)
#>  ..  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "nmatrix.2" "factor"
#>  ..  .. .. .. ..- attr(*, "names")= chr [1:2] "survival::Surv(AVAL, event)" "TRT01P"
#>  ..  ..$ assign           :List of 1
#>  ..  .. ..$ TRT01P: int 1
#>  ..  ..$ wald.test        : Named num 6.93
#>  ..  .. ..- attr(*, "names")= chr "TRT01PPlacebo"
#>  ..  ..$ concordance      : Named num [1:7] 214 71 267 0 0 ...
#>  ..  .. ..- attr(*, "names")= chr [1:7] "concordant" "discordant" "tied.x" "tied.y" ...
#>  ..  ..$ y                : 'Surv' num [1:40, 1:2]  16.084+  28.719+  14.372    0.551   34.411    2.774   30.790   31.039   26.846    7.320  ...
#>  ..  .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. ..$ : chr [1:40] "1" "2" "3" "4" ...
#>  ..  .. .. ..$ : chr [1:2] "time" "status"
#>  ..  .. ..- attr(*, "type")= chr "right"
#>  ..  ..$ timefix          : logi TRUE
#>  ..  ..$ formula          :Class 'formula'  language survival::Surv(AVAL, event) ~ TRT01P
#>  ..  .. .. ..- attr(*, ".Environment")=<environment: 0x555a718d8a78> 
#>  ..  ..$ xlevels          :List of 1
#>  ..  .. ..$ TRT01P: chr [1:2] "Active" "Placebo"
#>  ..  ..$ contrasts        :List of 1
#>  ..  .. ..$ TRT01P: chr "contr.treatment"
#>  ..  ..$ call             : language survival::coxph(formula = surv_formula, data = df)
#>  ..  ..- attr(*, "class")= chr "coxph"
#>  .. $ ph_test  :List of 6
#>  ..  ..$ results    :'data.frame':   1 obs. of  5 variables:
#>  ..  .. ..$ variable : chr "TRT01P"
#>  ..  .. ..$ rho      : num NA
#>  ..  .. ..$ chisq    : num 0.733
#>  ..  .. ..$ p_value  : num 0.392
#>  ..  .. ..$ violation: logi FALSE
#>  ..  ..$ global_test: num 0.392
#>  ..  ..$ violation  : logi FALSE
#>  ..  ..$ model      :List of 21
#>  ..  .. ..$ coefficients     : Named num 1.2
#>  ..  .. .. ..- attr(*, "names")= chr "TRT01PPlacebo"
#>  ..  .. ..$ var              : num [1, 1] 0.209
#>  ..  .. ..$ loglik           : num [1:2] -76.4 -72.8
#>  ..  .. ..$ score            : num 7.69
#>  ..  .. ..$ iter             : int 4
#>  ..  .. ..$ linear.predictors: num [1:40] 1.2 1.2 1.2 1.2 1.2 ...
#>  ..  .. ..$ residuals        : Named num [1:40] -0.566 -1.378 0.502 0.962 -0.995 ...
#>  ..  .. .. ..- attr(*, "names")= chr [1:40] "1" "2" "3" "4" ...
#>  ..  .. ..$ means            : Named num 0
#>  ..  .. .. ..- attr(*, "names")= chr "TRT01PPlacebo"
#>  ..  .. ..$ method           : chr "efron"
#>  ..  .. ..$ n                : int 40
#>  ..  .. ..$ nevent           : num 27
#>  ..  .. ..$ terms            :Classes 'terms', 'formula'  language survival::Surv(AVAL, event) ~ TRT01P
#>  ..  .. .. .. ..- attr(*, "variables")= language list(survival::Surv(AVAL, event), TRT01P)
#>  ..  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : chr [1:2] "survival::Surv(AVAL, event)" "TRT01P"
#>  ..  .. .. .. .. .. ..$ : chr "TRT01P"
#>  ..  .. .. .. ..- attr(*, "term.labels")= chr "TRT01P"
#>  ..  .. .. .. ..- attr(*, "specials")=Dotted pair list of 5
#>  ..  .. .. .. .. ..$ strata : NULL
#>  ..  .. .. .. .. ..$ tt     : NULL
#>  ..  .. .. .. .. ..$ frailty: NULL
#>  ..  .. .. .. .. ..$ ridge  : NULL
#>  ..  .. .. .. .. ..$ pspline: NULL
#>  ..  .. .. .. ..- attr(*, "order")= int 1
#>  ..  .. .. .. ..- attr(*, "intercept")= num 1
#>  ..  .. .. .. ..- attr(*, "response")= int 1
#>  ..  .. .. .. ..- attr(*, ".Environment")=<environment: 0x555a718d8a78> 
#>  ..  .. .. .. ..- attr(*, "predvars")= language list(survival::Surv(AVAL, event), TRT01P)
#>  ..  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "nmatrix.2" "factor"
#>  ..  .. .. .. .. ..- attr(*, "names")= chr [1:2] "survival::Surv(AVAL, event)" "TRT01P"
#>  ..  .. ..$ assign           :List of 1
#>  ..  .. .. ..$ TRT01P: int 1
#>  ..  .. ..$ wald.test        : Named num 6.93
#>  ..  .. .. ..- attr(*, "names")= chr "TRT01PPlacebo"
#>  ..  .. ..$ concordance      : Named num [1:7] 214 71 267 0 0 ...
#>  ..  .. .. ..- attr(*, "names")= chr [1:7] "concordant" "discordant" "tied.x" "tied.y" ...
#>  ..  .. ..$ y                : 'Surv' num [1:40, 1:2]  16.084+  28.719+  14.372    0.551   34.411    2.774   30.790   31.039   26.846    7.320  ...
#>  ..  .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. ..$ : chr [1:40] "1" "2" "3" "4" ...
#>  ..  .. .. .. ..$ : chr [1:2] "time" "status"
#>  ..  .. .. ..- attr(*, "type")= chr "right"
#>  ..  .. ..$ timefix          : logi TRUE
#>  ..  .. ..$ formula          :Class 'formula'  language survival::Surv(AVAL, event) ~ TRT01P
#>  ..  .. .. .. ..- attr(*, ".Environment")=<environment: 0x555a718d8a78> 
#>  ..  .. ..$ xlevels          :List of 1
#>  ..  .. .. ..$ TRT01P: chr [1:2] "Active" "Placebo"
#>  ..  .. ..$ contrasts        :List of 1
#>  ..  .. .. ..$ TRT01P: chr "contr.treatment"
#>  ..  .. ..$ call             : language survival::coxph(formula = surv_formula, data = df)
#>  ..  .. ..- attr(*, "class")= chr "coxph"
#>  ..  ..$ zph        :List of 7
#>  ..  .. ..$ table    : num [1:2, 1:3] 0.733 0.733 1 1 0.392 ...
#>  ..  .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. ..$ : chr [1:2] "TRT01P" "GLOBAL"
#>  ..  .. .. .. ..$ : chr [1:3] "chisq" "df" "p"
#>  ..  .. ..$ x        : num [1:27] 0 0.025 0.05 0.075 0.101 ...
#>  ..  .. ..$ time     : num [1:27] 0.551 2.774 5.415 7.32 11.04 ...
#>  ..  .. ..$ y        : num [1:27, 1] 2.51 2.56 2.62 2.75 -2.75 ...
#>  ..  .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. ..$ : chr [1:27] "0.5512" "2.774" "5.415" "7.32" ...
#>  ..  .. .. .. ..$ : chr "TRT01P"
#>  ..  .. ..$ var      : num [1, 1] 5.65
#>  ..  .. ..$ transform: chr "km"
#>  ..  .. ..$ call     : language survival::cox.zph(fit = model)
#>  ..  .. ..- attr(*, "class")= chr "cox.zph"
#>  ..  ..$ plot       : NULL
#>  .. $ ref_group: chr "Active"
#>  .. $ landmarks: NULL
#>  .. $ time_unit: chr "months"
#>  @ data        : tibble [5 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ Statistic: chr [1:5] "N" "Events n (%)" "Median (95% CI)" "HR (95% CI)" ...
#>  $ Active   : chr [1:5] "20" "13 (65.0)" "57.5 (25.9, NE)" "Reference" ...
#>  $ Placebo  : chr [1:5] "20" "14 (70.0)" "20.4 (13.9, NE)" "3.33 (1.36, 8.17)" ...
#>  @ flextable   :List of 7
#>  .. $ header    :List of 8
#>  ..  ..$ dataset   :'data.frame':    2 obs. of  3 variables:
#>  ..  .. ..$ Statistic: chr [1:2] "Time-to-Event Summary" "Statistic"
#>  ..  .. ..$ Active   : chr [1:2] "Time-to-Event Summary" "Active"
#>  ..  .. ..$ Placebo  : chr [1:2] "Time-to-Event Summary" "Placebo"
#>  ..  ..$ content   :List of 5
#>  ..  .. ..$ data   :List of 6
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "Time-to-Event Summary"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "Statistic"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "Time-to-Event Summary"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "Active"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "Time-to-Event Summary"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "Placebo"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..- attr(*, "dim")= int [1:2] 2 3
#>  ..  .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. ..$ nrow   : int 2
#>  ..  .. ..$ ncol   : int 3
#>  ..  .. ..$ default:List of 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr ""
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..- attr(*, "class")= chr "paragraph"
#>  ..  .. ..- attr(*, "class")= chr "chunkset_struct"
#>  ..  ..$ col_keys  : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  ..$ colwidths : Named num [1:3] 1.11 1.02 1.09
#>  ..  .. ..- attr(*, "names")= chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  ..$ rowheights: num [1:2] 0.276 0.276
#>  ..  ..$ hrule     : chr [1:2] "auto" "auto"
#>  ..  ..$ spans     :List of 2
#>  ..  .. ..$ rows   : num [1:2, 1:3] 3 1 0 1 0 1
#>  ..  .. ..$ columns: num [1:2, 1:3] 1 1 1 1 1 1
#>  ..  ..$ styles    :List of 3
#>  ..  .. ..$ cells:List of 22
#>  ..  .. .. ..$ vertical.align     :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "center" "center" "center" "center" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "center"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ width              :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:3] NA NA NA NA NA NA
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ height             :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:3] NA NA NA NA NA NA
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.bottom      :List of 5
#>  ..  .. .. .. ..$ data   : int [1:2, 1:3] 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.top         :List of 5
#>  ..  .. .. .. ..$ data   : int [1:2, 1:3] 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.left        :List of 5
#>  ..  .. .. .. ..$ data   : int [1:2, 1:3] 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.right       :List of 5
#>  ..  .. .. .. ..$ data   : int [1:2, 1:3] 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.bottom:List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:3] 1 1 1 1 1 1
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.top   :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:3] 1 1 1 1 1 1
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.left  :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:3] 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.right :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:3] 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "gray70" "gray70" "gray70" "gray70" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "gray70" "gray70" "gray70" "gray70" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ text.direction     :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "lrtb" "lrtb" "lrtb" "lrtb" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "lrtb"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ background.color   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "gray95" "gray95" "gray95" "gray95" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ hrule              :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "auto" "auto" "auto" "auto" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "auto"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "cell_struct"
#>  ..  .. ..$ pars :List of 21
#>  ..  .. .. ..$ text.align         :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "center" "center" "center" "center" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "left"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.bottom     :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:3] 2 2 2 2 2 2
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.top        :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:3] 2 2 2 2 2 2
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.left       :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:3] 2 2 2 2 2 2
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.right      :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:3] 2 2 2 2 2 2
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ line_spacing       :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:3] 1 1 1 1 1 1
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 1
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.bottom:List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:3] 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.top   :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:3] 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.left  :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:3] 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.right :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:3] 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ shading.color      :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "transparent" "transparent" "transparent" "transparent" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ keep_with_next     :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:2, 1:3] FALSE FALSE FALSE FALSE FALSE FALSE
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ tabs               :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] NA NA NA NA ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "par_struct"
#>  ..  .. ..$ text :List of 11
#>  ..  .. .. ..$ color          :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ font.size      :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:3] 10 10 10 10 10 10
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 11
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ bold           :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:2, 1:3] TRUE TRUE TRUE TRUE TRUE TRUE
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ italic         :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:2, 1:3] FALSE FALSE FALSE FALSE FALSE FALSE
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ underlined     :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:2, 1:3] FALSE FALSE FALSE FALSE FALSE FALSE
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ font.family    :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "Arial" "Arial" "Arial" "Arial" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ hansi.family   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "Arial" "Arial" "Arial" "Arial" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ eastasia.family:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "Arial" "Arial" "Arial" "Arial" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ cs.family      :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "Arial" "Arial" "Arial" "Arial" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ vertical.align :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "baseline" "baseline" "baseline" "baseline" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "baseline"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ shading.color  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:3] "transparent" "transparent" "transparent" "transparent" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "text_struct"
#>  ..  ..- attr(*, "class")= chr "complex_tabpart"
#>  .. $ body      :List of 8
#>  ..  ..$ dataset   :'data.frame':    5 obs. of  3 variables:
#>  ..  .. ..$ Statistic: chr [1:5] "N" "Events n (%)" "Median (95% CI)" "HR (95% CI)" ...
#>  ..  .. ..$ Active   : chr [1:5] "20" "13 (65.0)" "57.5 (25.9, NE)" "Reference" ...
#>  ..  .. ..$ Placebo  : chr [1:5] "20" "14 (70.0)" "20.4 (13.9, NE)" "3.33 (1.36, 8.17)" ...
#>  ..  ..$ content   :List of 5
#>  ..  .. ..$ data   :List of 15
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "N"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "Events n (%)"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "Median (95% CI)"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "HR (95% CI)"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "p-value"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "20"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "13 (65.0)"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "57.5 (25.9, NE)"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "Reference"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "-"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "20"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "14 (70.0)"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "20.4 (13.9, NE)"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "3.33 (1.36, 8.17)"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "0.008"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..- attr(*, "dim")= int [1:2] 5 3
#>  ..  .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. ..$ nrow   : int 5
#>  ..  .. ..$ ncol   : int 3
#>  ..  .. ..$ default:List of 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr ""
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..- attr(*, "class")= chr "paragraph"
#>  ..  .. ..- attr(*, "class")= chr "chunkset_struct"
#>  ..  ..$ col_keys  : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  ..$ colwidths : Named num [1:3] 1.11 1.02 1.09
#>  ..  .. ..- attr(*, "names")= chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  ..$ rowheights: num [1:5] 0.263 0.291 0.291 0.291 0.291
#>  ..  ..$ hrule     : chr [1:5] "auto" "auto" "auto" "auto" ...
#>  ..  ..$ spans     :List of 2
#>  ..  .. ..$ rows   : int [1:5, 1:3] 1 1 1 1 1 1 1 1 1 1 ...
#>  ..  .. ..$ columns: int [1:5, 1:3] 1 1 1 1 1 1 1 1 1 1 ...
#>  ..  ..$ styles    :List of 3
#>  ..  .. ..$ cells:List of 22
#>  ..  .. .. ..$ vertical.align     :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "center" "center" "center" "center" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "center"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ width              :List of 5
#>  ..  .. .. .. ..$ data   : num [1:5, 1:3] NA NA NA NA NA NA NA NA NA NA ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ height             :List of 5
#>  ..  .. .. .. ..$ data   : num [1:5, 1:3] NA NA NA NA NA NA NA NA NA NA ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.bottom      :List of 5
#>  ..  .. .. .. ..$ data   : int [1:5, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.top         :List of 5
#>  ..  .. .. .. ..$ data   : int [1:5, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.left        :List of 5
#>  ..  .. .. .. ..$ data   : int [1:5, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.right       :List of 5
#>  ..  .. .. .. ..$ data   : int [1:5, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.bottom:List of 5
#>  ..  .. .. .. ..$ data   : num [1:5, 1:3] 0 0 0 0 1 0 0 0 0 1 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.top   :List of 5
#>  ..  .. .. .. ..$ data   : num [1:5, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.left  :List of 5
#>  ..  .. .. .. ..$ data   : num [1:5, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.right :List of 5
#>  ..  .. .. .. ..$ data   : num [1:5, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ text.direction     :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "lrtb" "lrtb" "lrtb" "lrtb" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "lrtb"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ background.color   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "white" "white" "white" "white" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ hrule              :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "auto" "auto" "auto" "auto" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "auto"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "cell_struct"
#>  ..  .. ..$ pars :List of 21
#>  ..  .. .. ..$ text.align         :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "left" "left" "left" "left" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "left"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.bottom     :List of 5
#>  ..  .. .. .. ..$ data   : num [1:5, 1:3] 2 2 2 2 2 2 2 2 2 2 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.top        :List of 5
#>  ..  .. .. .. ..$ data   : num [1:5, 1:3] 2 2 2 2 2 2 2 2 2 2 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.left       :List of 5
#>  ..  .. .. .. ..$ data   : num [1:5, 1:3] 2 2 2 2 2 2 2 2 2 2 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.right      :List of 5
#>  ..  .. .. .. ..$ data   : num [1:5, 1:3] 2 2 2 2 2 2 2 2 2 2 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ line_spacing       :List of 5
#>  ..  .. .. .. ..$ data   : num [1:5, 1:3] 1 1 1 1 1 1 1 1 1 1 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 1
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.bottom:List of 5
#>  ..  .. .. .. ..$ data   : num [1:5, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.top   :List of 5
#>  ..  .. .. .. ..$ data   : num [1:5, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.left  :List of 5
#>  ..  .. .. .. ..$ data   : num [1:5, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.right :List of 5
#>  ..  .. .. .. ..$ data   : num [1:5, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ shading.color      :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "transparent" "transparent" "transparent" "transparent" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ keep_with_next     :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:5, 1:3] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ tabs               :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] NA NA NA NA ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "par_struct"
#>  ..  .. ..$ text :List of 11
#>  ..  .. .. ..$ color          :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ font.size      :List of 5
#>  ..  .. .. .. ..$ data   : num [1:5, 1:3] 9 9 9 9 9 9 9 9 9 9 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 11
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ bold           :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:5, 1:3] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ italic         :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:5, 1:3] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ underlined     :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:5, 1:3] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ font.family    :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "Arial" "Arial" "Arial" "Arial" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ hansi.family   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "Arial" "Arial" "Arial" "Arial" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ eastasia.family:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "Arial" "Arial" "Arial" "Arial" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ cs.family      :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "Arial" "Arial" "Arial" "Arial" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ vertical.align :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "baseline" "baseline" "baseline" "baseline" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "baseline"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ shading.color  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:5, 1:3] "transparent" "transparent" "transparent" "transparent" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 5
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "text_struct"
#>  ..  ..- attr(*, "class")= chr "complex_tabpart"
#>  .. $ footer    :List of 8
#>  ..  ..$ dataset   :'data.frame':    4 obs. of  3 variables:
#>  ..  .. ..$ Statistic: chr [1:4] "FAS Population" "Time unit: months" "HR reference group: Active" "NE = Not Estimable"
#>  ..  .. ..$ Active   : chr [1:4] "FAS Population" "Time unit: months" "HR reference group: Active" "NE = Not Estimable"
#>  ..  .. ..$ Placebo  : chr [1:4] "FAS Population" "Time unit: months" "HR reference group: Active" "NE = Not Estimable"
#>  ..  ..$ content   :List of 5
#>  ..  .. ..$ data   :List of 12
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "FAS Population"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "Time unit: months"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "HR reference group: Active"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "NE = Not Estimable"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "FAS Population"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "Time unit: months"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "HR reference group: Active"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "NE = Not Estimable"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "FAS Population"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "Time unit: months"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "HR reference group: Active"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "NE = Not Estimable"
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..- attr(*, "dim")= int [1:2] 4 3
#>  ..  .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. ..$ nrow   : int 4
#>  ..  .. ..$ ncol   : int 3
#>  ..  .. ..$ default:List of 1
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr ""
#>  ..  .. .. .. ..$ font.size      : num NA
#>  ..  .. .. .. ..$ italic         : logi NA
#>  ..  .. .. .. ..$ bold           : logi NA
#>  ..  .. .. .. ..$ underlined     : logi NA
#>  ..  .. .. .. ..$ color          : chr NA
#>  ..  .. .. .. ..$ shading.color  : chr NA
#>  ..  .. .. .. ..$ font.family    : chr NA
#>  ..  .. .. .. ..$ hansi.family   : chr NA
#>  ..  .. .. .. ..$ eastasia.family: chr NA
#>  ..  .. .. .. ..$ cs.family      : chr NA
#>  ..  .. .. .. ..$ vertical.align : chr NA
#>  ..  .. .. .. ..$ width          : num NA
#>  ..  .. .. .. ..$ height         : num NA
#>  ..  .. .. .. ..$ url            : chr NA
#>  ..  .. .. .. ..$ eq_data        : chr NA
#>  ..  .. .. .. ..$ word_field_data: chr NA
#>  ..  .. .. .. ..$ img_data       :List of 1
#>  ..  .. .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ .chunk_index   : int 1
#>  ..  .. .. ..- attr(*, "class")= chr "paragraph"
#>  ..  .. ..- attr(*, "class")= chr "chunkset_struct"
#>  ..  ..$ col_keys  : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  ..$ colwidths : Named num [1:3] 1.11 1.02 1.09
#>  ..  .. ..- attr(*, "names")= chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  ..$ rowheights: num [1:4] 0.25 0.25 0.25 0.25
#>  ..  ..$ hrule     : chr [1:4] "auto" "auto" "auto" "auto"
#>  ..  ..$ spans     :List of 2
#>  ..  .. ..$ rows   : num [1:4, 1:3] 3 3 3 3 0 0 0 0 0 0 ...
#>  ..  .. ..$ columns: num [1:4, 1:3] 1 1 1 1 1 1 1 1 1 1 ...
#>  ..  ..$ styles    :List of 3
#>  ..  .. ..$ cells:List of 22
#>  ..  .. .. ..$ vertical.align     :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "center" "center" "center" "center" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "center"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ width              :List of 5
#>  ..  .. .. .. ..$ data   : num [1:4, 1:3] NA NA NA NA NA NA NA NA NA NA ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ height             :List of 5
#>  ..  .. .. .. ..$ data   : num [1:4, 1:3] NA NA NA NA NA NA NA NA NA NA ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.bottom      :List of 5
#>  ..  .. .. .. ..$ data   : int [1:4, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.top         :List of 5
#>  ..  .. .. .. ..$ data   : int [1:4, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.left        :List of 5
#>  ..  .. .. .. ..$ data   : int [1:4, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.right       :List of 5
#>  ..  .. .. .. ..$ data   : int [1:4, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.bottom:List of 5
#>  ..  .. .. .. ..$ data   : num [1:4, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.top   :List of 5
#>  ..  .. .. .. ..$ data   : num [1:4, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.left  :List of 5
#>  ..  .. .. .. ..$ data   : num [1:4, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.right :List of 5
#>  ..  .. .. .. ..$ data   : num [1:4, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "transparent" "transparent" "transparent" "transparent" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "transparent" "transparent" "transparent" "transparent" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "transparent" "transparent" "transparent" "transparent" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "transparent" "transparent" "transparent" "transparent" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ text.direction     :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "lrtb" "lrtb" "lrtb" "lrtb" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "lrtb"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ background.color   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "transparent" "transparent" "transparent" "transparent" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ hrule              :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "auto" "auto" "auto" "auto" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "auto"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "cell_struct"
#>  ..  .. ..$ pars :List of 21
#>  ..  .. .. ..$ text.align         :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "left" "left" "left" "left" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "left"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.bottom     :List of 5
#>  ..  .. .. .. ..$ data   : num [1:4, 1:3] 5 5 5 5 5 5 5 5 5 5 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.top        :List of 5
#>  ..  .. .. .. ..$ data   : num [1:4, 1:3] 5 5 5 5 5 5 5 5 5 5 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.left       :List of 5
#>  ..  .. .. .. ..$ data   : num [1:4, 1:3] 5 5 5 5 5 5 5 5 5 5 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.right      :List of 5
#>  ..  .. .. .. ..$ data   : num [1:4, 1:3] 5 5 5 5 5 5 5 5 5 5 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ line_spacing       :List of 5
#>  ..  .. .. .. ..$ data   : num [1:4, 1:3] 1 1 1 1 1 1 1 1 1 1 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 1
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.bottom:List of 5
#>  ..  .. .. .. ..$ data   : num [1:4, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.top   :List of 5
#>  ..  .. .. .. ..$ data   : num [1:4, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.left  :List of 5
#>  ..  .. .. .. ..$ data   : num [1:4, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.right :List of 5
#>  ..  .. .. .. ..$ data   : num [1:4, 1:3] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ shading.color      :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "transparent" "transparent" "transparent" "transparent" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ keep_with_next     :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:4, 1:3] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ tabs               :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] NA NA NA NA ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "par_struct"
#>  ..  .. ..$ text :List of 11
#>  ..  .. .. ..$ color          :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ font.size      :List of 5
#>  ..  .. .. .. ..$ data   : num [1:4, 1:3] 8 8 8 8 8 8 8 8 8 8 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: num 11
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ bold           :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:4, 1:3] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ italic         :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:4, 1:3] TRUE TRUE TRUE TRUE TRUE TRUE ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ underlined     :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:4, 1:3] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ font.family    :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "DejaVu Sans" "DejaVu Sans" "DejaVu Sans" "DejaVu Sans" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ hansi.family   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "DejaVu Sans" "DejaVu Sans" "DejaVu Sans" "DejaVu Sans" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ eastasia.family:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "DejaVu Sans" "DejaVu Sans" "DejaVu Sans" "DejaVu Sans" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ cs.family      :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "DejaVu Sans" "DejaVu Sans" "DejaVu Sans" "DejaVu Sans" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ vertical.align :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "baseline" "baseline" "baseline" "baseline" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "baseline"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ shading.color  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:4, 1:3] "transparent" "transparent" "transparent" "transparent" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ keys   : chr [1:3] "Statistic" "Active" "Placebo"
#>  ..  .. .. .. ..$ nrow   : int 4
#>  ..  .. .. .. ..$ ncol   : int 3
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "text_struct"
#>  ..  ..- attr(*, "class")= chr "complex_tabpart"
#>  .. $ col_keys  : chr [1:3] "Statistic" "Active" "Placebo"
#>  .. $ caption   :List of 1
#>  ..  ..$ value: NULL
#>  .. $ blanks    : chr(0) 
#>  .. $ properties:List of 8
#>  ..  ..$ layout          : chr "fixed"
#>  ..  ..$ width           : num 0
#>  ..  ..$ align           : chr "center"
#>  ..  ..$ opts_html       :List of 3
#>  ..  .. ..$ extra_css  : chr ""
#>  ..  .. ..$ scroll     : NULL
#>  ..  .. ..$ extra_class: NULL
#>  ..  .. ..- attr(*, "class")= chr "opts_ft_html"
#>  ..  ..$ opts_word       :List of 3
#>  ..  .. ..$ split         : logi TRUE
#>  ..  .. ..$ keep_with_next: logi FALSE
#>  ..  .. ..$ repeat_headers: logi TRUE
#>  ..  .. ..- attr(*, "class")= chr "opts_ft_word"
#>  ..  ..$ opts_pdf        :List of 7
#>  ..  .. ..$ tabcolsep         : num 2
#>  ..  .. ..$ arraystretch      : num 1.5
#>  ..  .. ..$ float             : chr "none"
#>  ..  .. ..$ default_line_color: chr "black"
#>  ..  .. ..$ caption_repeat    : logi TRUE
#>  ..  .. ..$ footer_repeat     : logi FALSE
#>  ..  .. ..$ fonts_ignore      : logi FALSE
#>  ..  .. ..- attr(*, "class")= chr "opts_ft_pdf"
#>  ..  ..$ word_title      : NULL
#>  ..  ..$ word_description: NULL
#>  .. - attr(*, "class")= chr "flextable"
#>  @ n_rows      : int 5
#>  @ n_cols      : int 3
#>  @ column_names: chr [1:3] "Statistic" "Active" "Placebo"
```
