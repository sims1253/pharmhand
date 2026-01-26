# Create Laboratory Shift Table

Create Laboratory Shift Table

## Usage

``` r
create_lab_shift_table(
  data,
  paramcd = "ALT",
  visit = "Week 24",
  trt_var = "TRT01P",
  title = "Laboratory Shift Table",
  autofit = TRUE
)
```

## Arguments

- data:

  ADLB data frame or ADaMData object

- paramcd:

  Parameter code to analyze

- visit:

  Visit to analyze

- trt_var:

  Treatment variable name (default: "TRT01P")

- title:

  Table title

- autofit:

  Logical, whether to autofit column widths (default: TRUE)

## Value

ClinicalTable object

## Examples

``` r
# Create lab shift table
adlb <- data.frame(
  USUBJID = c("01", "02", "03", "04"),
  TRT01P = c("Placebo", "Placebo", "Active", "Active"),
  PARAMCD = rep("ALT", 4),
  BNRIND = c("Normal", "Normal", "High", "Normal"),
  ANRIND = c("Normal", "High", "High", "Normal"),
  AVISIT = rep("Week 24", 4)
)
table <- create_lab_shift_table(adlb, paramcd = "ALT", visit = "Week 24")
#> Automatically wrapping data.frame in ADaMData object
table@type
#> [1] "lab_shift"
```
