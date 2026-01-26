# Create Laboratory Summary Table

Create Laboratory Summary Table

## Usage

``` r
create_lab_summary_table(
  data,
  params = c("HGB", "WBC", "PLAT", "ALT", "AST", "BILI", "CREAT"),
  visit = "Week 24",
  trt_var = "TRT01P",
  title = "Laboratory Parameters Summary",
  autofit = TRUE
)
```

## Arguments

- data:

  ADLB data frame or ADaMData object

- params:

  Vector of parameter codes to analyze

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
# Create lab summary table
adlb <- data.frame(
  USUBJID = c("01", "02", "03", "04"),
  TRT01P = c("Placebo", "Placebo", "Active", "Active"),
  PARAMCD = rep("HGB", 4),
  PARAM = rep("Hemoglobin", 4),
  AVISIT = rep("Week 24", 4),
  AVAL = c(14.2, 13.8, 14.5, 14.1)
)
table <- create_lab_summary_table(adlb, params = "HGB", visit = "Week 24")
#> Automatically wrapping data.frame in ADaMData object
table@type
#> [1] "lab_summary"
```
