# Analyze Adverse Events by SOC and PT

Analyze Adverse Events by SOC and PT

## Usage

``` r
analyze_soc_pt(data, soc_var = "AEBODSYS", pt_var = "AEDECOD")
```

## Arguments

- data:

  ADAE dataset (or ADaMData containing it)

- soc_var:

  Character string for SOC variable. Default "AEBODSYS"

- pt_var:

  Character string for PT variable. Default "AEDECOD"

## Value

AnalysisResults object
