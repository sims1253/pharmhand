# Generates a league table showing all pairwise treatment comparisons from a network meta-analysis.

Generates a league table showing all pairwise treatment comparisons from
a network meta-analysis.

## Usage

``` r
create_league_table(
  nma_result,
  digits = 2,
  show_ci = TRUE,
  highlight_sig = TRUE,
  conf_level = NULL,
  theme = c("hta", "iqwig", "gba", "clinical")
)
```

## Arguments

- nma_result:

  Result from network_meta()

- digits:

  Integer. Decimal places for estimates. Default: 2

- show_ci:

  Logical. Show confidence intervals. Default: TRUE

- highlight_sig:

  Logical. Highlight significant comparisons. Default: TRUE

- conf_level:

  Numeric. Confidence level. If NULL (default), uses the level from the
  NMA result or falls back to 0.95.

- theme:

  Theme preset: "hta", "iqwig", "gba", or "clinical" (default: "hta")

## Value

[ClinicalTable](https://sims1253.github.io/pharmhand/branch/dev/reference/ClinicalTable.md)
with styled league table

## Examples

``` r
# Create league table for NMA
nma_data <- data.frame(
  study = c("S1", "S2", "S3"),
  treat1 = c("A", "B", "A"),
  treat2 = c("B", "C", "C"),
  effect = log(c(0.75, 0.90, 0.80)),
  se = c(0.12, 0.15, 0.18)
)
nma_result <- network_meta(nma_data, effect_measure = "hr")
table <- create_league_table(nma_result)
print(table)
#> <pharmhand::ClinicalTable>
#>  @ type        : chr "league_table"
#>  @ title       : chr "League Table (HR)"
#>  @ metadata    :List of 4
#>  .. $ theme         : chr "hta"
#>  .. $ effect_measure: chr "hr"
#>  .. $ n_treatments  : int 3
#>  .. $ reference     : chr "A"
#>  @ data        : tibble [3 × 4] (S3: tbl_df/tbl/data.frame)
#>  $ Treatment: chr [1:3] "A" "B" "C"
#>  $ A        : chr [1:3] "A" "0.75 (0.59, 0.95)*" "0.80 (0.56, 1.14)"
#>  $ B        : chr [1:3] "1.33 (1.05, 1.69)*" "B" "1.07 (0.70, 1.63)"
#>  $ C        : chr [1:3] "1.25 (0.88, 1.78)" "0.94 (0.61, 1.43)" "C"
#>  @ flextable   :List of 7
#>  .. $ header    :List of 8
#>  ..  ..$ dataset   :'data.frame':    2 obs. of  4 variables:
#>  ..  .. ..$ Treatment: chr [1:2] "League Table (HR)" "Treatment"
#>  ..  .. ..$ A        : chr [1:2] "League Table (HR)" "A"
#>  ..  .. ..$ B        : chr [1:2] "League Table (HR)" "B"
#>  ..  .. ..$ C        : chr [1:2] "League Table (HR)" "C"
#>  ..  ..$ content   :List of 5
#>  ..  .. ..$ data   :List of 8
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "League Table (HR)"
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
#>  ..  .. .. .. ..$ txt            : chr "Treatment"
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
#>  ..  .. .. .. ..$ txt            : chr "League Table (HR)"
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
#>  ..  .. .. .. ..$ txt            : chr "A"
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
#>  ..  .. .. .. ..$ txt            : chr "League Table (HR)"
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
#>  ..  .. .. .. ..$ txt            : chr "B"
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
#>  ..  .. .. .. ..$ txt            : chr "League Table (HR)"
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
#>  ..  .. .. .. ..$ txt            : chr "C"
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
#>  ..  .. .. ..- attr(*, "dim")= int [1:2] 2 4
#>  ..  .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. ..$ nrow   : int 2
#>  ..  .. ..$ ncol   : int 4
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
#>  ..  ..$ col_keys  : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  ..$ colwidths : Named num [1:4] 0.846 1.14 1.14 1.091
#>  ..  .. ..- attr(*, "names")= chr [1:4] "Treatment" "A" "B" "C"
#>  ..  ..$ rowheights: num [1:2] 0.272 0.272
#>  ..  ..$ hrule     : chr [1:2] "auto" "auto"
#>  ..  ..$ spans     :List of 2
#>  ..  .. ..$ rows   : num [1:2, 1:4] 4 1 0 1 0 1 0 1
#>  ..  .. ..$ columns: num [1:2, 1:4] 1 1 1 1 1 1 1 1
#>  ..  ..$ styles    :List of 3
#>  ..  .. ..$ cells:List of 22
#>  ..  .. .. ..$ vertical.align     :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "center" "center" "center" "center" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "center"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ width              :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:4] NA NA NA NA NA NA NA NA
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ height             :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:4] NA NA NA NA NA NA NA NA
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.bottom      :List of 5
#>  ..  .. .. .. ..$ data   : int [1:2, 1:4] 0 0 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.top         :List of 5
#>  ..  .. .. .. ..$ data   : int [1:2, 1:4] 0 0 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.left        :List of 5
#>  ..  .. .. .. ..$ data   : int [1:2, 1:4] 0 0 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.right       :List of 5
#>  ..  .. .. .. ..$ data   : int [1:2, 1:4] 0 0 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.bottom:List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:4] 1 1 1 1 1 1 1 1
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.top   :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:4] 1 1 1 1 1 1 1 1
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.left  :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:4] 0 0 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.right :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:4] 0 0 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "gray70" "gray70" "gray70" "gray70" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "gray70" "gray70" "gray70" "gray70" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ text.direction     :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "lrtb" "lrtb" "lrtb" "lrtb" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "lrtb"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ background.color   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "gray95" "gray95" "gray95" "gray95" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ hrule              :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "auto" "auto" "auto" "auto" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "auto"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "cell_struct"
#>  ..  .. ..$ pars :List of 21
#>  ..  .. .. ..$ text.align         :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "center" "center" "center" "center" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "left"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.bottom     :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:4] 2 2 2 2 2 2 2 2
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.top        :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:4] 2 2 2 2 2 2 2 2
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.left       :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:4] 2 2 2 2 2 2 2 2
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.right      :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:4] 2 2 2 2 2 2 2 2
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ line_spacing       :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:4] 1 1 1 1 1 1 1 1
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 1
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.bottom:List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:4] 0 0 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.top   :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:4] 0 0 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.left  :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:4] 0 0 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.right :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:4] 0 0 0 0 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ shading.color      :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "transparent" "transparent" "transparent" "transparent" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ keep_with_next     :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:2, 1:4] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ tabs               :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] NA NA NA NA ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "par_struct"
#>  ..  .. ..$ text :List of 11
#>  ..  .. .. ..$ color          :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ font.size      :List of 5
#>  ..  .. .. .. ..$ data   : num [1:2, 1:4] 10 10 10 10 10 10 10 10
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 11
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ bold           :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:2, 1:4] TRUE TRUE TRUE TRUE TRUE TRUE ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ italic         :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:2, 1:4] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ underlined     :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:2, 1:4] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ font.family    :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "Arial" "Arial" "Arial" "Arial" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ hansi.family   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "Arial" "Arial" "Arial" "Arial" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ eastasia.family:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "Arial" "Arial" "Arial" "Arial" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ cs.family      :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "Arial" "Arial" "Arial" "Arial" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ vertical.align :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "baseline" "baseline" "baseline" "baseline" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "baseline"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ shading.color  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:2, 1:4] "transparent" "transparent" "transparent" "transparent" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 2
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "text_struct"
#>  ..  ..- attr(*, "class")= chr "complex_tabpart"
#>  .. $ body      :List of 8
#>  ..  ..$ dataset   :'data.frame':    3 obs. of  4 variables:
#>  ..  .. ..$ Treatment: chr [1:3] "A" "B" "C"
#>  ..  .. ..$ A        : chr [1:3] "A" "0.75 (0.59, 0.95)*" "0.80 (0.56, 1.14)"
#>  ..  .. ..$ B        : chr [1:3] "1.33 (1.05, 1.69)*" "B" "1.07 (0.70, 1.63)"
#>  ..  .. ..$ C        : chr [1:3] "1.25 (0.88, 1.78)" "0.94 (0.61, 1.43)" "C"
#>  ..  ..$ content   :List of 5
#>  ..  .. ..$ data   :List of 12
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "A"
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
#>  ..  .. .. .. ..$ txt            : chr "B"
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
#>  ..  .. .. .. ..$ txt            : chr "C"
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
#>  ..  .. .. .. ..$ txt            : chr "A"
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
#>  ..  .. .. .. ..$ txt            : chr "0.75 (0.59, 0.95)*"
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
#>  ..  .. .. .. ..$ txt            : chr "0.80 (0.56, 1.14)"
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
#>  ..  .. .. .. ..$ txt            : chr "1.33 (1.05, 1.69)*"
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
#>  ..  .. .. .. ..$ txt            : chr "B"
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
#>  ..  .. .. .. ..$ txt            : chr "1.07 (0.70, 1.63)"
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
#>  ..  .. .. .. ..$ txt            : chr "1.25 (0.88, 1.78)"
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
#>  ..  .. .. .. ..$ txt            : chr "0.94 (0.61, 1.43)"
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
#>  ..  .. .. .. ..$ txt            : chr "C"
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
#>  ..  .. .. ..- attr(*, "dim")= int [1:2] 3 4
#>  ..  .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. ..$ nrow   : int 3
#>  ..  .. ..$ ncol   : int 4
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
#>  ..  ..$ col_keys  : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  ..$ colwidths : Named num [1:4] 0.846 1.14 1.14 1.091
#>  ..  .. ..- attr(*, "names")= chr [1:4] "Treatment" "A" "B" "C"
#>  ..  ..$ rowheights: num [1:3] 0.291 0.291 0.291
#>  ..  ..$ hrule     : chr [1:3] "auto" "auto" "auto"
#>  ..  ..$ spans     :List of 2
#>  ..  .. ..$ rows   : int [1:3, 1:4] 1 1 1 1 1 1 1 1 1 1 ...
#>  ..  .. ..$ columns: int [1:3, 1:4] 1 1 1 1 1 1 1 1 1 1 ...
#>  ..  ..$ styles    :List of 3
#>  ..  .. ..$ cells:List of 22
#>  ..  .. .. ..$ vertical.align     :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "center" "center" "center" "center" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "center"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ width              :List of 5
#>  ..  .. .. .. ..$ data   : num [1:3, 1:4] NA NA NA NA NA NA NA NA NA NA ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ height             :List of 5
#>  ..  .. .. .. ..$ data   : num [1:3, 1:4] NA NA NA NA NA NA NA NA NA NA ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.bottom      :List of 5
#>  ..  .. .. .. ..$ data   : int [1:3, 1:4] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.top         :List of 5
#>  ..  .. .. .. ..$ data   : int [1:3, 1:4] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.left        :List of 5
#>  ..  .. .. .. ..$ data   : int [1:3, 1:4] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.right       :List of 5
#>  ..  .. .. .. ..$ data   : int [1:3, 1:4] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.bottom:List of 5
#>  ..  .. .. .. ..$ data   : num [1:3, 1:4] 0 0 1 0 0 1 0 0 1 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.top   :List of 5
#>  ..  .. .. .. ..$ data   : num [1:3, 1:4] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.left  :List of 5
#>  ..  .. .. .. ..$ data   : num [1:3, 1:4] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.right :List of 5
#>  ..  .. .. .. ..$ data   : num [1:3, 1:4] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "black" "black" "gray70" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ text.direction     :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "lrtb" "lrtb" "lrtb" "lrtb" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "lrtb"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ background.color   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "white" "white" "white" "white" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ hrule              :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "auto" "auto" "auto" "auto" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "auto"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "cell_struct"
#>  ..  .. ..$ pars :List of 21
#>  ..  .. .. ..$ text.align         :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "left" "left" "left" "left" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "left"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.bottom     :List of 5
#>  ..  .. .. .. ..$ data   : num [1:3, 1:4] 2 2 2 2 2 2 2 2 2 2 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.top        :List of 5
#>  ..  .. .. .. ..$ data   : num [1:3, 1:4] 2 2 2 2 2 2 2 2 2 2 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.left       :List of 5
#>  ..  .. .. .. ..$ data   : num [1:3, 1:4] 2 2 2 2 2 2 2 2 2 2 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.right      :List of 5
#>  ..  .. .. .. ..$ data   : num [1:3, 1:4] 2 2 2 2 2 2 2 2 2 2 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ line_spacing       :List of 5
#>  ..  .. .. .. ..$ data   : num [1:3, 1:4] 1 1 1 1 1 1 1 1 1 1 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 1
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.bottom:List of 5
#>  ..  .. .. .. ..$ data   : num [1:3, 1:4] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.top   :List of 5
#>  ..  .. .. .. ..$ data   : num [1:3, 1:4] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.left  :List of 5
#>  ..  .. .. .. ..$ data   : num [1:3, 1:4] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.right :List of 5
#>  ..  .. .. .. ..$ data   : num [1:3, 1:4] 0 0 0 0 0 0 0 0 0 0 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "solid" "solid" "solid" "solid" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ shading.color      :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "transparent" "transparent" "transparent" "transparent" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ keep_with_next     :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:3, 1:4] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ tabs               :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] NA NA NA NA ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "par_struct"
#>  ..  .. ..$ text :List of 11
#>  ..  .. .. ..$ color          :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "black" "black" "black" "black" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ font.size      :List of 5
#>  ..  .. .. .. ..$ data   : num [1:3, 1:4] 9 9 9 9 9 9 9 9 9 9 ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 11
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ bold           :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:3, 1:4] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ italic         :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:3, 1:4] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ underlined     :List of 5
#>  ..  .. .. .. ..$ data   : logi [1:3, 1:4] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ font.family    :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "Arial" "Arial" "Arial" "Arial" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ hansi.family   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "Arial" "Arial" "Arial" "Arial" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ eastasia.family:List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "Arial" "Arial" "Arial" "Arial" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ cs.family      :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "Arial" "Arial" "Arial" "Arial" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ vertical.align :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "baseline" "baseline" "baseline" "baseline" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "baseline"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ shading.color  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1:3, 1:4] "transparent" "transparent" "transparent" "transparent" ...
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 3
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "text_struct"
#>  ..  ..- attr(*, "class")= chr "complex_tabpart"
#>  .. $ footer    :List of 8
#>  ..  ..$ dataset   :'data.frame':    1 obs. of  4 variables:
#>  ..  .. ..$ Treatment: chr "Row treatment vs Column treatment. * indicates statistical significance at 95% confidence level."
#>  ..  .. ..$ A        : chr "Row treatment vs Column treatment. * indicates statistical significance at 95% confidence level."
#>  ..  .. ..$ B        : chr "Row treatment vs Column treatment. * indicates statistical significance at 95% confidence level."
#>  ..  .. ..$ C        : chr "Row treatment vs Column treatment. * indicates statistical significance at 95% confidence level."
#>  ..  ..$ content   :List of 5
#>  ..  .. ..$ data   :List of 4
#>  ..  .. .. ..$ :'data.frame':    1 obs. of  19 variables:
#>  ..  .. .. .. ..$ txt            : chr "Row treatment vs Column treatment. * indicates statistical significance at 95% confidence level."
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
#>  ..  .. .. .. ..$ txt            : chr "Row treatment vs Column treatment. * indicates statistical significance at 95% confidence level."
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
#>  ..  .. .. .. ..$ txt            : chr "Row treatment vs Column treatment. * indicates statistical significance at 95% confidence level."
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
#>  ..  .. .. .. ..$ txt            : chr "Row treatment vs Column treatment. * indicates statistical significance at 95% confidence level."
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
#>  ..  .. .. ..- attr(*, "dim")= int [1:2] 1 4
#>  ..  .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. ..$ : NULL
#>  ..  .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. ..$ nrow   : int 1
#>  ..  .. ..$ ncol   : int 4
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
#>  ..  ..$ col_keys  : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  ..$ colwidths : Named num [1:4] 0.846 1.14 1.14 1.091
#>  ..  .. ..- attr(*, "names")= chr [1:4] "Treatment" "A" "B" "C"
#>  ..  ..$ rowheights: num 0.25
#>  ..  ..$ hrule     : chr "auto"
#>  ..  ..$ spans     :List of 2
#>  ..  .. ..$ rows   : int [1, 1:4] 4 0 0 0
#>  ..  .. ..$ columns: int [1, 1:4] 1 1 1 1
#>  ..  ..$ styles    :List of 3
#>  ..  .. ..$ cells:List of 22
#>  ..  .. .. ..$ vertical.align     :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "center" "center" "center" "center"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "center"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ width              :List of 5
#>  ..  .. .. .. ..$ data   : num [1, 1:4] NA NA NA NA
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ height             :List of 5
#>  ..  .. .. .. ..$ data   : num [1, 1:4] NA NA NA NA
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.bottom      :List of 5
#>  ..  .. .. .. ..$ data   : int [1, 1:4] 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.top         :List of 5
#>  ..  .. .. .. ..$ data   : int [1, 1:4] 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.left        :List of 5
#>  ..  .. .. .. ..$ data   : int [1, 1:4] 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ margin.right       :List of 5
#>  ..  .. .. .. ..$ data   : int [1, 1:4] 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: int 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.bottom:List of 5
#>  ..  .. .. .. ..$ data   : num [1, 1:4] 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.top   :List of 5
#>  ..  .. .. .. ..$ data   : num [1, 1:4] 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.left  :List of 5
#>  ..  .. .. .. ..$ data   : num [1, 1:4] 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.right :List of 5
#>  ..  .. .. .. ..$ data   : num [1, 1:4] 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "transparent" "transparent" "transparent" "transparent"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "transparent" "transparent" "transparent" "transparent"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "transparent" "transparent" "transparent" "transparent"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "transparent" "transparent" "transparent" "transparent"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "solid" "solid" "solid" "solid"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "solid" "solid" "solid" "solid"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "solid" "solid" "solid" "solid"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "solid" "solid" "solid" "solid"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ text.direction     :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "lrtb" "lrtb" "lrtb" "lrtb"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "lrtb"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ background.color   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "transparent" "transparent" "transparent" "transparent"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ hrule              :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "auto" "auto" "auto" "auto"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "auto"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "cell_struct"
#>  ..  .. ..$ pars :List of 21
#>  ..  .. .. ..$ text.align         :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "left" "left" "left" "left"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "left"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.bottom     :List of 5
#>  ..  .. .. .. ..$ data   : num [1, 1:4] 5 5 5 5
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.top        :List of 5
#>  ..  .. .. .. ..$ data   : num [1, 1:4] 5 5 5 5
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.left       :List of 5
#>  ..  .. .. .. ..$ data   : num [1, 1:4] 5 5 5 5
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ padding.right      :List of 5
#>  ..  .. .. .. ..$ data   : num [1, 1:4] 5 5 5 5
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 5
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ line_spacing       :List of 5
#>  ..  .. .. .. ..$ data   : num [1, 1:4] 1 1 1 1
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 1
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.bottom:List of 5
#>  ..  .. .. .. ..$ data   : num [1, 1:4] 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.top   :List of 5
#>  ..  .. .. .. ..$ data   : num [1, 1:4] 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.left  :List of 5
#>  ..  .. .. .. ..$ data   : num [1, 1:4] 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.width.right :List of 5
#>  ..  .. .. .. ..$ data   : num [1, 1:4] 0 0 0 0
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 0
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "black" "black" "black" "black"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "black" "black" "black" "black"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "black" "black" "black" "black"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.color.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "black" "black" "black" "black"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.bottom:List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "solid" "solid" "solid" "solid"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.top   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "solid" "solid" "solid" "solid"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.left  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "solid" "solid" "solid" "solid"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ border.style.right :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "solid" "solid" "solid" "solid"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "solid"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ shading.color      :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "transparent" "transparent" "transparent" "transparent"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ keep_with_next     :List of 5
#>  ..  .. .. .. ..$ data   : logi [1, 1:4] FALSE FALSE FALSE FALSE
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ tabs               :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] NA NA NA NA
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr NA
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "par_struct"
#>  ..  .. ..$ text :List of 11
#>  ..  .. .. ..$ color          :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "black" "black" "black" "black"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "black"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ font.size      :List of 5
#>  ..  .. .. .. ..$ data   : num [1, 1:4] 8 8 8 8
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: num 11
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ bold           :List of 5
#>  ..  .. .. .. ..$ data   : logi [1, 1:4] FALSE FALSE FALSE FALSE
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ italic         :List of 5
#>  ..  .. .. .. ..$ data   : logi [1, 1:4] TRUE TRUE TRUE TRUE
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ underlined     :List of 5
#>  ..  .. .. .. ..$ data   : logi [1, 1:4] FALSE FALSE FALSE FALSE
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: logi FALSE
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ font.family    :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "DejaVu Sans" "DejaVu Sans" "DejaVu Sans" "DejaVu Sans"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ hansi.family   :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "DejaVu Sans" "DejaVu Sans" "DejaVu Sans" "DejaVu Sans"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ eastasia.family:List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "DejaVu Sans" "DejaVu Sans" "DejaVu Sans" "DejaVu Sans"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ cs.family      :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "DejaVu Sans" "DejaVu Sans" "DejaVu Sans" "DejaVu Sans"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "DejaVu Sans"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ vertical.align :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "baseline" "baseline" "baseline" "baseline"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "baseline"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..$ shading.color  :List of 5
#>  ..  .. .. .. ..$ data   : chr [1, 1:4] "transparent" "transparent" "transparent" "transparent"
#>  ..  .. .. .. .. ..- attr(*, "dimnames")=List of 2
#>  ..  .. .. .. .. .. ..$ : NULL
#>  ..  .. .. .. .. .. ..$ : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ keys   : chr [1:4] "Treatment" "A" "B" "C"
#>  ..  .. .. .. ..$ nrow   : int 1
#>  ..  .. .. .. ..$ ncol   : int 4
#>  ..  .. .. .. ..$ default: chr "transparent"
#>  ..  .. .. .. ..- attr(*, "class")= chr "fpstruct"
#>  ..  .. .. ..- attr(*, "class")= chr "text_struct"
#>  ..  ..- attr(*, "class")= chr "complex_tabpart"
#>  .. $ col_keys  : chr [1:4] "Treatment" "A" "B" "C"
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
#>  @ footnotes   : chr "Row treatment vs Column treatment. * indicates statistical significance at 95% confidence level."
#>  @ n_rows      : int 3
#>  @ n_cols      : int 4
#>  @ column_names: chr [1:4] "Treatment" "A" "B" "C"
```
