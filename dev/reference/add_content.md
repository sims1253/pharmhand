# Add a table to a StudyResult

Add a table to a StudyResult

Add a plot to a StudyResult

Add a section to a ClinicalReport

Add content to a ReportSection

## Usage

``` r
add_table(obj, table, name = NULL)

add_plot(obj, plot, name = NULL)

add_section(obj, section, name = NULL)

add_content(obj, content, name = NULL)
```

## Arguments

- obj:

  A ReportSection object

- table:

  A ClinicalTable object

- name:

  Character string for content name/identifier

- plot:

  A ClinicalPlot object

- section:

  A ReportSection object

- content:

  A ClinicalContent object

## Value

The modified StudyResult object

The modified StudyResult object

The modified ClinicalReport object

The modified ReportSection object
