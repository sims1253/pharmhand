# Visualize Network Geometry

Creates a network graph visualization showing the structure of treatment
comparisons in a network meta-analysis.

## Usage

``` r
create_network_plot(
  nma_result,
  node_size = c("equal", "n_studies", "n_patients"),
  edge_width = c("n_studies", "equal"),
  show_labels = TRUE,
  highlight_ref = TRUE,
  title = "Network Geometry",
  layout = c("circle", "star", "auto"),
  palette = NULL,
  base_size = 11
)
```

## Arguments

- nma_result:

  Result from network_meta() or data with network structure

- node_size:

  Character. Size nodes by "equal", "n_studies", or "n_patients".
  Default: "equal"

- edge_width:

  Character. Width edges by "equal" or "n_studies". Default: "n_studies"

- show_labels:

  Logical. Show edge labels with study counts. Default: TRUE

- highlight_ref:

  Logical. Highlight reference treatment. Default: TRUE

- title:

  Character. Plot title. Default: "Network Geometry"

- layout:

  Character. Layout algorithm: "circle", "star", "auto". Default:
  "circle"

- palette:

  Character vector. Node colors. Default: NULL

- base_size:

  Numeric. Base font size. Default: 11

## Value

A ClinicalPlot object containing the network graph
