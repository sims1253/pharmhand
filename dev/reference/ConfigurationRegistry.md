# ConfigurationRegistry Class

Central registry for all configuration objects. Manages subgroups,
populations, SOC/PT settings, report templates, and performance
settings.

## Usage

``` r
ConfigurationRegistry(
  subgroups = list(),
  populations = list(),
  soc_config = NULL,
  pt_config = NULL,
  report_types = list(),
  performance = list(),
  plots = list(),
  tables = list(),
  validation = list()
)
```

## Arguments

- subgroups:

  Named list of SubgroupConfig objects

- populations:

  Named list of PopulationConfig objects

- soc_config:

  An SOCConfig object

- pt_config:

  A PTConfig object

- report_types:

  List of report type configurations

- performance:

  List of performance settings

- plots:

  List of plot styling settings

- tables:

  List of table styling settings

- validation:

  List of validation settings

## Value

A ConfigurationRegistry object

## Examples

``` r
if (FALSE) { # \dontrun{
registry <- load_config()
# Override a subgroup at runtime
registry <- define_subgroup_config(registry, "custom", "VAR1")
} # }
```
