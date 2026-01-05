# Load configuration from YAML file

Loads configuration from a YAML file and creates a
ConfigurationRegistry. If no path is provided, loads the default
configuration from the package.

## Usage

``` r
load_config(path = NULL)
```

## Arguments

- path:

  Character string path to YAML configuration file. If NULL, loads the
  default package configuration.

## Value

A ConfigurationRegistry object

## Examples

``` r
if (FALSE) { # \dontrun{
# Load default configuration
registry <- load_config()

# Load custom configuration
registry <- load_config("path/to/config.yaml")
} # }
```
