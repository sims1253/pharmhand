# Get performance setting

Retrieves a performance setting value.

## Usage

``` r
get_performance_setting(registry, name, default = NULL)
```

## Arguments

- registry:

  A ConfigurationRegistry object

- name:

  Character string name of the setting

- default:

  Default value if setting not found

## Value

The setting value

## Examples

``` r
if (FALSE) { # \dontrun{
registry <- load_config()
batch_size <- get_performance_setting(registry, "docx.batch_size", 50)
} # }
```
