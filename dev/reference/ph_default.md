# Get Package Default Value

Retrieves a default parameter value, checking options first, then
falling back to built-in defaults.

## Usage

``` r
ph_default(name, default = NULL)
```

## Arguments

- name:

  Character string name of the parameter

- default:

  Optional override default if both option and built-in are NULL

## Value

The default value for the parameter

## Examples

``` r
if (FALSE) { # \dontrun{
# Get default treatment variable
ph_default("trt_var") # "TRT01P"

# Override via option
options(pharmhand.trt_var = "ARM")
ph_default("trt_var") # "ARM"
} # }
```
