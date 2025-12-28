# Contributing to pharmhand

Contributions are welcome and appreciated.

## Getting started

1.  Fork the repository and clone your fork
2.  Install dependencies: `devtools::install_dev_deps()`
3.  Create a branch for your changes:
    `git checkout -b feature/your-feature`

## Development workflow

### Code style

This package follows the [tidyverse style
guide](https://style.tidyverse.org/). Before submitting:

``` r
# Format code
styler::style_pkg()

# Check linting
lintr::lint_package()
```

### Testing

Add tests for new functionality in `tests/testthat/`. Run tests with:

``` r
devtools::test()
```

### Documentation

- Use roxygen2 for function documentation
- Include `@examples` where practical
- Run `devtools::document()` to update docs

### Checking

Before submitting a PR, run:

``` r
devtools::check()
```

## Pull requests

1.  Update `NEWS.md` with a summary of changes
2.  Ensure CI checks pass
3.  Keep PRs focused on a single feature or fix

## Reporting issues

Use [GitHub Issues](https://github.com/sims1253/pharmhand/issues) to
report bugs or request features. Include a minimal reproducible example
when reporting bugs.

## Code of Conduct

Please note that this project has a [Code of
Conduct](https://sims1253.github.io/pharmhand/CODE_OF_CONDUCT.md). By
participating, you agree to abide by its terms.
