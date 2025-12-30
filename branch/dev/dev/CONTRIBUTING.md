# Contributing to pharmhand

## Getting started

1.  Fork and clone:

``` bash
git clone https://github.com/YOUR_USERNAME/pharmhand.git
```

2.  Install dependencies:

``` r
devtools::install_dev_deps()
```

3.  Create branch:

``` bash
git checkout -b feature/your-feature
```

## System dependencies

Install air:

``` bash
# macOS
brew install posit-dev/tap/air

# Other platforms
# See https://posit-dev.github.io/air/
```

For R CMD check: - **qpdf**: `brew install qpdf` (macOS) or
`sudo apt-get install qpdf` (Ubuntu) - **Pandoc**: Included with RStudio

Skip vignettes:

``` r
devtools::check(build_args = "--no-build-vignettes")
```

## Code style

Use **air** for formatting (not styler): - Line width: 80 characters -
Indent: tabs (width 2)

Format:

``` bash
air format .
```

Check:

``` bash
air format . --check
```

Lint:

``` r
lintr::lint_package()
```

## Testing

Run tests:

``` r
devtools::test()
```

Add tests in `tests/testthat/`.

## Documentation

Document with roxygen2:

``` r
devtools::document()
```

Include `@examples` where practical.

## Before submitting

Run checks:

``` r
air format .
lintr::lint_package()
devtools::test()
devtools::check()
```

Update `NEWS.md`.

## Pull requests

1.  Push branch to fork
2.  Open pull request
3.  Ensure CI passes
4.  Keep PRs focused

## Issues

Report bugs at <https://github.com/sims1253/pharmhand/issues>.

Include minimal reproducible example.

## Code of Conduct

Follows [Contributor
Covenant](https://sims1253.github.io/pharmhand/dev/CODE_OF_CONDUCT.md).
