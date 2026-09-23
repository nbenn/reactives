
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reactives

<!-- badges: start -->

[![ci](https://github.com/nbenn/reactives/actions/workflows/ci.yaml/badge.svg)](https://github.com/nbenn/reactives/actions/workflows/ci.yaml)
[![covr](https://codecov.io/gh/nbenn/reactives/graph/badge.svg?token=S882X4B4YH)](https://codecov.io/gh/nbenn/reactives)
<!-- badges: end -->

Keyed collections of shiny reactives whose keys can be added, removed
and reordered, with dependencies tracked per key.

Shiny’s reactive building blocks leave a gap:

|          | A single one    | A keyed collection |
|----------|-----------------|--------------------|
| Stored   | `reactiveVal()` | `reactiveValues()` |
| Computed | `reactive()`    | none               |

A `reactiveValues()` object can gain keys, but it can’t remove or
reorder them
([rstudio/shiny#2439](https://github.com/rstudio/shiny/issues/2439)),
and shiny has no keyed collection of computed values. This package
provides a collection that fills both gaps.

## Background

The package grows out of two workarounds in
[blockr.core](https://github.com/BristolMyersSquibb/blockr.core). One is
`trim_rv()`, which removes keys from a `reactiveValues()` object through
shiny’s internals. The other is the `reactives` class, which holds the
computed inputs of variadic blocks.

## Status

Under development. Planned work is tracked in the
[issues](https://github.com/nbenn/reactives/issues).

## Installation

``` r
# install.packages("pak")
pak::pak("nbenn/reactives")
```
