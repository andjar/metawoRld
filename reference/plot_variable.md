# Plot all reported values of a variable

A descriptive overview (no pooling): every group-level value reported
for the variable, with its reported spread (SD, SE, IQR, range or CI),
coloured by group category.

## Usage

``` r
plot_variable(path = ".", variable, world = NULL)
```

## Arguments

- path:

  Project root.

- variable:

  Variable name (canonical or as extracted).

- world:

  Optional result of
  [`load_metawoRld()`](https://andjar.github.io/metawoRld/reference/load_metawoRld.md).

## Value

Draws the plot; returns the plotted data invisibly.
