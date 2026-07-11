# display a module-by-wave availability matrix

prints a cross-tabulation showing which modules have data available in
which waves. uses the cached blueprint if available, otherwise calls
[`liss_blueprint()`](https://siardv.github.io/lissr/reference/liss_blueprint.md)
first.

## Usage

``` r
liss_wave_matrix()
```

## Value

a data frame (invisibly) with modules as rows and waves as columns.
available cells contain a multiplication sign (unicode U+00D7) and
missing cells contain a middle dot (unicode U+00B7).

## Examples

``` r
if (FALSE) { # \dontrun{
liss_wave_matrix()
} # }
```
