# check LISS Data Archive availability

sends a HEAD request to the LISS Data Archive website and reports
whether the server responds with HTTP 200.

## Usage

``` r
liss_server_status(verbose = TRUE)
```

## Arguments

- verbose:

  logical. print status messages to the console?

## Value

invisible logical. `TRUE` if the website is accessible.

## Examples

``` r
if (FALSE) { # \dontrun{
liss_server_status()
} # }
```
