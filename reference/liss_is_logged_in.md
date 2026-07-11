# check whether the current session is still authenticated

probes the cached session cheaply: when a cached blueprint offers a
known protected file path, a HEAD request against it (deterministic
first entry, no body transfer) decides; otherwise the login page is
requested once and inspected. the probe never scrapes the archive, never
downloads a data file, and never touches the random number generator
(the previous implementation did all three). returns `FALSE` if the
session has expired or was never established.

## Usage

``` r
liss_is_logged_in()
```

## Value

logical scalar.

## Examples

``` r
if (FALSE) { # \dontrun{
liss_login()
liss_is_logged_in()
} # }
```
