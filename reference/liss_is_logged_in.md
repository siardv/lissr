# check whether the current session is still authenticated

verifies the cached session by attempting to download a random data file
from the archive. returns `FALSE` if the session has expired or was
never established.

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
