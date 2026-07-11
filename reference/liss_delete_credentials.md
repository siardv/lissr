# delete stored LISS Data Archive credentials

removes the username and password previously saved with
[`liss_store_credentials()`](https://siardv.github.io/lissr/reference/liss_store_credentials.md)
from the system keyring.

## Usage

``` r
liss_delete_credentials(username)
```

## Arguments

- username:

  character or numeric. the username whose credentials should be
  removed.

## Value

invisible `TRUE` on success.

## Examples

``` r
if (FALSE) { # \dontrun{
liss_delete_credentials(username = 12345)
} # }
```
