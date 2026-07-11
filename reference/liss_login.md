# log in to the LISS Data Archive

authenticates with username and password (retrieved from the system
keyring via
[`keyring::key_get()`](https://keyring.r-lib.org/reference/key_get.html))
and handles two-factor verification. stores the authenticated session in
an internal cache for use by all other `liss_*` functions.

## Usage

``` r
liss_login(username = NULL)
```

## Arguments

- username:

  character or numeric. the LISS archive username (typically a 5-digit
  number). if `NULL` (the default), looks up saved credentials via
  [`keyring::key_list()`](https://keyring.r-lib.org/reference/key_get.html).
  when exactly one set of credentials is stored, that username is used
  automatically. if no credentials are saved, prompts interactively for
  username and password.

## Value

the authenticated
[rvest::session](https://rvest.tidyverse.org/reference/session.html)
(invisibly), or `NULL` on failure.

## Examples

``` r
if (FALSE) { # \dontrun{
# store credentials once (interactive prompt)
liss_store_credentials(username = 12345)

# then log in — credentials are picked up automatically
liss_login()
} # }
```
