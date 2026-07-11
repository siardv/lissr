# store LISS Data Archive credentials in the system keyring

saves username and password to the operating system's credential store
(macOS Keychain, Windows Credential Store, or Linux Secret Service) so
that
[`liss_login()`](https://siardv.github.io/lissr/reference/liss_login.md)
can retrieve them automatically.

## Usage

``` r
liss_store_credentials(username, password = NULL)
```

## Arguments

- username:

  character or numeric. your LISS Data Archive username (typically a
  5-digit number).

- password:

  character. if `NULL` (the default), prompts interactively via
  [`keyring::key_set()`](https://keyring.r-lib.org/reference/key_get.html).
  supplying the password directly is discouraged outside of
  non-interactive environments because the value may be recorded in
  `.Rhistory`.

## Value

invisible `TRUE` on success.

## Examples

``` r
if (FALSE) { # \dontrun{
# interactive (recommended) — prompts for password securely
liss_store_credentials(username = 12345)

# non-interactive (CI / automated environments only)
liss_store_credentials(username = 12345, password = Sys.getenv("LISS_PASSWORD"))
} # }
```
