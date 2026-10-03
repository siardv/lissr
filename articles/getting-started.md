# Getting Started with lissr

## Prerequisites

You need a registered account at the [LISS Data
Archive](https://www.dataarchive.lissdata.nl/).

## Store credentials

Store your username and password in the system keyring (macOS Keychain,
Windows Credential Store, or Linux Secret Service) so lissr can retrieve
them automatically; the `keyring` package is a dependency and installs
with lissr. The password is never saved in plain text or in your R
history.

``` r

library(lissr)
liss_store_credentials("1234")
```

## Log in

``` r

liss_login()
```

You will be prompted for a two-factor verification code sent to your
email.

## Browse modules

``` r

liss_modules()
liss_wave_matrix()
```

## Select and download

``` r

selection <- liss_select()
liss_download(selection)
```

## Merge downloaded modules

``` r

results <- merge_liss_modules(
  data_dir = "liss",
  output_dir = "./output"
)
```

The batch detects supported core-module wave files, then uses the
corresponding built-in recipes in alphabetical module-code order. For
each module, it checks the module-code subdirectory (for example
`liss/ch/`) when present, otherwise the main data folder. Existing
recipes still determine which waves can be processed. You do not need to
repeat your module selection or merge a single module first. Modules
without local files are omitted from the automatic batch; warnings about
the recipes and data being processed remain visible.

This uses all supported local files in the folder, including previous
downloads. It does not use the most recent `selection` to restrict the
merge or choose variables for your analysis. Use a dedicated data folder
when you want to keep a download separate.

## Next steps

See [Merging LISS Panel
Data](https://siardv.github.io/lissr/articles/merge-workflow.md) for
output details and optional single-module or custom-recipe control. *(or
run
[`vignette("merge-workflow", package = "lissr")`](https://siardv.github.io/lissr/articles/merge-workflow.md)
in the console)*
