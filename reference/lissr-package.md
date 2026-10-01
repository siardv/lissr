# lissr: Access, Download, Harmonize, and Merge LISS Panel Data

Programmatic access to the LISS Data Archive
(<https://www.lissdata.nl/>). Authenticate with two-factor verification,
browse available modules and waves, and interactively select and
download longitudinal survey data. Schema-validated YAML recipes make
variable harmonization, comparability rules, and validation checks
explicit when merging LISS panel waves. Transformation logs, input-file
hashes, and versioned provenance support reproducible research
workflows. A rule-driven household-income cleaning framework produces
decision ledgers and reports. Credentials are stored securely via the
system keyring.

## See also

The canonical recipe schema that every merge recipe must satisfy,
shipped with the package and locatable via
`system.file("schema", "CANONICAL_SCHEMA.md", package = "lissr")`.
Primary entry points:
[`merge_liss_module()`](https://siardv.github.io/lissr/reference/merge_liss_module.md),
[`load_recipe()`](https://siardv.github.io/lissr/reference/load_recipe.md),
and
[`validate_recipe()`](https://siardv.github.io/lissr/reference/validate_recipe.md).

## Author

**Maintainer**: Siard van den Bosch <siardvandenbosch@me.com>
\[copyright holder\]

Authors:

- Siard van den Bosch <siardvandenbosch@me.com> \[copyright holder\]
