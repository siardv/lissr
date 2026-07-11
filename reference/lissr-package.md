# lissr: View, Download, and Merge LISS Panel Data

Programmatic access to the LISS Data Archive
(<https://www.lissdata.nl/>). Authenticate with two-factor verification,
browse available modules and waves, interactively select and download
data files, and merge longitudinal waves using recipe-driven YAML
specifications conforming to a canonical schema (v1.0.0). Credentials
are stored securely via the system keyring.

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
