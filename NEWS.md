# lissr 1.0.0

First public release.

* Recipe-driven merge engine. Longitudinal LISS waves are merged from
  declarative YAML recipes that conform to the canonical schema
  (`CANONICAL_SCHEMA.md`, schema version 1.0.0). A recipe captures every
  merge-relevant decision for a module: wave file patterns, variable
  harmonization, boundary handling, comparability contracts, and validation
  checks.
* Controlled action vocabulary with fail-fast validation. Recipes are
  validated before any merge runs via `validate_recipe()` (also called by
  `load_recipe()` and `merge_liss_module()`); unknown actions and malformed
  rules are rejected up front.
* Authoring-time check for unrecognized rule keys. `validate_recipe()` emits a
  non-fatal warning listing any rule-level key that the merge engine neither
  consults nor sanctions as documentation, so mis-named keys are surfaced at
  load time rather than ignored silently. The check is warning-only; every
  recipe still loads and merges unchanged. The recognized set and the
  documentation allow-list are both documented in `CANONICAL_SCHEMA.md`.
* Audit-grade JSONL logging, with a per-run summary artifact.
* Ten built-in module recipes: Assets (ca), Housing (cd), Family and
  Household (cf), Health (ch), Economic Integration (ci), Personality (cp),
  Religion and Ethnicity (cr), Culture and Sports (cs), Politics and Values
  (cv), and Work and Schooling (cw).
* Authentication against the LISS Data Archive with two-factor verification;
  credentials stored via the system keyring.
* Interactive browse, select, and download workflow (`liss_modules()`,
  `liss_wave_matrix()`, `liss_select()`, `liss_download()`).
* New-wave onboarding via `onboard_new_wave()` to extend an existing recipe
  to a newly released wave.
