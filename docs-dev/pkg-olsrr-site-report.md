# pkg-olsrr-site: rebuild the olsrr site with rsatemplate

- **Branch:** `site/rsatemplate-071` (from `develop`, olsrr 0.7.1)
- **Date:** 2026-11-19
- **Scope:** site only — `DESCRIPTION` (`Config/Needs/website` only), `_pkgdown.yml`, `docs/`, and the removal of the now-obsolete vendored site-kit. No `R/`, `man/`, `NAMESPACE`, `NEWS.md`, or test changes.
- **Commit:** `Site: adopt rsatemplate for the 0.7.1 site` (single commit)

## Environment

| Component | Version / source |
|---|---|
| R | 4.6.1 (2026-06-24 ucrt) |
| pkgdown | 2.2.1 |
| rsatemplate | 0.1.1, installed from `F:/R/Packages/tools/rsatemplate` |
| olsrr | 0.7.1, installed from this worktree (`F:/R/Packages/_worktrees/olsrr-site`) |
| temp library | `C:/Users/DELL/AppData/Local/Temp/opencode/rsalib` (first on `.libPaths()`) |

The build log confirms the installed copy was used:

```
olsrr from: C:/Users/DELL/AppData/Local/Temp/opencode/rsalib/olsrr
olsrr version: 0.7.1
rsatemplate from: C:/Users/DELL/AppData/Local/Temp/opencode/rsalib/rsatemplate
```

## Commands run

Scripts were written to files under the temp directory and run with `Rscript`
(never `Rscript -e` with shell redirection).

```r
# install.R
lib <- "C:/Users/DELL/AppData/Local/Temp/opencode/rsalib"
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))
remotes::install_local("F:/R/Packages/tools/rsatemplate", lib = lib,
                       upgrade = "never", force = TRUE)
remotes::install_local("F:/R/Packages/_worktrees/olsrr-site", lib = lib,
                       upgrade = "never", force = TRUE,
                       dependencies = TRUE, build_vignettes = FALSE)

# build.R
.libPaths(c(lib, .libPaths()))
Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
pkgdown::build_site(pkg = "F:/R/Packages/_worktrees/olsrr-site",
                    preview = FALSE, install = FALSE, new_process = FALSE,
                    quiet = FALSE)
rsatemplate::fix_site("F:/R/Packages/_worktrees/olsrr-site/docs")
```

```text
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/install.R
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/build.R > build.log 2>&1   # exit 0
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/cleanup.R                   # stale docs pages
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/resite.R                    # fix_site again
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/checks.R                    # Rd/alias coverage
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/counts.R                    # page counts
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/canon.R                     # canonical audit
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/links.R                     # internal-link audit
```

`build_site()` was called with `install = FALSE` / `new_process = FALSE` so that
it would use the olsrr 0.7.1 build already installed in the temp library
(a child process would not inherit that `.libPaths()`). `preview = FALSE` as
specified.

## Configuration changes

### `DESCRIPTION`

Only one line added, at the end; `Version: 0.7.1` and everything else untouched:

```yaml
Config/Needs/website: rsquaredacademy/rsatemplate
```

### `_pkgdown.yml`

Replaced the old theme configuration:

```yaml
template:
  bootstrap: 5

templates:
  params:
    bootswatch: cerulean
```

with:

```yaml
template:
  package: rsatemplate
  params:
    site_url: https://olsrr.rsquaredacademy.com
    hero_description: Tools for building ordinary least squares regression models.
    course_topic: OLS regression
    course_url: https://app.rsquaredacademy.com
    dev_repo: rsquaredacademy/olsrr
```

One template-required navbar change: **removed `navbar.type: inverse`**. The
rsatemplate navbar renders `data-bs-theme="{{type}}"`, and Bootstrap 5 only
accepts `light`/`dark`; the old `inverse` value produced
`data-bs-theme="inverse"`. The template supplies its own light/dark navbar
styling and the light switch, so no replacement value is needed.

`navbar` items, `reference`, and `articles` config are unchanged. Two notes:

- The template's navbar automatically appends its own "Learn" link, so the
  existing right-hand items still render after it.
- The `footer:` block in `_pkgdown.yml` is now inert: rsatemplate ships its own
  `footer.html` (sign-up card + analytics opt-out). It was left in place to
  keep the diff minimal; it has no effect on the output.

### Removed vendored site-kit (per rsatemplate README, "Migrating an existing site")

```text
pkgdown/assets/site-kit/          (5 files, site-kit 1.2.0)
pkgdown/templates/in-header.html  (loaded site-kit 1.2.0 a second time)
```

Leaving these would have loaded site-kit 1.2.0 after the template's 1.3.0 and
double-counted GA4 page views. `pkgdown/favicon/` was kept. The built site now
carries site-kit **1.3.0** (`docs/site-kit/VERSION`) and each page has exactly
one `rsa-analytics.js` loader (0 pages with more than one).

## Checks

### 1. Build log and rendering coverage

- `build_site()` exited 0; the log has **no error** (`grep -E "^Error|Error in|Execution halted"` → no matches).
- Sitrep: `✔ URLs ok. ✔ Favicons ok. ✔ Open graph metadata ok.`
- Finished with `── Finished building pkgdown site for package olsrr ──`.

| Source | Count | Output |
|---|---|---|
| `man/*.Rd` | 113 | 113 reference topic pages |
| aliases in those Rd files | 162 | 49 alias redirect stubs (+ 113 topic pages, + `reference/index.html`) |
| `vignettes/*.Rmd` | 7 | 7 article pages (+ `articles/index.html`) |

- Every Rd alias resolves: `aliases without a page: 0`.
- Reference directory totals 163 HTML files = 113 topics + 49 alias
  redirect stubs + index.
- All 7 vignettes render: `heteroskedasticity`, `influence_measures`, `intro`,
  `reference-media`, `regression_diagnostics`, `residual_diagnostics`,
  `variable_selection`.

The only build-log notice is the pre-existing pkgdown accessibility hint
`✖ Icon "fa-github" lacks an aria-label.` It is not an error, the build
completed, and it is unrelated to the template switch (same navbar icon as
before). Left as-is to avoid changing the navbar beyond what the template
requires.

### 2. Admin pages, redirects, sitemap, robots

- `docs/` contains no `AGENTS*`, `GEMINI*`, `CLAUDE*`, or `README*` pages
  (`ls docs | grep -E '^(AGENTS|GEMINI|CLAUDE|README)'` → empty). `fix_site()`
  removed the stale `AGENTS.html`, `AGENTS.md`, `ISSUE_TEMPLATE.html`, and
  `ISSUE_TEMPLATE.md`.
- `docs/_redirects` starts with the managed block:

  ```text
  # >>> rsatemplate: blocked paths (do not edit inside this block)
  /AGENTS.html         /404.html   404!
  ...
  /README.md           /404.html   404!
  /CONTRIBUTING.md     /404.html   404!
  ...
  # <<< rsatemplate: blocked paths
  ```

  I removed the older hand-written duplicate of the same rules that sat below
  the managed block, so future `fix_site()` runs keep the file clean.
- `docs/sitemap.xml` has 130 `<loc>` entries and **no** `AGENTS`,
  `ISSUE_TEMPLATE`, `GEMINI`, `CLAUDE`, or `CONDUCT` entries.
- `docs/robots.txt`:

  ```text
  User-agent: *
  Allow: /

  Sitemap: https://olsrr.rsquaredacademy.com/sitemap.xml
  ```

### 3. CONTRIBUTING link on the home page

- Home page: `href="CONTRIBUTING.html">Contributing guide`.
- `docs/CONTRIBUTING.html` exists (10,226 bytes), generated from
  `.github/CONTRIBUTING.md`. The link now resolves (it 404'd on the live site
  because the older pkgdown build never emitted the page).
- Internal-link audit over all 181 HTML pages found one pre-existing broken
  relative link *inside* that page: `../CODE_OF_CONDUCT.html`, because
  `.github/CONTRIBUTING.md` writes `../CODE_OF_CONDUCT.md` relative to
  `.github/`. pkgdown publishes the page at the site root, so `../` escapes
  `docs/`. This is source-content, not template, and fixing it would mean
  editing `.github/CONTRIBUTING.md` (outside this task's scope); flagging it
  here.

### 4. Version and news

- Home page: `CRAN v0.7.1` badge and navbar `0.7.1`.
- `docs/news/index.html` first version section is `olsrr 0.7.1`
  (`id="olsrr-071"`, CRAN release 2026-11-18), followed by 0.7.0 and 0.6.0.

### 5. Canonical tags

`fix_site()` reports 180 canonicals written; an audit of all pages found
`canonicals not lowercase/extensionless: 0`. Spot checks:

```text
docs/index.html
  <link rel="canonical" href="https://olsrr.rsquaredacademy.com/">
docs/reference/ols_regress.html
  <link rel="canonical" href="https://olsrr.rsquaredacademy.com/reference/ols_regress">
docs/articles/intro.html
  <link rel="canonical" href="https://olsrr.rsquaredacademy.com/articles/intro">
docs/news/index.html
  <link rel="canonical" href="https://olsrr.rsquaredacademy.com/news">
```

## `fix_site()` results

```text
first run:  canonical = 286, redirect_stubs = 49, sitemap_kept = 235,
            removed = AGENTS.html, AGENTS.md, ISSUE_TEMPLATE.html, ISSUE_TEMPLATE.md
final run:  canonical = 180, redirect_stubs = 49, sitemap_kept = 130,
            removed = (none)
```

The drop from 286/235 to 180/130 is the stale-page cleanup below. `fix_site()`
is idempotent; the final run removed nothing.

## Stale outputs removed from `docs/`

`pkgdown::build_site()` does not delete files it no longer generates, and this
`docs/` tree still held pages from pkgdown 1.x and from pre-0.7 function names.
I removed them so the committed site matches the 0.7.1 build:

- 104 orphan `docs/reference/*.html` pages for functions that no longer exist
  (e.g. `ols_stepwise.html`, `all_subset.html`, `ols_rsd_hist.html`,
  `ols_step_backward.html`). They were not in the new sitemap and no page
  links to them. This makes the reference tree exactly 113 topics + 49 alias
  stubs + index.
- `docs/CONDUCT.html` (old copy of the code of conduct; nothing links to it).
- `docs/LICENSE.html` (pkgdown 1.x / Bootstrap 3 page; the current page is
  `LICENSE-text.html`, which the home sidebar links to).
- `docs/jquery.sticky-kit.min.js` (only referenced by the removed
  `LICENSE.html`).
- Five unreferenced `docs/README-*.png` images from an old README.

Hand-maintained files were left untouched and verified unchanged: `docs/CNAME`,
`docs/_headers`, `docs/google02e424888b5e2def.html`, `docs/og_olsrr.png`,
`docs/hex_olsrr.png`.

## Not done / limitations

- No push, no `gh`, no branch switching (per instructions).
- No browser/visual review was possible in this environment; verification is
  HTML/structure-level (template markup, counts, links, canonicals).
- The `.github/CONTRIBUTING.md` → `../CODE_OF_CONDUCT.md` relative link noted
  above is still broken inside the rendered page and was left untouched.
- The pkgdown accessibility hint about the `fa-github` icon's missing
  `aria-label` remains; adding one would require another full site rebuild and
  a navbar change the task did not ask for.
