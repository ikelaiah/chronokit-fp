# Maintaining the ChronoKit-FP documentation site

The static publisher is intentionally dependency-free. It reads the Markdown
under `docs/`, produces a versioned GitHub Pages artifact, and packages an
offline copy.

## Add or reorganise a page

1. Add the Markdown page under `docs/`.
2. Add its path to `required_pages` in `docs/layout.json`.
3. Add it to a `navigation` section for a primary page, or to `hidden_pages`
   when keeping an existing URL without placing it in the main sidebar.
4. Run `python tools/check_docs.py` and the builder checks below.

`hidden_pages` are still generated and indexed so existing bookmarks remain
useful; they are simply not first-class navigation.

## Add a version

Add a `release` and `source_ref` entry to `docs/versions.json`. Always keep
every previously published entry. Set the new release at the TOP and update
`current`. Published releases must reference their immutable tag (`vX.Y.Z`);
never use `main` for a published `source_ref`. A versioned build is produced in
two modes:

- `build_all_docs.py --development-current` (default) builds the declared
  current release from the current checkout and historical releases from their
  tags. Use it to preview unreleased work.
- `build_all_docs.py --released` builds every release, including the current
  one, from its immutable tag. Use it for anything published.

Older release tags predate `layout.json` and `index.md`. The builder falls
back to the curated navigation policy from
`docs/version-navigation-policy.json` (owned by `main`) so historical releases
get a usable, user-facing sidebar instead of a dump of every Markdown file.
Only pages that actually exist in the release source appear; internal documents
such as `PR-*`, `RELEASE-NOTES-*`, API audits/transitions and decision records
stay buildable, searchable and linkable but are not primary navigation. When a
release source has no `index.md`, a version landing page is generated from the
tag's actual content. Historical pages are never rewritten to match the current
template.

A future release whose tag already contains a schema-2 `docs/layout.json` uses
its own explicit navigation; the policy fallback only applies when the release
source lacks `layout.json`.

Run `python tools/check_release.py --pre-tag` before tagging a release and
`--released` after the tag exists. See [../RELEASING.md](../RELEASING.md) for
the full release checklist.

## Local verification

```text
python tools/test_build_docs.py
python tools/test_build_all_docs.py
python tools/test_check_built_docs.py
python tools/test_check_docs.py
python tools/test_check_release.py
python tools/check_docs.py
python tools/check_release.py --pre-tag
python tools/build_all_docs.py --site-root site --offline-dir artifacts
python tools/check_built_docs.py --site site
```

The resulting archive is `artifacts/chronokit-fp-docs-<version>.zip`, with a
matching `.sha256` checksum. Open `site/index.html` through a local web server
when visually checking the site.
