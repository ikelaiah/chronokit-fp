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

Add a `release` and `source_ref` entry to `docs/versions.json`. Only add a
historical release after its ref contains the compatible documentation source.
The current release is built from the checkout; older releases are built in
temporary detached worktrees.

## Local verification

```text
python tools/test_build_docs.py
python tools/test_build_all_docs.py
python tools/test_check_built_docs.py
python tools/test_check_docs.py
python tools/check_docs.py
python tools/build_all_docs.py --site-root site --offline-dir artifacts
python tools/check_built_docs.py --site site
```

The resulting archive is `artifacts/chronokit-fp-docs-<version>.zip`, with a
matching `.sha256` checksum. Open `site/index.html` through a local web server
when visually checking the site.
