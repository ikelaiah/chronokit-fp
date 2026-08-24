# Releasing ChronoKit-FP

This guide is the single record of how a release is prepared and published. The
repository enforces most of it mechanically: `tools/check_release.py` and the
Python documentation tests fail when a required step is forgotten, so the
checklist below maps to automated gates rather than maintainer memory.

When preparing a release, **never replace `docs/versions.json` with a
single-version catalogue**. Add the new release at the TOP of `versions`,
update `current`, retain EVERY historical entry, and use the immutable
`vX.Y.Z` tag as `source_ref`. Run the repository release checker before
opening the PR. These rules apply equally to human maintainers and coding
agents.

## Version selection

- Choose a new Semantic Version (`X.Y.Z`) consistent with
  [ROADMAP.md](ROADMAP.md) and the [Changelog](CHANGELOG.md).
- Do not reuse a released version, and never create `vX.Y.Z` tags ahead of the
  release PR they belong to.

## Before the release PR

- [ ] Choose the new Semantic Version.
- [ ] Update the package/library version metadata
      (`packages/lazarus/chronokit_fp.lpk` and the `@version` comment in
      `src/ChronoKit.pas`).
- [ ] Add the CHANGELOG entry under the current heading.
- [ ] Create/update the release notes (`docs/RELEASE-NOTES-vX.Y.Z.md`).
- [ ] Create/update the PR/release summary (`docs/PR-vX.Y.Z.md`).
- [ ] Update the README current-version badge.
- [ ] Update the generated API reference and API cheat sheet as applicable
      (`tools/GenerateApiReference.ps1`, `tools/GenerateApiManifest.ps1`).
- [ ] Update `docs/layout.json` release metadata.
- [ ] Set `docs/versions.json` `current` to the new version.
- [ ] Add the new version at the TOP of `versions`.
- [ ] Set its `source_ref` to `vX.Y.Z`.
- [ ] Retain EVERY previous published version entry.
- [ ] Do not use `main` as a published `source_ref`.
- [ ] Run pre-tag release metadata validation:
      `python tools/check_release.py --pre-tag --expected-version X.Y.Z`.
- [ ] Run unit/tests/examples/docs/package checks (CI on the PR).
- [ ] Create the release PR.
- [ ] Wait for CI.
- [ ] Merge manually.

## After merge

- [ ] Update local `main`.
- [ ] Verify the merged tree is clean.
- [ ] Create an annotated, immutable `vX.Y.Z` tag on merged `main`.
- [ ] Push the tag without force.
- [ ] Create the GitHub Release.
- [ ] Run released metadata validation:
      `python tools/check_release.py --released --expected-version X.Y.Z`.
- [ ] Publish documentation from `main` while sourcing versioned content from
      immutable tags: `python tools/build_all_docs.py --site-root site
      --offline-dir artifacts --released`.
- [ ] Verify every allowed version appears in the selector.
- [ ] Verify the new version is marked `(current)`.
- [ ] Verify historical versions remain selectable.
- [ ] Verify the root docs URL points to the current release.
- [ ] Verify the Pages deployment.
- [ ] Never move a published tag.

## Validation commands

Run all of these locally before opening a release PR:

```text
python tools/test_build_docs.py
python tools/test_build_all_docs.py
python tools/test_check_built_docs.py
python tools/test_check_docs.py
python tools/test_check_release.py
python tools/check_docs.py
python tools/check_release.py --pre-tag --expected-version X.Y.Z
python tools/build_all_docs.py --site-root build/docs-site --released
python tools/check_built_docs.py --site build/docs-site
```

A release PR must not merge while any of these fail.

## Building documentation

`tools/build_all_docs.py` has two modes:

- `--development-current` (default): the declared current release is built
  from the current checkout for previewing unreleased work; historical
  releases are built from their immutable tags.
- `--released`: every release, including the current one, is built from its
  immutable tag. This is the only mode used for publishing.

Releases whose tag predates `docs/layout.json` receive the curated navigation
policy from `docs/version-navigation-policy.json` on `main`; their content stays
immutable while the sidebar stays usable. A release whose tag already contains
a schema-2 `docs/layout.json` uses its own explicit navigation instead.

## Why immutable tags

Published documentation must never change under a version URL. `main` moves
with every commit, so a published version built from `main` would silently
rewrite released documentation. Tagged content is frozen at release time, so
`/1.7.0/` always means the documentation that shipped with v1.7.0.