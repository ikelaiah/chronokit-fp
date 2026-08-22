# ADR-0002: Publish versioned static documentation with the local Python toolchain

## Status

Accepted

## Context

ChronoKit-FP needs a polished documentation site that works on GitHub Pages,
keeps its source alongside the Pascal library, can produce an offline archive,
and does not require a JavaScript package manager or hosted search service.
Existing user-facing guides should stay linkable, while implementation audits,
release preparation notes, and earlier design records must not crowd the main
navigation.

## Decision

Use the repository's dependency-free Python publisher.
`docs/layout.json` defines the page hierarchy and homepage content.
`docs/versions.json` defines the current release and future source refs.
Pages in `hidden_pages` are built and searchable for compatibility but are not
shown in the curated sidebar. The publisher emits static HTML, a client-side
search index, version redirects, a deterministic offline ZIP, and its SHA-256
checksum.

The documentation workflow tests source checks and the Python toolchain,
builds every configured release path, validates the generated site, then
deploys the artifact with GitHub Pages.

## Consequences

- A new primary page must be added to `navigation` and `required_pages` in
  `docs/layout.json`.
- A preserved compatibility page belongs in `hidden_pages` and
  `required_pages` instead.
- A future release adds a `release`/`source_ref` entry to `docs/versions.json`
  only once its documentation source exists at that ref.
- The source docs stay portable and reviewable as Markdown; generated `site/`
  and offline archives are build outputs.
