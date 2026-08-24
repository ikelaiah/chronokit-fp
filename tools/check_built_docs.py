#!/usr/bin/env python3
"""Validate local links and release metadata in a built ChronoKit-FP docs site."""

from __future__ import annotations

import argparse
import json
import re
from functools import lru_cache
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit


class PageParser(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.links: list[str] = []
        self.identifiers: set[str] = set()
        self.duplicate_identifiers: set[str] = set()
        self.release_values: list[str] = []
        self.version_targets: list[str] = []
        self.title_count = 0
        self.has_selector = False
        self.version_options: list[tuple[str, str, bool]] = []
        self._option_value: str | None = None
        self._option_selected = False
        self._option_label: list[str] = []

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        values = dict(attrs)
        if values.get("id"):
            identifier = str(values["id"])
            if identifier in self.identifiers:
                self.duplicate_identifiers.add(identifier)
            self.identifiers.add(identifier)
        if tag == "select" and values.get("id") == "version-select":
            self.has_selector = True
        if tag == "option":
            self._option_value = str(values.get("value", ""))
            self._option_selected = "selected" in values
            self._option_label = []
        if tag in {"a", "link"} and values.get("href"):
            self.links.append(str(values["href"]))
        if tag in {"img", "script"} and values.get("src"):
            self.links.append(str(values["src"]))
        if tag == "meta" and values.get("name") == "chronokit-release" and values.get("content"):
            self.release_values.append(str(values["content"]))
        if tag == "option" and values.get("value"):
            self.version_targets.append(str(values["value"]))
        if tag == "title":
            self.title_count += 1

    def handle_data(self, data: str) -> None:
        if self._option_value is not None:
            self._option_label.append(data)

    def handle_endtag(self, tag: str) -> None:
        if tag == "option" and self._option_value is not None:
            label = "".join(self._option_label).strip()
            self.version_options.append((self._option_value, label, self._option_selected))
            self._option_value = None


@lru_cache(maxsize=None)
def parse_page(path: Path, content: str) -> PageParser:
    parser = PageParser()
    parser.feed(content)
    return parser


def page_data(path: Path) -> PageParser:
    return parse_page(path, path.read_text(encoding="utf-8"))


def local_target(page: Path, raw_link: str, site: Path) -> tuple[Path | None, str]:
    split = urlsplit(raw_link)
    if split.scheme or split.netloc or raw_link.startswith(("mailto:", "javascript:")):
        return None, ""
    target = page if not split.path else (page.parent / unquote(split.path)).resolve()
    try:
        target.relative_to(site.resolve())
    except ValueError:
        return None, "escapes built site"
    if target.is_dir():
        target = target / "index.html"
    return target, unquote(split.fragment)


def check_page(page: Path, site: Path, release: str, current: str, releases: set[str]) -> list[str]:
    errors: list[str] = []
    parsed = page_data(page)
    source = page.read_text(encoding="utf-8")
    if parsed.release_values != [release]:
        errors.append(f"{page}: release metadata {parsed.release_values!r}, expected [{release!r}]")
    if parsed.title_count != 1:
        errors.append(f"{page}: expected exactly one page title")
    for identifier in sorted(parsed.duplicate_identifiers):
        errors.append(f"{page}: duplicate id: {identifier}")
    if re.search(r"(?:^|[\"'\s])(?:file://|[A-Za-z]:[\\/])", source, re.IGNORECASE):
        errors.append(f"{page}: contains an absolute local filesystem path")
    for link in parsed.links:
        split = urlsplit(link)
        if split.scheme.lower() not in {"", "http", "https", "mailto"}:
            errors.append(f"{page}: unsafe link: {link}")
            continue
        target, fragment = local_target(page, link, site)
        if target is None:
            if fragment == "escapes built site":
                errors.append(f"{page}: local link escapes built site: {link}")
            continue
        if not target.is_file():
            errors.append(f"{page}: missing link target: {link}")
            continue
        if fragment and target.suffix.lower() == ".html" and fragment not in page_data(target).identifiers:
            errors.append(f"{page}: missing link anchor: {link}")
    for target_value in parsed.version_targets:
        target, _fragment = local_target(page, target_value, site)
        if target is None or not target.is_file():
            errors.append(f"{page}: missing version target: {target_value}")
    if parsed.has_selector:
        option_releases: set[str] = set()
        for _target, label, selected in parsed.version_options:
            match = re.search(r"\bv(\d+\.\d+\.\d+)\b", label)
            if not match:
                errors.append(f"{page}: version option without a release label: {label!r}")
                continue
            option_releases.add(match.group(1))
            if match.group(1) == release and not selected:
                errors.append(f"{page}: viewed release {release} is not selected in the version selector")
            if label.endswith("(current)") and match.group(1) != current:
                errors.append(f"{page}: non-current release {match.group(1)} is labelled (current)")
            if match.group(1) == current and not label.endswith("(current)"):
                errors.append(f"{page}: current release {current} is not labelled (current)")
        if option_releases != releases:
            missing = sorted(releases - option_releases)
            extra = sorted(option_releases - releases)
            detail = []
            if missing:
                detail.append(f"missing: {', '.join(missing)}")
            if extra:
                detail.append(f"extra: {', '.join(extra)}")
            errors.append(f"{page}: version selector does not match the catalogue ({'; '.join(detail)})")
        current_labels = [label for _target, label, _selected in parsed.version_options if "(current)" in label]
        if len(current_labels) != 1:
            errors.append(f"{page}: expected exactly one (current) version, found {len(current_labels)}")
    return errors


def check_site(site: Path) -> list[str]:
    site = site.resolve()
    errors: list[str] = []
    try:
        manifest = json.loads((site / "versions.json").read_text(encoding="utf-8"))
        current = str(manifest["current"])
        entries = manifest["versions"]
        if not isinstance(entries, list) or not entries:
            raise ValueError("versions must be a non-empty list")
    except (OSError, KeyError, TypeError, ValueError, json.JSONDecodeError) as exc:
        return [f"{site / 'versions.json'}: invalid version metadata: {exc}"]
    try:
        release_names = {str(entry["release"]) for entry in entries}
    except (KeyError, TypeError) as exc:
        return [f"{site / 'versions.json'}: invalid release record: {exc}"]
    landing = site / "index.html"
    if not landing.is_file():
        errors.append(f"{landing}: missing landing page")
    elif f"{current}/index.html" not in landing.read_text(encoding="utf-8"):
        errors.append(f"{landing}: does not point to current release {current}")

    for entry in entries:
        try:
            release = str(entry["release"])
            directory = site / release
            identity = json.loads((directory / "release.json").read_text(encoding="utf-8"))
            if identity.get("release") != release or not identity.get("source_ref"):
                raise ValueError("release/source_ref identity mismatch")
            indexed = json.loads((directory / "search-index.json").read_text(encoding="utf-8"))
            if not isinstance(indexed, list) or not indexed:
                raise ValueError("search index must be a non-empty list")
            for item in indexed:
                indexed_page = directory / item["url"]
                if not indexed_page.is_file():
                    errors.append(f"{directory / 'search-index.json'}: missing indexed page {item['url']}")
        except (OSError, KeyError, TypeError, ValueError, json.JSONDecodeError) as exc:
            errors.append(f"{directory}: invalid release output: {exc}")
            continue
        for asset in ("assets/site.css", "assets/site.js", "search-index.js"):
            if not (directory / asset).is_file():
                errors.append(f"{directory}: missing required asset: {asset}")
        pages = sorted(directory.rglob("*.html"))
        if not pages:
            errors.append(f"{directory}: no HTML pages")
        for page in pages:
            errors.extend(check_page(page, site, release, current, release_names))
    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--site", type=Path, required=True)
    args = parser.parse_args()
    errors = check_site(args.site)
    if errors:
        print("\n".join(errors))
        return 1
    print(f"Built documentation checks passed: {args.site}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
