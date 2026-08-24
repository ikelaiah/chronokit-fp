#!/usr/bin/env python3
"""Validate ChronoKit-FP release metadata and the docs/versions.json catalogue.

Two validation phases are supported:

--pre-tag      before the current release tag exists (release preparation PR)
--released     after the current release tag exists (released/published docs)

Pre-tag validation accepts a planned current source_ref such as ``v1.8.0`` even
when tag ``v1.8.0`` has not been created yet. Released validation requires every
catalogue source_ref to resolve and every published historical release to remain
listed.

Historical truth is derived from immutable git tags, cross-checked against
CHANGELOG release headings, so a future release can never silently drop an older
version from the selector. Tags are listed from the local repository; CI must
fetch tags (full history) so shallow clones do not hide historical releases.
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
from dataclasses import dataclass
from pathlib import Path

SUPPORTED_SCHEMA = 1
SEMVER_PATTERN = re.compile(r"^(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<patch>0|[1-9]\d*)$")
TAG_VERSION_PATTERN = re.compile(r"^v[.\-]?(?P<version>\d+\.\d+\.\d+)(?:[.\-].*)?$")
MOVING_SOURCE_REFS = ("", "main", "master", "HEAD", "head")
CHANGELOG_HEADING = re.compile(r"(?m)^##\s*\[\s*(?P<version>\d+\.\d+\.\d+)\s*\]")
PACKAGE_VERSION_PATTERN = re.compile(r'<Version\s+Major="?(?P<major>\d+)"?\s+Minor="?(?P<minor>\d+)"?')
SOURCE_VERSION_PATTERN = re.compile(r"@version\s+(?P<version>\d+\.\d+\.\d+)")
README_VERSION_PATTERN = re.compile(r"[Vv]ersion-(?P<version>\d+\.\d+\.\d+)")
API_MANIFEST_VERSION_PATTERN = re.compile(r"ChronoKit-v(?P<version>\d+\.\d+\.\d+)-")
CHEAT_SHEET_PREFERRED_PATTERN = re.compile(r"preferred\s+v(?P<major>\d+)\.(?P<minor>\d+)(?![0-9.])")


@dataclass(frozen=True)
class Catalogue:
    current: str
    versions: list[dict[str, str]]

    def release(self, name: str) -> dict[str, str] | None:
        return next((entry for entry in self.versions if entry["release"] == name), None)


def tag_name_for(release: str) -> str:
    return f"v{release}"


def load_catalogue(path: Path) -> Catalogue:
    try:
        metadata = json.loads(path.read_text(encoding="utf-8"))
        schema = metadata.get("schema_version")
        if schema != SUPPORTED_SCHEMA:
            raise ValueError(f"unsupported schema_version {schema!r}; expected {SUPPORTED_SCHEMA}")
        current = metadata["current"]
        raw_versions = metadata["versions"]
        if not isinstance(raw_versions, list):
            raise ValueError("versions must be an array")
        versions = [
            {"release": str(entry["release"]), "source_ref": str(entry["source_ref"])}
            for entry in raw_versions
        ]
        return Catalogue(str(current), versions)
    except (OSError, KeyError, TypeError, ValueError, json.JSONDecodeError) as exc:
        raise ValueError(f"invalid version catalogue {path}: {exc}") from exc


def catalogue_errors(catalogue: Catalogue) -> list[str]:
    errors: list[str] = []
    current = catalogue.current
    versions = catalogue.versions

    if not current:
        errors.append("current must be a non-empty release name")
    if not versions:
        errors.append("versions must be a non-empty list")
        return errors

    releases = [entry["release"] for entry in versions]
    duplicates = {release for release in releases if releases.count(release) > 1}
    if duplicates:
        errors.append(f"duplicate release entry: {', '.join(sorted(duplicates))}")
    if current not in releases:
        errors.append("current is absent from versions")
    if releases.count(current) > 1:
        errors.append("current appears more than once in versions")
    elif releases and releases[0] != current:
        errors.append("current release must be the first entry")

    for entry in versions:
        release = entry["release"]
        source_ref = entry["source_ref"]
        if not release:
            errors.append("release names must be non-empty")
        elif not SEMVER_PATTERN.match(release):
            errors.append(f"release {release!r} is not a semantic version X.Y.Z")
        if not source_ref:
            errors.append(f"release {release} has an empty source_ref")
        elif source_ref in MOVING_SOURCE_REFS:
            errors.append(f"release {release} must not use moving source_ref {source_ref!r}")
        elif not TAG_VERSION_PATTERN.match(source_ref):
            errors.append(f"release {release} source_ref {source_ref!r} is not an immutable tag reference")

    for entry in versions:
        release = entry["release"]
        source_ref = entry["source_ref"]
        if release == current:
            if source_ref != tag_name_for(current):
                errors.append(f"current release source_ref must be {tag_name_for(current)!r}, not {source_ref!r}")
        else:
            match = TAG_VERSION_PATTERN.match(source_ref)
            if match and SEMVER_PATTERN.match(release) and match.group("version") != release:
                errors.append(f"release {release} source_ref {source_ref!r} names a different version")

    semver_versions = [entry for entry in versions if SEMVER_PATTERN.match(entry["release"])]
    for left, right in zip(semver_versions, semver_versions[1:]):
        a = tuple(int(part) for part in left["release"].split("."))
        b = tuple(int(part) for part in right["release"].split("."))
        if a < b:
            errors.append(
                f"versions must be ordered newest-to-oldest: {left['release']} before {right['release']}"
            )
    return errors


def first_version(content: str, pattern: re.Pattern[str]) -> str | None:
    match = pattern.search(content)
    return match.group(1) if match else None


def expect_version_file(label: str, path: Path, pattern: re.Pattern[str], errors: list[str], current: str) -> None:
    if not path.is_file():
        errors.append(f"{label} file is missing: {path}")
        return
    content = path.read_text(encoding="utf-8", errors="replace")
    found = first_version(content, pattern)
    if found is None:
        errors.append(f"{label} does not declare a version: {path}")
    elif found != current:
        errors.append(f"{label} declares {found}, expected {current}: {path}")


def metadata_errors(root: Path, current: str) -> list[str]:
    errors: list[str] = []
    match = SEMVER_PATTERN.match(current)
    if not match:
        return [f"current {current!r} is not a semantic version X.Y.Z"]
    major, minor = match.group("major"), match.group("minor")

    lpk_path = root / "packages" / "lazarus" / "chronokit_fp.lpk"
    if lpk_path.is_file():
        content = lpk_path.read_text(encoding="utf-8", errors="replace")
        package_match = PACKAGE_VERSION_PATTERN.search(content)
        if not package_match:
            errors.append(f"package metadata does not declare a version: {lpk_path}")
        elif f"{package_match.group('major')}.{package_match.group('minor')}" != f"{major}.{minor}":
            errors.append(
                f"package metadata declares {package_match.group('major')}.{package_match.group('minor')}, "
                f"expected {major}.{minor}: {lpk_path}"
            )
    else:
        errors.append(f"package metadata file is missing: {lpk_path}")

    expect_version_file("library source", root / "src" / "ChronoKit.pas", SOURCE_VERSION_PATTERN, errors, current)
    expect_version_file("README version badge", root / "README.md", README_VERSION_PATTERN, errors, current)

    changelog_path = root / "CHANGELOG.md"
    if not changelog_path.is_file():
        errors.append(f"CHANGELOG file is missing: {changelog_path}")
    else:
        content = changelog_path.read_text(encoding="utf-8", errors="replace")
        headings = CHANGELOG_HEADING.findall(content)
        if not headings:
            errors.append(f"CHANGELOG declares no release headings: {changelog_path}")
        elif headings[0] != current:
            errors.append(f"CHANGELOG current entry is {headings[0]}, expected {current}: {changelog_path}")

    layout_path = root / "docs" / "layout.json"
    if not layout_path.is_file():
        errors.append(f"layout metadata file is missing: {layout_path}")
    else:
        try:
            layout = json.loads(layout_path.read_text(encoding="utf-8"))
            if str(layout.get("release")) != current:
                errors.append(f"layout metadata declares {layout.get('release')!r}, expected {current!r}: {layout_path}")
        except (json.JSONDecodeError, OSError) as exc:
            errors.append(f"layout metadata is unreadable: {layout_path}: {exc}")

    for label, filename in (("release notes", f"RELEASE-NOTES-v{current}.md"), ("PR summary", f"PR-v{current}.md")):
        document = root / "docs" / filename
        if not document.is_file():
            errors.append(f"{label} document is missing: {document}")
        elif current not in document.read_text(encoding="utf-8", errors="replace"):
            errors.append(f"{label} document does not mention {current}: {document}")

    api_dir = root / "api"
    if not api_dir.is_dir():
        errors.append(f"API manifest directory is missing: {api_dir}")
    else:
        versions_found = {
            match.group("version")
            for manifest in api_dir.glob("ChronoKit-v*.txt")
            if (match := API_MANIFEST_VERSION_PATTERN.search(manifest.name))
        }
        if not versions_found:
            errors.append(f"API manifests declare no versioned files in {api_dir}")
        elif current not in versions_found:
            errors.append(f"API manifests are frozen at {', '.join(sorted(versions_found))}, expected {current}: {api_dir}")

    cheat_sheet = root / "docs" / "Cheat-Sheet.md"
    if cheat_sheet.is_file():
        content = cheat_sheet.read_text(encoding="utf-8", errors="replace")
        preferred = CHEAT_SHEET_PREFERRED_PATTERN.findall(content)
        if preferred and any(f"{a}.{b}" != f"{major}.{minor}" for a, b in preferred):
            errors.append(
                f"cheat sheet marks a different preferred version than {major}.{minor}: {cheat_sheet}"
            )
    return errors


def git_present(root: Path) -> bool:
    result = subprocess.run(
        ["git", "-C", str(root), "rev-parse", "--is-inside-work-tree"],
        text=True, capture_output=True, check=False,
    )
    return result.returncode == 0


def git_ref_resolves(root: Path, source_ref: str) -> bool:
    result = subprocess.run(
        ["git", "-C", str(root), "rev-parse", "--verify", "--quiet", f"{source_ref}^{{commit}}"],
        text=True, capture_output=True, check=False,
    )
    return result.returncode == 0


def git_tag_versions(root: Path) -> list[str]:
    result = subprocess.run(["git", "-C", str(root), "tag", "-l"], text=True, capture_output=True, check=False)
    if result.returncode:
        return []
    versions: list[str] = []
    for tag in result.stdout.splitlines():
        match = TAG_VERSION_PATTERN.match(tag.strip())
        if match:
            versions.append(match.group("version"))
    return sorted(set(versions), key=lambda release: tuple(int(part) for part in release.split(".")), reverse=True)


def changelog_release_versions(root: Path) -> list[str]:
    changelog = root / "CHANGELOG.md"
    if not changelog.is_file():
        return []
    content = changelog.read_text(encoding="utf-8", errors="replace")
    return [heading for heading in CHANGELOG_HEADING.findall(content)]


def release_errors(root: Path, catalogue: Catalogue, mode: str) -> list[str]:
    errors: list[str] = []
    current = catalogue.current
    current_entry = catalogue.release(current)
    if current_entry is None:
        return errors
    expected_ref = tag_name_for(current)
    if current_entry["source_ref"] != expected_ref:
        errors.append(f"current release source_ref must be {expected_ref!r}, not {current_entry['source_ref']!r}")

    if not git_present(root):
        return errors + ["cannot verify release source refs without a git repository"]

    tag_versions = set(git_tag_versions(root))
    declared = {entry["release"] for entry in catalogue.versions}

    missing_published = sorted(tag_versions - declared, key=lambda release: tuple(int(part) for part in release.split(".")))
    if missing_published:
        errors.append("catalogue is missing published historical release(s): " + ", ".join(missing_published))

    changelog_versions = set(changelog_release_versions(root))
    missing_changelog = sorted(
        (tag_versions - {current} - changelog_versions),
        key=lambda release: tuple(int(part) for part in release.split(".")),
    )
    if missing_changelog:
        errors.append("tagged release(s) lack a CHANGELOG entry: " + ", ".join(missing_changelog))

    if mode == "released":
        for entry in catalogue.versions:
            if not git_ref_resolves(root, entry["source_ref"]):
                errors.append(f"release {entry['release']} source_ref {entry['source_ref']!r} does not resolve")
        if not git_ref_resolves(root, expected_ref):
            errors.append(f"current source_ref {expected_ref!r} does not resolve in released mode")
    else:
        for entry in catalogue.versions:
            if entry["release"] == current:
                continue
            if not git_ref_resolves(root, entry["source_ref"]):
                errors.append(f"historical release {entry['release']} source_ref {entry['source_ref']!r} does not resolve")
    return errors


def check_release(root: Path, mode: str, expected_version: str | None = None) -> list[str]:
    root = root.resolve()
    versions_path = root / "docs" / "versions.json"
    try:
        catalogue = load_catalogue(versions_path)
    except ValueError as exc:
        return [str(exc)]
    errors: list[str] = []
    if expected_version and expected_version != catalogue.current:
        errors.append(f"--expected-version {expected_version} does not match catalogue current {catalogue.current}")
    errors.extend(catalogue_errors(catalogue))
    errors.extend(metadata_errors(root, catalogue.current))
    errors.extend(release_errors(root, catalogue, mode))
    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1])
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--pre-tag", action="store_true", help="validate before the current release tag exists")
    group.add_argument("--released", action="store_true", help="validate after the current release tag exists")
    parser.add_argument("--expected-version", help="require this catalogue current version")
    args = parser.parse_args()
    mode = "released" if args.released else "pre-tag"
    errors = check_release(args.root, mode, args.expected_version)
    if errors:
        print("\n".join(errors))
        return 1
    print(f"Release metadata checks passed ({mode})")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())