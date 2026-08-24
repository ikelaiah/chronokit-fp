#!/usr/bin/env python3
"""Regression tests for release metadata and the docs/versions.json catalogue."""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


TOOLS = Path(__file__).resolve().parent
ROOT = TOOLS.parent
sys.path.insert(0, str(TOOLS))

from check_release import (
    Catalogue,
    catalogue_errors,
    check_release,
    load_catalogue,
    metadata_errors,
    release_errors,
    tag_name_for,
)  # noqa: E402

RELEASED = ["1.7.0", "1.6.0", "1.5.0", "1.4.0", "1.3.0", "1.2.0", "1.1.0"]
HISTORICAL = RELEASED[1:]


def git(root: Path, *arguments: str) -> str:
    result = subprocess.run(["git", "-C", str(root), *arguments], text=True, capture_output=True, check=False)
    if result.returncode:
        raise RuntimeError(f"git {' '.join(arguments)} failed:\n{result.stdout}{result.stderr}")
    return result.stdout


class CatalogueContractTests(unittest.TestCase):
    def make_catalogue(self, current: str = "1.7.0", historical: list[str] | None = None) -> Catalogue:
        entries = [{"release": current, "source_ref": tag_name_for(current)}]
        for release in historical or HISTORICAL:
            entries.append({"release": release, "source_ref": tag_name_for(release)})
        return Catalogue(current, entries)

    def test_accepts_current_catalogue(self) -> None:
        self.assertEqual([], catalogue_errors(self.make_catalogue()))

    def test_rejects_main_as_a_published_source_ref(self) -> None:
        catalogue = self.make_catalogue()
        catalogue.versions[0]["source_ref"] = "main"
        self.assertTrue(any("moving source_ref" in error for error in catalogue_errors(catalogue)))

    def test_rejects_an_empty_source_ref(self) -> None:
        catalogue = self.make_catalogue()
        catalogue.versions[-1]["source_ref"] = ""
        self.assertTrue(any("empty source_ref" in error for error in catalogue_errors(catalogue)))

    def test_rejects_duplicate_version_entries(self) -> None:
        catalogue = self.make_catalogue()
        catalogue.versions.append(dict(catalogue.versions[0]))
        self.assertTrue(any("duplicate release entry" in error for error in catalogue_errors(catalogue)))

    def test_requires_current_as_first_entry(self) -> None:
        catalogue = self.make_catalogue()
        catalogue.versions[0], catalogue.versions[1] = catalogue.versions[1], catalogue.versions[0]
        self.assertTrue(any("must be the first entry" in error for error in catalogue_errors(catalogue)))

    def test_rejects_out_of_order_versions(self) -> None:
        catalogue = self.make_catalogue()
        catalogue.versions[1]["release"] = "1.0.0"
        catalogue.versions[1]["source_ref"] = "v1.0.0"
        self.assertTrue(any("newest-to-oldest" in error for error in catalogue_errors(catalogue)))

    def test_requires_semantic_release_names(self) -> None:
        catalogue = self.make_catalogue()
        catalogue.versions[0]["release"] = "banana"
        self.assertTrue(any("not a semantic version" in error for error in catalogue_errors(catalogue)))

    def test_rejects_source_ref_naming_a_different_version(self) -> None:
        catalogue = self.make_catalogue(historical=["1.5.0"])
        catalogue.versions[1]["source_ref"] = "v1.6.0"
        self.assertTrue(any("names a different version" in error for error in catalogue_errors(catalogue)))

    def test_requires_current_source_ref_to_be_its_tag(self) -> None:
        catalogue = self.make_catalogue(historical=[])
        catalogue.versions[0]["source_ref"] = "v9.9.9"
        self.assertTrue(any("current release source_ref must be" in error for error in catalogue_errors(catalogue)))


class MetadataConsistencyTests(unittest.TestCase):
    def write_repo(self, root: Path, version: str) -> None:
        docs = root / "docs"
        docs.mkdir(parents=True)
        api = root / "api"
        api.mkdir()
        (root / "packages" / "lazarus").mkdir(parents=True)
        (root / "packages" / "lazarus" / "chronokit_fp.lpk").write_text(
            f'<CONFIG><Package Version="5"><Version Major="1" Minor="7"/></Package></CONFIG>',
            encoding="utf-8",
        )
        (root / "src").mkdir()
        (root / "src" / "ChronoKit.pas").write_text(f"@version {version}\n", encoding="utf-8")
        (root / "README.md").write_text(f"![Version](https://img.shields.io/badge/version-{version}-8B5CF6.svg)\n", encoding="utf-8")
        (root / "CHANGELOG.md").write_text(f"## [{version}] - 2026-01-01\n", encoding="utf-8")
        (docs / "layout.json").write_text(json.dumps({"schema_version": 2, "release": version}), encoding="utf-8")
        (docs / "versions.json").write_text(json.dumps({"schema_version": 1, "current": version, "versions": [{"release": version, "source_ref": f"v{version}"}]}), encoding="utf-8")
        (docs / f"RELEASE-NOTES-v{version}.md").write_text(f"# Release notes for {version}\n", encoding="utf-8")
        (docs / f"PR-v{version}.md").write_text(f"# PR summary for {version}\n", encoding="utf-8")
        (docs / "Cheat-Sheet.md").write_text(f"This index includes the preferred v{version.split('.')[0]}.{version.split('.')[1]} methods.\n", encoding="utf-8")
        (api / f"ChronoKit-v{version}-windows.txt").write_text("platform=windows\n", encoding="utf-8")

    def test_accepts_consistent_version_metadata(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.write_repo(root, "1.7.0")
            self.assertEqual([], metadata_errors(root, "1.7.0"))

    def test_reports_drifted_readme_badge(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.write_repo(root, "1.7.0")
            (root / "README.md").write_text("![Version](https://img.shields.io/badge/version-1.6.0-8B5CF6.svg)\n", encoding="utf-8")
            self.assertTrue(any("README version badge declares 1.6.0" in error for error in metadata_errors(root, "1.7.0")))

    def test_reports_stale_changelog_entry(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.write_repo(root, "1.7.0")
            (root / "CHANGELOG.md").write_text("## [1.6.0] - 2026-01-01\n", encoding="utf-8")
            self.assertTrue(any("CHANGELOG current entry is 1.6.0" in error for error in metadata_errors(root, "1.7.0")))

    def test_reports_missing_release_notes(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.write_repo(root, "1.7.0")
            (root / "docs" / "RELEASE-NOTES-v1.7.0.md").unlink()
            self.assertTrue(any("release notes document is missing" in error for error in metadata_errors(root, "1.7.0")))

    def test_current_metadata_is_consistent_for_the_published_version(self) -> None:
        catalogue = load_catalogue(ROOT / "docs" / "versions.json")
        self.assertEqual([], metadata_errors(ROOT, catalogue.current))


class CatalogueLoadTests(unittest.TestCase):
    def test_rejects_an_unsupported_schema_version(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "versions.json"
            path.write_text(json.dumps({"schema_version": 99, "current": "1.0.0", "versions": [{"release": "1.0.0", "source_ref": "v1.0.0"}]}), encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "unsupported schema_version"):
                load_catalogue(path)

    def test_rejects_a_non_array_versions_entry(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "versions.json"
            path.write_text(json.dumps({"schema_version": 1, "current": "1.0.0", "versions": "nope"}), encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "versions must be an array"):
                load_catalogue(path)

    def test_v1_7_0_uses_its_immutable_tag_not_main(self) -> None:
        catalogue = load_catalogue(ROOT / "docs" / "versions.json")
        self.assertEqual(
            {"release": "1.7.0", "source_ref": "v1.7.0"},
            catalogue.versions[0],
        )


class HistoricalReleaseFixture:
    """A tiny git repository with tags for released versions."""

    def __init__(self, root: Path) -> None:
        self.root = root
        git(root, "init", "-b", "main")
        git(root, "config", "user.email", "test@example.com")
        git(root, "config", "user.name", "Test")

    def commit(self, path: Path, content: str, clean: bool = True) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    def snapshot(self, message: str = "fixture") -> None:
        git(self.root, "add", ".")
        git(self.root, "commit", "--no-gpg-sign", "-m", message)

    def tag(self, name: str) -> None:
        git(self.root, "tag", name)

    def write_release_files(self, current: str, versions: list[dict[str, str]], extra_docs: str = "") -> None:
        changelog = "\n".join(f"## [{entry['release']}]" for entry in versions)
        self.root.joinpath("CHANGELOG.md").write_text(changelog + "\n", encoding="utf-8")
        docs = self.root / "docs"
        docs.mkdir(parents=True, exist_ok=True)
        versions_path = docs / "versions.json"
        versions_path.write_text(json.dumps({"schema_version": 1, "current": current, "versions": versions}), encoding="utf-8")
        (docs / "index.md").write_text("# Documentation\n\n" + extra_docs, encoding="utf-8")


class PreTagReleasedTests(unittest.TestCase):
    @staticmethod
    def released_versions() -> list[dict[str, str]]:
        return [{"release": release, "source_ref": tag_name_for(release)} for release in RELEASED]

    def build_snapshot(self, directory: str, current: str = "1.8.0") -> HistoricalReleaseFixture:
        fixture = HistoricalReleaseFixture(Path(directory))
        versions = [{"release": current, "source_ref": tag_name_for(current)}, *self.released_versions()]
        fixture.write_release_files(current, versions)
        fixture.snapshot()
        # Only historical releases are tagged; the current release stays planned.
        for release in RELEASED:
            fixture.tag(tag_name_for(release))
        return fixture

    @staticmethod
    def catalogue(fixture: HistoricalReleaseFixture, current: str, versions: list[dict[str, str]]) -> Catalogue:
        return Catalogue(current, versions)

    def test_pre_tag_accepts_a_planned_future_release(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = self.build_snapshot(directory)
            versions = [{"release": "1.8.0", "source_ref": "v1.8.0"}, *self.released_versions()]
            catalogue = self.catalogue(fixture, "1.8.0", versions)
            self.assertEqual([], release_errors(fixture.root, catalogue, "pre-tag"))

    def test_released_mode_rejects_a_not_yet_created_tag(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = self.build_snapshot(directory)
            versions = [{"release": "1.8.0", "source_ref": "v1.8.0"}, *self.released_versions()]
            catalogue = self.catalogue(fixture, "1.8.0", versions)
            errors = release_errors(fixture.root, catalogue, "released")
            self.assertTrue(any("v1.8.0' does not resolve" in error for error in errors))

    def test_released_mode_accepts_a_fully_tagged_catalogue(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = HistoricalReleaseFixture(Path(directory))
            versions = self.released_versions()
            fixture.write_release_files("1.7.0", versions)
            fixture.snapshot()
            for release in RELEASED:
                fixture.tag(tag_name_for(release))
            catalogue = self.catalogue(fixture, "1.7.0", versions)
            self.assertEqual([], release_errors(fixture.root, catalogue, "released"))

    def test_missing_historical_release_fails_validation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = HistoricalReleaseFixture(Path(directory))
            versions = [{"release": "1.8.0", "source_ref": "v1.8.0"}]
            fixture.write_release_files("1.8.0", versions)
            fixture.snapshot()
            for release in RELEASED:
                fixture.tag(tag_name_for(release))
            catalogue = self.catalogue(fixture, "1.8.0", versions)
            for mode in ("pre-tag", "released"):
                errors = release_errors(fixture.root, catalogue, mode)
                self.assertTrue(any("missing published historical release" in error for error in errors), mode)

    def test_unresolved_historical_ref_fails_validation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = HistoricalReleaseFixture(Path(directory))
            versions = [
                {"release": "1.8.0", "source_ref": "v1.8.0"},
                {"release": "1.7.0", "source_ref": "v1.7.0"},
                {"release": "1.6.0", "source_ref": "v9.9.9"},
            ]
            fixture.write_release_files("1.8.0", versions)
            fixture.snapshot()
            fixture.tag("v1.7.0")
            catalogue = self.catalogue(fixture, "1.8.0", versions)
            for mode in ("pre-tag", "released"):
                errors = release_errors(fixture.root, catalogue, mode)
                self.assertTrue(any("source_ref 'v9.9.9' does not resolve" in error for error in errors), mode)

    def test_historical_truth_cross_checks_changelog(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = HistoricalReleaseFixture(Path(directory))
            versions = self.released_versions()
            fixture.write_release_files("1.7.0", versions)
            fixture.snapshot()
            for release in RELEASED:
                fixture.tag(tag_name_for(release))
            # Removing a changelog heading for a tagged release is detected.
            changelog = fixture.root / "CHANGELOG.md"
            changelog.write_text(changelog.read_text(encoding="utf-8").replace("## [1.1.0]", "## [0.9.9-dropped]"), encoding="utf-8")
            catalogue = self.catalogue(fixture, "1.7.0", versions)
            errors = release_errors(fixture.root, catalogue, "released")
            self.assertTrue(any("lack a CHANGELOG entry" in error for error in errors))

    def test_future_simulation_preserves_historical_entries(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            fixture = self.build_snapshot(directory)
            future = [
                {"release": "1.8.0", "source_ref": "v1.8.0"},
                {"release": "1.7.0", "source_ref": "v1.7.0"},
                {"release": "1.6.0", "source_ref": "v.1.6.0"},
            ]
            catalogue = self.catalogue(fixture, "1.8.0", future)
            errors = release_errors(fixture.root, catalogue, "pre-tag")
            self.assertTrue(any("missing published historical release" in error for error in errors))


class CheckReleaseIntegrationTests(unittest.TestCase):
    def test_real_repository_passes_pre_tag_and_released_for_current(self) -> None:
        self.assertEqual([], check_release(ROOT, "pre-tag"))
        self.assertEqual([], check_release(ROOT, "released"))

    def test_expected_version_mismatch_is_reported(self) -> None:
        errors = check_release(ROOT, "pre-tag", expected_version="9.9.9")
        self.assertTrue(any("--expected-version 9.9.9" in error for error in errors))

    def test_current_catalogue_is_version_1_7_0(self) -> None:
        catalogue = load_catalogue(ROOT / "docs" / "versions.json")
        self.assertEqual("1.7.0", catalogue.current)


if __name__ == "__main__":
    unittest.main()