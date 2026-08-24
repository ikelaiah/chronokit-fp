#!/usr/bin/env python3
"""Regression tests for generated ChronoKit-FP documentation validation."""

from __future__ import annotations

import json
import re
import sys
import tempfile
import unittest
from pathlib import Path


TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))

from build_docs import build_site  # noqa: E402
from check_built_docs import check_site  # noqa: E402


class CheckBuiltDocsTests(unittest.TestCase):
    def build_fixture(self, root: Path) -> Path:
        source = root / "docs"
        output = root / "site" / "1.9.1"
        source.mkdir()
        (source / "index.md").write_text("# Index\n\n[Guide](guide.md)\n", encoding="utf-8")
        (source / "guide.md").write_text("# Guide\n\nAll good.\n", encoding="utf-8")
        (source / "layout.json").write_text(json.dumps({"schema_version": 1, "release": "1.9.1"}), encoding="utf-8")
        versions = source / "versions.json"
        versions.write_text(
            json.dumps(
                {
                    "schema_version": 1,
                    "current": "1.9.1",
                    "site_url": "https://example.invalid/chronokit-fp",
                    "repository_url": "https://github.com/example/chronokit-fp",
                    "versions": [{"release": "1.9.1", "source_ref": "main"}],
                }
            ),
            encoding="utf-8",
        )
        build_site(source, output, output.parent, versions)
        return output.parent

    def test_accepts_a_complete_versioned_site(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            self.assertEqual([], check_site(self.build_fixture(Path(directory))))

    def test_reports_a_missing_generated_link_target(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_fixture(Path(directory))
            page = site / "1.9.1" / "index.html"
            page.write_text(page.read_text(encoding="utf-8").replace('guide.html', 'missing.html'), encoding="utf-8")
            self.assertTrue(any("missing link target" in error for error in check_site(site)))

    def test_reports_duplicate_ids_and_unsafe_links(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_fixture(Path(directory))
            page = site / "1.9.1" / "guide.html"
            page.write_text(
                page.read_text(encoding="utf-8").replace(
                    "</main>",
                    '<p id="guide">Duplicate identifier</p><a href="javascript:alert(1)">Unsafe</a></main>',
                ),
                encoding="utf-8",
            )
            errors = check_site(site)
            self.assertTrue(any("duplicate id" in error for error in errors))
            self.assertTrue(any("unsafe link" in error for error in errors))

    def test_requires_the_documentation_assets_and_version_targets(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_fixture(Path(directory))
            (site / "1.9.1" / "assets" / "site.js").unlink()
            errors = check_site(site)
            self.assertTrue(any("missing required asset" in error for error in errors))


class SelectorTests(unittest.TestCase):
    def build_two_version_site(self, root: Path) -> Path:
        source = root / "docs"
        source.mkdir()
        (source / "index.md").write_text("# Index\n\n[Guide](guide.md)\n", encoding="utf-8")
        (source / "guide.md").write_text("# Guide\n\nAll good.\n", encoding="utf-8")
        (source / "layout.json").write_text(json.dumps({"schema_version": 1, "release": "1.9.1"}), encoding="utf-8")
        versions = source / "versions.json"
        versions.write_text(
            json.dumps(
                {
                    "schema_version": 1,
                    "current": "1.9.1",
                    "site_url": "https://example.invalid/chronokit-fp",
                    "repository_url": "https://github.com/example/chronokit-fp",
                    "versions": [
                        {"release": "1.9.1", "source_ref": "v1.9.1"},
                        {"release": "1.9.0", "source_ref": "v1.9.0"},
                    ],
                }
            ),
            encoding="utf-8",
        )
        build_site(source, root / "site" / "1.9.1", root / "site", versions)
        (source / "layout.json").write_text(json.dumps({"schema_version": 1, "release": "1.9.0"}), encoding="utf-8")
        build_site(source, root / "site" / "1.9.0", root / "site", versions, release="1.9.0")
        return root / "site"

    def test_every_selector_lists_every_version_and_marks_only_current(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_two_version_site(Path(directory))
            self.assertEqual([], check_site(site))

    def test_historical_selector_is_selecting_itself_not_current(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_two_version_site(Path(directory))
            for page in (site / "1.9.0").rglob("*.html"):
                content = page.read_text(encoding="utf-8")
                self.assertIn('selected>v1.9.0</option>', content)
                self.assertIn('(current)</option>', content)
                self.assertNotIn('selected>v1.9.1</option>', content)

    def test_dropped_version_from_selector_is_reported(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_two_version_site(Path(directory))
            page = site / "1.9.1" / "index.html"
            content = page.read_text(encoding="utf-8")
            content = re.sub(r"<option[^>]*>v1\.9\.0</option>", "", content)
            page.write_text(content, encoding="utf-8")
            errors = check_site(site)
            self.assertTrue(any("version selector does not match the catalogue" in error for error in errors))
            self.assertTrue(any("missing: 1.9.0" in error for error in errors))

    def test_duplicate_current_label_is_reported(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_two_version_site(Path(directory))
            page = site / "1.9.1" / "index.html"
            page.write_text(page.read_text(encoding="utf-8").replace("v1.9.0</option>", "v1.9.0 (current)</option>"), encoding="utf-8")
            errors = check_site(site)
            self.assertTrue(any("non-current release 1.9.0 is labelled (current)" in error for error in errors))
            self.assertTrue(any("expected exactly one (current) version" in error for error in errors))

    def test_unselected_viewed_version_is_reported(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_two_version_site(Path(directory))
            page = site / "1.9.0" / "index.html"
            content = page.read_text(encoding="utf-8")
            page.write_text(content.replace(' selected>v1.9.0<', '>v1.9.0<'), encoding="utf-8")
            errors = check_site(site)
            self.assertTrue(any("viewed release 1.9.0 is not selected" in error for error in errors))

    def test_pages_keep_theme_search_navigation_and_version_assets(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_two_version_site(Path(directory))
            for release in ("1.9.1", "1.9.0"):
                page = site / release / "index.html"
                content = page.read_text(encoding="utf-8")
                self.assertIn('id="theme-toggle"', content, release)
                self.assertIn('id="search"', content, release)
                self.assertIn('class="docs-navigation"', content, release)
                self.assertIn('id="version-select"', content, release)
                self.assertTrue((site / release / "search-index.js").is_file(), release)
                self.assertTrue((site / release / "assets" / "site.js").is_file(), release)

    def test_version_switch_preserves_the_page_and_falls_back_safely(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_two_version_site(Path(directory))
            java_script = (site / "1.9.1" / "assets" / "site.js").read_text(encoding="utf-8")
            self.assertIn('method: "HEAD"', java_script)
            self.assertIn('response.ok ? target : targetIndex', java_script)
            self.assertIn('.catch(() => { window.location.assign(targetIndex); })', java_script)


class NavigationContractTests(unittest.TestCase):
    def simple_versions(self) -> dict[str, object]:
        return {
            "schema_version": 1,
            "current": "1.9.1",
            "site_url": "https://example.invalid/chronokit-fp",
            "repository_url": "https://github.com/example/chronokit-fp",
            "versions": [{"release": "1.9.1", "source_ref": "v1.9.1"}],
        }

    def write_versions(self, source: Path) -> None:
        (source / "versions.json").write_text(json.dumps(self.simple_versions()), encoding="utf-8")

    def build_legacy_site(self, root: Path, history_pages: list[str]) -> Path:
        source = root / "docs"
        source.mkdir()
        (source / "index.md").write_text("# Index\n\n[Guide](guide.md)\n", encoding="utf-8")
        (source / "guide.md").write_text("# Guide\n\nAll good.\n", encoding="utf-8")
        for page in history_pages:
            target = source / page
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(f"# {page}\n\nInternal record.\n", encoding="utf-8")
        (source / "layout.json").write_text(json.dumps({"schema_version": 1, "release": "1.9.1"}), encoding="utf-8")
        self.write_versions(source)
        build_site(source, root / "site" / "1.9.1", root / "site", source / "versions.json")
        return root / "site"

    def test_sidebar_dump_with_release_notes_or_pr_summaries_is_reported(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_legacy_site(Path(directory), ["RELEASE-NOTES-v1.9.1.md", "PR-v1.9.1.md"])
            errors = check_site(site)
            self.assertTrue(any("internal/history page appears in the sidebar" in error for error in errors))
            self.assertTrue(any("RELEASE-NOTES-v1.9.1.html" in error for error in errors))
            self.assertTrue(any("PR-v1.9.1.html" in error for error in errors))

    def test_sidebar_dump_with_audits_and_decisions_is_reported(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_legacy_site(
                Path(directory),
                ["API-Audit-v1.5.0.md", "V2-DECISION.md", "decisions/0001-domain-internals.md"],
            )
            errors = check_site(site)
            self.assertTrue(any("internal/history page appears in the sidebar" in error for error in errors))

    def test_curated_sidebar_without_internal_pages_is_accepted(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_legacy_site(Path(directory), [])
            self.assertEqual([], check_site(site))

    def test_sidebar_link_escaping_its_version_is_reported(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "docs"
            source.mkdir()
            (source / "index.md").write_text("# Index\n\n[Guide](guide.md)\n", encoding="utf-8")
            (source / "guide.md").write_text("# Guide\n\nAll good.\n", encoding="utf-8")
            versions_path = source / "versions.json"
            versions_path.write_text(
                json.dumps(
                    {
                        "schema_version": 1,
                        "current": "1.9.1",
                        "site_url": "https://example.invalid/chronokit-fp",
                        "repository_url": "https://github.com/example/chronokit-fp",
                        "versions": [
                            {"release": "1.9.1", "source_ref": "v1.9.1"},
                            {"release": "1.9.0", "source_ref": "v1.9.0"},
                        ],
                    }
                ),
                encoding="utf-8",
            )
            (source / "layout.json").write_text(json.dumps({"schema_version": 1, "release": "1.9.1"}), encoding="utf-8")
            build_site(source, root / "site" / "1.9.1", root / "site", versions_path)
            (source / "layout.json").write_text(json.dumps({"schema_version": 1, "release": "1.9.0"}), encoding="utf-8")
            build_site(source, root / "site" / "1.9.0", root / "site", versions_path, release="1.9.0")

            page = root / "site" / "1.9.1" / "index.html"
            page.write_text(
                page.read_text(encoding="utf-8").replace(
                    'class="nav-link" href="guide.html"',
                    'class="nav-link" href="../1.9.0/index.html"',
                ),
                encoding="utf-8",
            )
            errors = check_site(root / "site")
            self.assertTrue(any("sidebar link escapes its own version directory" in error for error in errors))

    def test_missing_theme_or_search_on_a_selector_page_is_reported(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_legacy_site(Path(directory), [])
            page = site / "1.9.1" / "index.html"
            page.write_text(page.read_text(encoding="utf-8").replace(' id="search"', ""), encoding="utf-8")
            errors = check_site(site)
            self.assertTrue(any("missing UI elements" in error for error in errors))
            self.assertTrue(any("search" in error for error in errors))


if __name__ == "__main__":
    unittest.main()
