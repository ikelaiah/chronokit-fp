#!/usr/bin/env python3
"""Regression tests for building all declared documentation releases."""

from __future__ import annotations

import json
import re
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


TOOLS = Path(__file__).resolve().parent
ROOT = TOOLS.parent
sys.path.insert(0, str(TOOLS))

from build_all_docs import build_all, RELEASED_MODE  # noqa: E402


def git(root: Path, *arguments: str) -> None:
    result = subprocess.run(["git", "-C", str(root), *arguments], text=True, capture_output=True, check=False)
    if result.returncode:
        raise RuntimeError(f"git {' '.join(arguments)} failed:\n{result.stdout}{result.stderr}")


def write_fixture(root: Path, current: str, versions: list[dict[str, str]]) -> None:
    source = root / "docs"
    source.mkdir(parents=True, exist_ok=True)
    (source / "index.md").write_text(f"# Documentation for {current}\n", encoding="utf-8")
    (source / "layout.json").write_text(json.dumps({"schema_version": 1, "release": current}), encoding="utf-8")
    (source / "versions.json").write_text(
        json.dumps(
            {
                "schema_version": 1,
                "current": current,
                "site_url": "https://example.invalid/chronokit-fp",
                "repository_url": "https://github.com/example/chronokit-fp",
                "versions": versions,
            }
        ),
        encoding="utf-8",
    )


class BuildAllDocsTests(unittest.TestCase):
    def test_builds_the_current_release_without_a_git_worktree(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "docs"
            source.mkdir()
            (source / "index.md").write_text("# Documentation\n", encoding="utf-8")
            (source / "layout.json").write_text(json.dumps({"schema_version": 1, "release": "1.9.1"}), encoding="utf-8")
            (source / "versions.json").write_text(
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

            count = build_all(root, root / "site", root / "artifacts")

            self.assertEqual(1, count)
            self.assertTrue((root / "site" / "1.9.1" / "index.html").is_file())
            self.assertTrue((root / "artifacts" / "chronokit-fp-docs-1.9.1.zip").is_file())

    def test_released_mode_builds_the_current_release_from_its_tag_not_the_checkout(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            versions = [{"release": "1.9.1", "source_ref": "v1.9.1"}]
            write_fixture(root, "1.9.1", versions)
            git(root, "init", "-b", "main")
            git(root, "config", "user.email", "test@example.com")
            git(root, "config", "user.name", "Test")
            (root / "docs" / "index.md").write_text("# Tagged content for 1.9.1\n", encoding="utf-8")
            git(root, "add", ".")
            git(root, "commit", "--no-gpg-sign", "-m", "release fixture")
            git(root, "tag", "v1.9.1")
            (root / "docs" / "index.md").write_text("# Uncommitted checkout content\n", encoding="utf-8")

            build_all(root, root / "site", mode=RELEASED_MODE)

            released_page = (root / "site" / "1.9.1" / "index.html").read_text(encoding="utf-8")
            self.assertIn("Tagged content for 1.9.1", released_page)
            self.assertNotIn("Uncommitted checkout content", released_page)

    def test_released_mode_rejects_a_moving_source_ref(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            versions = [{"release": "1.9.1", "source_ref": "main"}]
            write_fixture(root, "1.9.1", versions)

            with self.assertRaisesRegex(ValueError, "moving source_ref"):
                build_all(root, root / "site", mode=RELEASED_MODE)

    def test_development_mode_builds_the_current_release_from_the_checkout(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            versions = [{"release": "1.9.1", "source_ref": "main"}, {"release": "1.9.0", "source_ref": "v1.9.0"}]
            write_fixture(root, "1.9.1", versions)
            git(root, "init", "-b", "main")
            git(root, "config", "user.email", "test@example.com")
            git(root, "config", "user.name", "Test")
            (root / "docs" / "layout.json").write_text(json.dumps({"schema_version": 1, "release": "1.9.0"}), encoding="utf-8")
            git(root, "add", ".")
            git(root, "commit", "--no-gpg-sign", "-m", "site fixture")
            git(root, "tag", "v1.9.0")
            (root / "docs" / "layout.json").write_text(json.dumps({"schema_version": 1, "release": "1.9.1"}), encoding="utf-8")
            (root / "docs" / "index.md").write_text("# Current checkout content\n", encoding="utf-8")

            build_all(root, root / "site")

            current_page = (root / "site" / "1.9.1" / "index.html").read_text(encoding="utf-8")
            historical_page = (root / "site" / "1.9.0" / "index.html").read_text(encoding="utf-8")
            self.assertIn("Current checkout content", current_page)
            self.assertIn("Documentation for 1.9.1", historical_page)


class RealRepositoryBuildTests(unittest.TestCase):
    def test_released_build_creates_every_declared_release_from_its_tag(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site_root = Path(directory) / "site"
            build_all(ROOT, site_root, mode=RELEASED_MODE)
            versions = json.loads((ROOT / "docs" / "versions.json").read_text(encoding="utf-8"))
            release_names = {entry["release"] for entry in versions["versions"]}
            built_dirs = {path.name for path in site_root.iterdir() if path.is_dir() and path.name in release_names}
            self.assertEqual(release_names, built_dirs)
            for entry in versions["versions"]:
                release = entry["release"]
                directory = site_root / release
                self.assertTrue((directory / "index.html").is_file(), release)
                identity = json.loads((directory / "release.json").read_text(encoding="utf-8"))
                self.assertEqual(entry["source_ref"], identity["source_ref"], release)


class HistoricalNavigationTests(unittest.TestCase):
    """Curated sidebar regression coverage against the real released site."""

    @classmethod
    def setUpClass(cls) -> None:
        cls._temporary = tempfile.TemporaryDirectory()
        cls.site = Path(cls._temporary.name) / "site"
        build_all(ROOT, cls.site, mode=RELEASED_MODE)

    @classmethod
    def tearDownClass(cls) -> None:
        cls._temporary.cleanup()

    def page(self, version: str) -> str:
        return (self.site / version / "Getting-Started.html").read_text(encoding="utf-8")

    def nav_hrefs(self, version: str) -> set[str]:
        version_dir = self.site / version
        page = version_dir / "Getting-Started.html"
        content = page.read_text(encoding="utf-8")
        hrefs: set[str] = set()
        for href in re.findall(r'class="nav-link[^"]*"[^>]*href="([^"]+\.html)"', content):
            try:
                target = (page.parent / href).resolve()
                hrefs.add(target.relative_to(version_dir).as_posix())
            except ValueError:
                continue
        return hrefs

    def nav_sections(self, version: str) -> list[str]:
        content = self.page(version)
        sections = re.findall(r'<section class="sidebar-section"><h2>([^<]+)</h2>', content)
        seen: set[str] = set()
        unique: list[str] = []
        for section in sections:
            if section not in seen:
                seen.add(section)
                unique.append(section)
        return unique

    def selector_labels(self, version: str) -> list[str]:
        content = self.page(version)
        return [match.group(1) for match in re.finditer(r"<option[^>]*>([^<]+)</option>", content)]

    def test_v7_sidebar_exposes_the_user_facing_sections(self) -> None:
        self.assertEqual(["Getting Started", "Guides", "Reference"], self.nav_sections("1.7.0"))
        expected = {
            "Getting-Started.html",
            "Learning-Path.html",
            "Decision-Guides.html",
            "Cheat-Sheet.html",
            "ChronoKit-FP.html",
            "Business-Calendars.html",
            "Troubleshooting.html",
            "API-Reference.html",
            "Business-Calendar-API.html",
            "Timezone-Contract.html",
            "MIGRATION-v1.6-to-v2.0.html",
        }
        self.assertEqual(expected, self.nav_hrefs("1.7.0"))

    def test_v7_sidebar_excludes_internal_and_history_documents(self) -> None:
        hrefs = self.nav_hrefs("1.7.0")
        for internal in (
            "PR-v1.7.0.html",
            "RELEASE-NOTES-v1.7.0.html",
            "API-Audit-v1.5.0.html",
            "API-Audit-v1.7.0.html",
            "API-Additions-v1.7.0.html",
            "API-Deprecations-v1.6.0.html",
            "V2-DECISION.html",
            "decisions/0001-domain-internals.html",
        ):
            self.assertNotIn(internal, hrefs)

    def test_internal_pages_still_build_for_links(self) -> None:
        for internal in (
            "PR-v1.7.0.html",
            "RELEASE-NOTES-v1.7.0.html",
            "API-Audit-v1.5.0.html",
            "V2-DECISION.html",
            "decisions/0001-domain-internals.html",
        ):
            self.assertTrue((self.site / "1.7.0" / internal).is_file(), internal)

    def test_six_sidebar_is_sensible(self) -> None:
        hrefs = self.nav_hrefs("1.6.0")
        self.assertNotIn("Learning-Path.html", hrefs)
        self.assertIn("MIGRATION-v1.6-to-v2.0.html", hrefs)
        self.assertNotIn("PR-v1.6.0.html", hrefs)
        self.assertNotIn("RELEASE-NOTES-v1.6.0.html", hrefs)

    def test_one_sidebar_is_sensible(self) -> None:
        hrefs = self.nav_hrefs("1.1.0")
        self.assertIn("Getting-Started.html", hrefs)
        self.assertIn("Cheat-Sheet.html", hrefs)
        self.assertIn("ChronoKit-FP.html", hrefs)
        self.assertNotIn("PR-v1.1.0.html", hrefs)
        self.assertNotIn("RELEASE-NOTES-v1.1.0.html", hrefs)

    def test_every_historical_sidebar_matches_the_curated_policy(self) -> None:
        policy = json.loads((ROOT / "docs" / "version-navigation-policy.json").read_text(encoding="utf-8"))
        policy_pages = {
            item["path"].rsplit(".", 1)[0] + ".html"
            for section in policy["sections"]
            for item in section["pages"]
        }
        versions = json.loads((ROOT / "docs" / "versions.json").read_text(encoding="utf-8"))["versions"]
        for entry in versions:
            version = entry["release"]
            version_dir = self.site / version
            built = {path.relative_to(version_dir).as_posix() for path in version_dir.rglob("*.html")}
            expected = {page for page in policy_pages if page in built}
            self.assertEqual(expected, self.nav_hrefs(version), version)

    def test_site_navigation_policy_is_well_formed(self) -> None:
        policy = json.loads((ROOT / "docs" / "version-navigation-policy.json").read_text(encoding="utf-8"))
        self.assertEqual(1, policy.get("schema_version"))
        self.assertTrue(policy.get("sections"))
        for section in policy["sections"]:
            self.assertTrue(section.get("title"))
            for item in section["pages"]:
                path = item["path"]
                self.assertTrue(path.endswith(".md"), path)
                self.assertNotIn("..", path)
                self.assertFalse(path.startswith("/"), path)
                self.assertTrue(item.get("title"))

    def test_all_seven_releases_remain_in_every_selector(self) -> None:
        for version in ("1.7.0", "1.6.0", "1.4.0", "1.1.0"):
            labels = self.selector_labels(version)
            self.assertEqual(len(labels), 7, version)
            self.assertEqual(
                ["v1.7.0", "v1.6.0", "v1.5.0", "v1.4.0", "v1.3.0", "v1.2.0", "v1.1.0"],
                [label.split(" (current)")[0] for label in labels],
                version,
            )

    def test_only_the_current_release_is_marked_current(self) -> None:
        for version in ("1.7.0", "1.4.0", "1.1.0"):
            labels = self.selector_labels(version)
            current = [label for label in labels if label.endswith("(current)")]
            self.assertEqual(["v1.7.0 (current)"], current, version)

    def test_generated_site_passes_the_built_site_checker(self) -> None:
        sys.path.insert(0, str(TOOLS))
        from check_built_docs import check_site  # noqa: E402

        self.assertEqual([], check_site(self.site))

    def test_version_switching_assets_include_the_page_preserving_handler(self) -> None:
        site_js = (self.site / "1.7.0" / "assets" / "site.js").read_text(encoding="utf-8")
        self.assertIn('method: "HEAD"', site_js)
        self.assertIn("response.ok ? target : targetIndex", site_js)

    def test_theme_search_and_page_outline_are_present_in_released_pages(self) -> None:
        pages = [path for path in (self.site / "1.7.0").rglob("*.html")]
        selector_pages = [path for path in pages if 'id="version-select"' in path.read_text(encoding="utf-8")]
        self.assertTrue(selector_pages)
        for page in selector_pages:
            content = page.read_text(encoding="utf-8")
            self.assertIn('id="theme-toggle"', content, str(page))
            self.assertIn('id="search"', content, str(page))
        self.assertTrue(
            any('class="on-page"' in page.read_text(encoding="utf-8") for page in selector_pages),
            "none of the v1.7.0 pages generated an On-this-page outline",
        )


if __name__ == "__main__":
    unittest.main()
