#!/usr/bin/env python3
"""Regression tests for the lightweight documentation publisher."""

from __future__ import annotations

import json
import sys
import tempfile
import unittest
from pathlib import Path


TOOLS = Path(__file__).resolve().parent
ROOT = TOOLS.parent
sys.path.insert(0, str(TOOLS))

from build_docs import build_site  # noqa: E402


class BuildDocsTests(unittest.TestCase):
    def write_fixture(self, root: Path) -> tuple[Path, Path, Path]:
        source = root / "docs"
        output = root / "site" / "1.9.1"
        site_root = output.parent
        (source / "start").mkdir(parents=True)
        (source / "index.md").write_text(
            "# ChronoKit-FP documentation\n\n"
            "Start with the [guide](start/guide.md).\n",
            encoding="utf-8",
        )
        (source / "start" / "guide.md").write_text(
            "# A tiny guide\n\n"
            "> [!NOTE]\n"
            "> This guide keeps Pascal's familiar 1-based indexing.\n\n"
            "## Repeat\n\n"
            "```pascal\n"
            "Writeln('Hello');\n"
            "```\n\n"
            "### Details\n\n"
            "The call writes one line.\n\n"
            "## Repeat\n\n"
            "The stable duplicate heading uses a distinct anchor.\n",
            encoding="utf-8",
        )
        (source / "layout.json").write_text(
            json.dumps(
                {
                    "schema_version": 2,
                    "release": "1.9.1",
                    "site_title": "ChronoKit-FP documentation",
                    "description": "Practical ChronoKit-FP documentation.",
                    "required_pages": ["index.md", "start/guide.md"],
                    "navigation": [
                        {
                            "title": "Getting Started",
                            "pages": [
                                {"path": "index.md", "title": "Introduction"},
                                {"path": "start/guide.md", "title": "Beginner Guide"},
                            ],
                        }
                    ],
                    "project": [{"title": "GitHub repository", "url": "https://github.com/example/chronokit-fp"}],
                    "homepage": {
                        "tagline": "A modern string toolkit for Free Pascal and Lazarus.",
                        "actions": [{"label": "Get Started", "path": "start/guide.md"}],
                    },
                }
            ),
            encoding="utf-8",
        )
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
        return source, output, site_root

    def test_builds_documentation_shell_navigation_and_pascal_code(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            source, output, site_root = self.write_fixture(Path(directory))

            build_site(source, output, site_root, source / "versions.json")

            index = (output / "index.html").read_text(encoding="utf-8")
            guide = (output / "start" / "guide.html").read_text(encoding="utf-8")
            landing = (site_root / "index.html").read_text(encoding="utf-8")
            self.assertIn('href="start/guide.html"', index)
            self.assertIn('class="doc-sidebar"', guide)
            self.assertIn('aria-label="Breadcrumb"', guide)
            self.assertIn('class="page-navigation"', guide)
            self.assertIn('class="on-page"', guide)
            self.assertIn('class="copy-code"', guide)
            self.assertIn('class="admonition admonition-note"', guide)
            self.assertIn('class="heading-anchor"', guide)
            self.assertIn('id="repeat-2"', guide)
            self.assertIn('id="version-select"', guide)
            self.assertIn('<pre><code class="language-pascal">', guide)
            self.assertTrue((output / "assets" / "site.css").is_file())
            self.assertTrue((output / "assets" / "site.js").is_file())
            self.assertTrue((output / "search-index.json").is_file())
            self.assertTrue((output / "search-index.js").is_file())
            self.assertIn(':root[data-theme="dark"]', (output / "assets" / "site.css").read_text(encoding="utf-8"))
            self.assertIn("ChronoKitSearchIndex", (output / "assets" / "site.js").read_text(encoding="utf-8"))
            self.assertEqual("Getting Started", json.loads((output / "search-index.json").read_text(encoding="utf-8"))[1]["section"])
            self.assertIn("ChronoKit-FP documentation", landing)

    def test_rejects_a_broken_internal_link(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            source, output, site_root = self.write_fixture(Path(directory))
            (source / "index.md").write_text(
                "# ChronoKit-FP documentation\n\n[Missing](missing.md)\n",
                encoding="utf-8",
            )

            with self.assertRaisesRegex(ValueError, "broken internal link"):
                build_site(source, output, site_root, source / "versions.json")

    def test_rejects_an_unsafe_markdown_url(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            source, output, site_root = self.write_fixture(Path(directory))
            (source / "index.md").write_text(
                "# ChronoKit-FP documentation\n\n[Unsafe](javascript:alert(1))\n",
                encoding="utf-8",
            )

            with self.assertRaisesRegex(ValueError, "unsafe link"):
                build_site(source, output, site_root, source / "versions.json")

    def test_links_project_markdown_to_its_repository_source(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source, output, site_root = self.write_fixture(root)
            (root / "CHANGELOG.md").write_text("# Changelog\n", encoding="utf-8")
            (source / "index.md").write_text(
                "# ChronoKit-FP documentation\n\n[Changelog](../CHANGELOG.md#release-notes)\n",
                encoding="utf-8",
            )

            build_site(source, output, site_root, source / "versions.json")

            index = (output / "index.html").read_text(encoding="utf-8")
            self.assertIn("https://github.com/example/chronokit-fp/blob/main/CHANGELOG.md#release-notes", index)

    def test_builds_a_preserved_release_from_its_own_source(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source, _output, site_root = self.write_fixture(root)
            (source / "layout.json").write_text(
                json.dumps({"schema_version": 1, "release": "1.9.0"}),
                encoding="utf-8",
            )
            versions = source / "versions.json"
            metadata = json.loads(versions.read_text(encoding="utf-8"))
            metadata["versions"].append({"release": "1.9.0", "source_ref": "v1.9.0"})
            versions.write_text(json.dumps(metadata), encoding="utf-8")
            output = site_root / "1.9.0"

            build_site(source, output, site_root, versions, release="1.9.0")

            release = json.loads((output / "release.json").read_text(encoding="utf-8"))
            self.assertEqual("1.9.0", release["release"])

    def test_rejects_an_undeclared_selected_release(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            source, output, site_root = self.write_fixture(Path(directory))

            with self.assertRaisesRegex(ValueError, "not declared"):
                build_site(source, output, site_root, source / "versions.json", release="2.0.0")

    def test_builds_unlisted_compatibility_pages_without_showing_them_in_navigation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            source, output, site_root = self.write_fixture(Path(directory))
            (source / "history.md").write_text(
                "# Historical release note\n\nPreserved for an existing link.\n",
                encoding="utf-8",
            )
            layout_path = source / "layout.json"
            layout = json.loads(layout_path.read_text(encoding="utf-8"))
            layout["hidden_pages"] = ["history.md"]
            layout["required_pages"].append("history.md")
            layout_path.write_text(json.dumps(layout), encoding="utf-8")

            build_site(source, output, site_root, source / "versions.json")

            index = (output / "index.html").read_text(encoding="utf-8")
            self.assertTrue((output / "history.html").is_file())
            self.assertNotIn('href="history.html"', index)

    def test_copies_and_renders_documentation_images(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            source, output, site_root = self.write_fixture(Path(directory))
            (source / "assets").mkdir()
            (source / "assets" / "mark.svg").write_text(
                '<svg xmlns="http://www.w3.org/2000/svg"/>', encoding="utf-8"
            )
            (source / "index.md").write_text(
                "# ChronoKit-FP documentation\n\n"
                "![ChronoKit mark](assets/mark.svg)\n",
                encoding="utf-8",
            )

            build_site(source, output, site_root, source / "versions.json")

            index = (output / "index.html").read_text(encoding="utf-8")
            self.assertIn('src="assets/mark.svg"', index)
            self.assertIn('alt="ChronoKit mark"', index)
            self.assertTrue((output / "assets" / "mark.svg").is_file())

    def test_current_generated_site_contains_no_stringkit_branding(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site_root = Path(directory) / "site"
            build_site(
                ROOT / "docs",
                site_root / "1.7.0",
                site_root,
                ROOT / "docs" / "versions.json",
            )

            generated = "\n".join(
                path.read_text(encoding="utf-8")
                for path in site_root.rglob("*")
                if path.is_file() and path.suffix in {".css", ".html", ".js", ".json", ".txt"}
            ).lower()
            self.assertNotIn("stringkit", generated)

    def write_legacy_fixture(self, root: Path) -> tuple[Path, Path, Path]:
        source = root / "docs"
        output = root / "site" / "1.1.0"
        site_root = output.parent
        source.mkdir()
        (source / "Getting-Started.md").write_text(
            "# Getting Started\n\nInstall and run your first example.\n",
            encoding="utf-8",
        )
        (source / "ChronoKit-FP.md").write_text(
            "# ChronoKit-FP tasks\n\nEvery supported task by name.\n",
            encoding="utf-8",
        )
        versions = source / "versions.json"
        versions.write_text(
            json.dumps(
                {
                    "schema_version": 1,
                    "current": "1.1.0",
                    "site_url": "https://example.invalid/chronokit-fp",
                    "repository_url": "https://github.com/example/chronokit-fp",
                    "versions": [
                        {"release": "1.1.0", "source_ref": "v1.1.0"},
                        {"release": "1.0.0", "source_ref": "v1.0.0"},
                    ],
                }
            ),
            encoding="utf-8",
        )
        return source, output, site_root

    def test_builds_an_older_release_without_layout_or_index_metadata(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            source, output, site_root = self.write_legacy_fixture(Path(directory))

            build_site(source, output, site_root, source / "versions.json")

            index = output / "index.html"
            self.assertTrue(index.is_file())
            content = index.read_text(encoding="utf-8")
            self.assertIn('meta http-equiv="refresh"', content)
            self.assertIn("Getting-Started.html", content)
            self.assertTrue((output / "Getting-Started.html").is_file())
            self.assertTrue((output / "ChronoKit-FP.html").is_file())
            legacy_nav = (output / "Getting-Started.html").read_text(encoding="utf-8")
            self.assertIn('class="docs-navigation"', legacy_nav)

    def test_version_selector_marks_only_current_and_selects_the_viewed_version(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            source, output, site_root = self.write_legacy_fixture(Path(directory))
            versions = source / "versions.json"
            metadata = json.loads(versions.read_text(encoding="utf-8"))
            metadata["current"] = "1.1.0"
            metadata["versions"] = [
                {"release": "1.1.0", "source_ref": "v1.1.0"},
                {"release": "1.0.0", "source_ref": "v1.0.0"},
            ]
            versions.write_text(json.dumps(metadata), encoding="utf-8")

            build_site(source, output, site_root, versions, release="1.1.0")
            build_site(source, site_root / "1.0.0", site_root, versions, release="1.0.0")

            latest = (output / "Getting-Started.html").read_text(encoding="utf-8")
            historical = (site_root / "1.0.0" / "Getting-Started.html").read_text(encoding="utf-8")
            self.assertIn("v1.1.0 (current)", latest)
            self.assertIn("v1.1.0 (current)", historical)
            self.assertNotIn("v1.0.0 (current)", historical)
            self.assertTrue((site_root / "1.0.0" / "index.html").is_file())


if __name__ == "__main__":
    unittest.main()
