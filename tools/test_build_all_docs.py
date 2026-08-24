#!/usr/bin/env python3
"""Regression tests for building all declared documentation releases."""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


TOOLS = Path(__file__).resolve().parent
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
        TOOLS = Path(__file__).resolve().parent
        ROOT = TOOLS.parent
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


if __name__ == "__main__":
    unittest.main()
