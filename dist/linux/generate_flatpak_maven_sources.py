#!/usr/bin/env python3
#
# This script is a modified version of the script found at
#
# https://github.com/lenucksi/SieveEditor/blob/master/scripts/generate_flatpak_maven_sources.py
#
"""
Generate Flatpak Maven sources YAML from Maven download log.

This script uses dl2.txt as the source of truth and generates YAML entries
for all downloaded Maven artifacts (.jar and .pom files).

Usage:
    python3 generate_flatpak_maven_sources.py dl2.txt test/ maven-sources.yaml
"""

import sys
import os
import re
import hashlib
import argparse
import xml.etree.ElementTree as ET
from pathlib import Path
from typing import Dict, Set, Optional, List


def extract_relative_path(url: str) -> Optional[str]:
    """
    Extract the relative Maven repository path from a URL.

    Supports:
    - Maven Central: https://repo.maven.apache.org/maven2/path/to/file.jar
    - JitPack: https://jitpack.io/path/to/file.jar
    - Other repositories with similar structure

    Args:
        url: The download URL

    Returns:
        Relative path (e.g., "com/example/artifact/1.0/artifact-1.0.jar") or None
    """
    # Try Maven Central pattern
    match = re.search(r'/maven2/(.+)$', url)
    if match:
        return match.group(1)

    # Try JitPack pattern
    match = re.search(r'jitpack\.io/(.+)$', url)
    if match:
        return match.group(1)

    # Try generic pattern: anything after repository name
    # This handles other repository types
    match = re.search(r'(?:repository|maven|repo)/(.+)$', url)
    if match:
        return match.group(1)

    return None


def parse_download_log(log_file: Path) -> Dict[str, str]:
    """
    Parse Maven download log and extract URL mappings.

    Args:
        log_file: Path to dl2.txt

    Returns:
        Dictionary mapping {relative_path: url} for all .jar and .pom files
    """
    url_map = {}
    line_number = 0

    with open(log_file, 'r', encoding='utf-8') as f:
        for line in f:
            line_number += 1
            # Match lines like:
            # [INFO] Downloaded from central: https://repo.maven.apache.org/maven2/...
            # [INFO] Downloaded from jitpack.io: https://jitpack.io/...
            match = re.search(r'Downloaded from [\w.]+:\s+(https?://\S+)', line)
            if not match:
                continue

            url = match.group(1)

            # Only process .jar and .pom files
            # Also needs metadata.xml from some repos
            if not url.endswith(('.jar', '.pom', 'metadata.xml')):
                continue

            # Extract relative path
            relative_path = extract_relative_path(url)
            if not relative_path:
                print(f"Warning: Could not extract path from URL at line {line_number}: {url}")
                print(f"  This file will be skipped. Please report this if it's a valid Maven URL.")
                continue

            # Store mapping (dict automatically deduplicates)
            if relative_path in url_map and url_map[relative_path] != url:
                print(f"Warning: Duplicate entry for {relative_path} with different URLs:")
                print(f"  First:  {url_map[relative_path]}")
                print(f"  Second: {url}")
                print(f"  Using the first URL.")
            else:
                # Change name from maven-metadata.xml to maven-metadata-central.xml
                rp = Path(relative_path)
                if rp.name == 'maven-metadata.xml':
                    rp = rp.parent / 'maven-metadata-central.xml'
                url_map[str(rp)] = url

    return url_map


def scan_maven_repo(repo_path: Path) -> Set[str]:
    """
    Scan Maven repository directory for all .jar and .pom files.

    Args:
        repo_path: Path to the Maven repository (e.g., test/)

    Returns:
        Set of relative paths for all .jar and .pom files
    """
    found_files = set()

    for file_path in repo_path.rglob('*'):
        if not file_path.is_file():
            continue

        # Skip metadata files
        if file_path.name in ('_remote.repositories',) or \
           file_path.name.endswith(('.sha1', '.sha256', '.md5', '.lastUpdated', '.repositories')):
            continue

        # Only include .jar and .pom files
        if file_path.suffix in ('.jar', '.pom'):
            # Get relative path from repo root
            relative_path = str(file_path.relative_to(repo_path))
            if 'vassal' in relative_path or 'wizard' in relative_path:
                continue

            found_files.add(relative_path)

    return found_files


def calculate_sha256(file_path: Path) -> str:
    """
    Calculate SHA256 checksum of a file.

    Args:
        file_path: Path to the file

    Returns:
        SHA256 hex digest
    """
    sha256_hash = hashlib.sha256()

    with open(file_path, "rb") as f:
        # Read file in chunks to handle large files
        for byte_block in iter(lambda: f.read(65536), b""):
            sha256_hash.update(byte_block)

    return sha256_hash.hexdigest()


def generate_yaml_entry(dest_path: str, url: str, sha256: str) -> str:
    """
    Generate a YAML entry for Flatpak Maven sources.

    Args:
        dest_path: Destination directory in .m2/repository
        url: Download URL
        sha256: SHA256 checksum

    Returns:
        Formatted YAML block
    """
    # If it's metadata, change the name
    destname = ''
    if url.endswith('metadata.xml'):
        destname = '\n  dest-filename: maven-metadata-central.xml'

    return f"""- type: file
  dest: .m2/repository/{dest_path}{destname}
  url: {url}
  sha256: {sha256}"""


def expected_artifact_dest(group_id: str, artifact_id: str, version: str) -> str:
    """Generate the expected YAML dest path for a dependency GAV."""
    group_path = group_id.replace('.', '/')
    return f".m2/repository/{group_path}/{artifact_id}/{version}"


def validate_against_pom(pom_path: Path, yaml_entries: List[str]) -> None:
    """
    Parse pom.xml and validate that all expected dependency artifacts
    have corresponding entries in the generated YAML.

    Non-test/provided scope missing → hard FAIL (exit 1).
    Test scope missing → WARNING only.
    """
    ns = {'m': 'http://maven.apache.org/POM/4.0.0'}
    tree = ET.parse(pom_path)
    root = tree.getroot()

    # Collect all dest paths from YAML entries
    yaml_dests = set()
    for entry in yaml_entries:
        for line in entry.split('\n'):
            line = line.strip()
            if line.startswith('dest:'):
                dest = line.split(':', 1)[1].strip()
                yaml_dests.add(dest)

    missing_compile = []
    missing_test = []

    for dep in root.findall('.//m:dependency', ns):
        g_el = dep.find('m:groupId', ns)
        a_el = dep.find('m:artifactId', ns)
        v_el = dep.find('m:version', ns)
        if g_el is None or a_el is None or v_el is None:
            continue
        g = g_el.text
        a = a_el.text
        v = v_el.text
        if g is None or a is None or v is None:
            continue

        scope_el = dep.find('m:scope', ns)
        scope = scope_el.text if scope_el is not None else 'compile'

        # Resolve version property if applicable
        if v.startswith('${'):
            prop_name = v[2:-1]
            prop_el = root.find(f'.//m:{prop_name}', ns)
            if prop_el is not None and prop_el.text:
                v = prop_el.text
            else:
                print(f"  WARNING: Cannot resolve version property '{v}' for {g}:{a}, skipping validation")
                continue

        expected = expected_artifact_dest(g, a, v)

        if expected not in yaml_dests:
            missing = [expected]
        else:
            missing = []

        if missing:
            if scope in ('compile', 'runtime'):
                missing_compile.append((g, a, v, scope, missing))
            elif scope == 'provided':
                missing_compile.append((g, a, v, scope, missing))
            else:
                missing_test.append((g, a, v, scope, missing))

    if missing_test:
        print(f"\nWARNING: {len(missing_test)} test-scope dependencies missing from YAML (may be expected):")
        for g, a, v, scope, paths in missing_test:
            for p in paths:
                print(f"  - {g}:{a}:{v} ({scope}) → {p}")

    if missing_compile:
        print(f"\nERROR: {len(missing_compile)} compile/runtime/provided dependencies missing from YAML:", file=sys.stderr)
        for g, a, v, scope, paths in missing_compile:
            for p in paths:
                print(f"  - {g}:{a}:{v} ({scope}) → {p}", file=sys.stderr)
        print("\nThis means the Flatpak build will fail at runtime.", file=sys.stderr)
        sys.exit(1)

    if not missing_test and not missing_compile:
        print("  All pom.xml dependencies have corresponding YAML entries")


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(description='Generate Flatpak Maven sources YAML from Maven download log.')
    parser.add_argument('download_log', type=Path, help='Maven download log (e.g., dl2.txt)')
    parser.add_argument('maven_repo_dir', type=Path, help='Maven repository directory (e.g., test/)')
    parser.add_argument('output_yaml', type=Path, help='Output YAML file (e.g., maven-sources.yaml)')
    parser.add_argument('--pom', type=Path, default=None, help='Path to pom.xml for GAV validation')
    args = parser.parse_args()

    log_file = args.download_log
    maven_repo_dir = args.maven_repo_dir
    output_file = args.output_yaml
    pom_path = args.pom

    # Validate inputs
    if not log_file.exists():
        print(f"ERROR: Download log file not found: {log_file}", file=sys.stderr)
        sys.exit(1)

    if not maven_repo_dir.exists() or not maven_repo_dir.is_dir():
        print(f"ERROR: Maven repository directory not found: {maven_repo_dir}", file=sys.stderr)
        sys.exit(1)

    print("=" * 70)
    print("Flatpak Maven Sources Generator")
    print("=" * 70)
    print()

    # PHASE 1: Parse dl2.txt (source of truth)
    print("PHASE 1: Parsing download log...")
    print(f"  Reading: {log_file}")
    url_map = parse_download_log(log_file)
    print(f"  Found {len(url_map)} unique artifacts in download log")
    print()

    # PHASE 2: Process each entry from dl2.txt
    print("PHASE 2: Processing artifacts and calculating checksums...")
    yaml_entries = []
    missing_files = []
    processed_count = 0

    for relative_path, url in sorted(url_map.items()):
        # Construct expected file location
        file_path = maven_repo_dir / relative_path

        # Check if file exists
        if not file_path.exists():
            missing_files.append((relative_path, url))
            print(f"  ERROR: File not found in repository: {relative_path}", file=sys.stderr)
            continue

        # Calculate SHA256
        processed_count += 1
        if processed_count % 50 == 0 or processed_count == 1:
            print(f"  Processing {processed_count}/{len(url_map)}: {relative_path}")

        try:
            sha256 = calculate_sha256(file_path)
        except Exception as e:
            print(f"  ERROR: Failed to calculate SHA256 for {relative_path}: {e}", file=sys.stderr)
            missing_files.append((relative_path, url))
            continue

        # Generate YAML entry
        # dest_path is the parent directory of the file
        dest_path = str(Path(relative_path).parent)
        yaml_entry = generate_yaml_entry(dest_path, url, sha256)
        yaml_entries.append(yaml_entry)

    print(f"  Completed processing {processed_count}/{len(url_map)} artifacts")
    print()

    # Check for errors in Phase 2
    if missing_files:
        print(f"ERROR: {len(missing_files)} files from dl2.txt were not found in {maven_repo_dir}", file=sys.stderr)
        print("This indicates a problem with the Maven download or repository.", file=sys.stderr)
        print("\nMissing files:", file=sys.stderr)
        for relative_path, url in missing_files[:20]:
            print(f"  - {relative_path}", file=sys.stderr)
        if len(missing_files) > 20:
            print(f"  ... and {len(missing_files) - 20} more", file=sys.stderr)
        sys.exit(1)

    # PHASE 3: Validation - check for orphaned files
    print("PHASE 3: Validating repository contents...")
    print(f"  Scanning {maven_repo_dir} for .jar and .pom files...")
    found_files = scan_maven_repo(maven_repo_dir)
    print(f"  Found {len(found_files)} files in repository")

    # Check for files in test/ that weren't in dl2.txt
    orphaned_files = found_files - set(url_map.keys())

    if orphaned_files:
        print(f"\nERROR: {len(orphaned_files)} files in {maven_repo_dir} were not in dl2.txt", file=sys.stderr)
        print("This indicates a problem with the download log or repository.", file=sys.stderr)
        print("\nOrphaned files:", file=sys.stderr)
        maxl = 50
        for file_path in sorted(list(orphaned_files)[:maxl]):
            print(f"  - {file_path}", file=sys.stderr)
        if len(orphaned_files) > maxl:
            print(f"  ... and {len(orphaned_files) - maxl} more", file=sys.stderr)
        sys.exit(1)

    print("  Validation passed: All files accounted for")
    print()

    # PHASE 4: Validate against pom.xml if requested
    if pom_path is not None:
        print("PHASE 4: Validating against pom.xml...")
        validate_against_pom(pom_path, yaml_entries)
        print()

    # Write output YAML
    print(f"Writing YAML to: {output_file}")
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write('\n'.join(yaml_entries))
        f.write('\n')  # Trailing newline

    print()
    print("=" * 70)
    print("SUCCESS!")
    print("=" * 70)
    print(f"  Files in download log:   {len(url_map)}")
    print(f"  Files in repository:     {len(found_files)}")
    print(f"  YAML entries generated:  {len(yaml_entries)}")
    print(f"  Output file:             {output_file}")
    print()
    print("Next step: Test with flatpak-builder")
    print("=" * 70)


if __name__ == '__main__':
    main()
