#!/usr/bin/env python3
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
            
            # print(f'{line_number:4d}: {line}')
            # if 'vassal-maven' in line:
            #     continue
            # Match lines like:
            # [INFO] Downloaded from central: https://repo.maven.apache.org/maven2/...
            # [INFO] Downloaded from jitpack.io: https://jitpack.io/...
            match = re.search(r'Downloaded from [\w.]+:\s+(https?://\S+)', line)
            if not match:
                continue


            url = match.group(1)

            # Only process .jar and .pom files
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
    destname = ''
    if url.endswith('metadata.xml'):
        destname = '\n  dest-filename: maven-metadata-central.xml'
        
    return f"""- type: file
  dest: .m2/repository/{dest_path}{destname}
  url: {url}
  sha256: {sha256}"""


def main():
    """Main entry point."""
    if len(sys.argv) != 4:
        print(f"Usage: {sys.argv[0]} <download_log> <maven_repo_dir> <output_yaml>", file=sys.stderr)
        print(file=sys.stderr)
        print("Generate Flatpak Maven sources YAML from Maven download log.", file=sys.stderr)
        print(file=sys.stderr)
        print("Arguments:", file=sys.stderr)
        print("  download_log    - Maven download log (e.g., dl2.txt)", file=sys.stderr)
        print("  maven_repo_dir  - Maven repository directory (e.g., test/)", file=sys.stderr)
        print("  output_yaml     - Output YAML file (e.g., maven-sources.yaml)", file=sys.stderr)
        print(file=sys.stderr)
        print("Example:", file=sys.stderr)
        print(f"  {sys.argv[0]} dl2.txt test/ maven-sources.yaml", file=sys.stderr)
        sys.exit(1)

    log_file = Path(sys.argv[1])
    maven_repo_dir = Path(sys.argv[2])
    output_file = Path(sys.argv[3])

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
        for file_path in sorted(list(orphaned_files)[:20]):
            print(f"  - {file_path}", file=sys.stderr)
        if len(orphaned_files) > 20:
            print(f"  ... and {len(orphaned_files) - 20} more", file=sys.stderr)
        sys.exit(1)

    print("  Validation passed: All files accounted for")
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
    print(f"  Files in dl2.txt:        {len(url_map)}")
    print(f"  Files in repository:     {len(found_files)}")
    print(f"  YAML entries generated:  {len(yaml_entries)}")
    print(f"  Output file:             {output_file}")
    print()
    print("Next step: Test with flatpak-builder")
    print("=" * 70)


if __name__ == '__main__':
    main()
