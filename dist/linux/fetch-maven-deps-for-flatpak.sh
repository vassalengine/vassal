#!/bin/bash
#
# This script is a modified version of
#
# https://github.com/lenucksi/SieveEditor/blob/master/scripts/fetch-maven-deps-for-flatpak.sh
#
# Fetch Maven dependencies and generate Flatpak YAML
# This script automates the entire process of fetching Maven dependencies
# and generating the maven-dependencies.yaml file for Flatpak builds.

set -euo pipefail  # Strict error handling

# --- Configuration ---
# Determine script directory and repo root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(pwd)"

# Output location (can be overridden by first argument)
OUTPUT_DIR="${1:-${REPO_ROOT}/flatpak}"
OUTPUT_YAML="${OUTPUT_DIR}/maven-dependencies.yml"
BACKUP_YAML="${OUTPUT_YAML}.old"
PYTHON_GENERATOR="${SCRIPT_DIR}/generate_flatpak_maven_sources.py"

# Ensure output directory exists
mkdir -p "${OUTPUT_DIR}"

# Change to repo root for Maven execution
cd "${REPO_ROOT}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# --- Functions ---

generate_random_id() {
  # Generate 12-character alphanumeric ID
  head -c 16 /dev/urandom | base64 | tr -dc 'a-z0-9' | head -c 12
}

cleanup_old_fetch_dirs() {
  echo -e "${BLUE}Cleaning up old fetch directories...${NC}"

  # Remove directories matching pattern
  local count=0
  rm -rf mvn-dep-fetch-* mvn-download-*.log*
  for dir in mvn-dep-fetch-* mvn-download-*.log; do
    if [ -e "$dir" ]; then
      echo "  Removing: $dir"
      rm -rf "$dir" || true
      ((count++))
    fi
  done

  # Also remove old test/ directory if it exists
  if [ -d "test" ]; then
    echo "  Removing: test/"
    rm -rf test || true
    ((count++))
  fi

  if [ $count -eq 0 ]; then
    echo "  No old directories to clean"
  else
    echo -e "${GREEN}  Removed $count item(s)${NC}"
  fi
}

run_maven_fetch() {
  local temp_dir="$1"
  local temp_log="$2"

  echo ""
  echo -e "${BLUE}Fetching Maven dependencies...${NC}"
  echo "  Maven repo: ${temp_dir}"
  echo "  Download log: ${temp_log}"
  echo ""

  # Run Maven and capture full output to a temp file
  local maven_output="${temp_log}.full"

  make jar MVN="mvn -B -Dmaven.repo.local=${temp_dir} -DskipTests" 2>/dev/stdout | tee ${maven_output}
  ret1=$?
  echo "  Make return: $ret1"

  if test $ret1 -eq 0; then
    # Maven succeeded, extract download lines
    grep "Downloaded" "${maven_output}" > "${temp_log}" || true

    local download_count
    download_count=$(wc -l < "${temp_log}" 2>/dev/null || echo "0")
    echo -e "${GREEN}  Maven build successful${NC}"
    echo "  Downloaded: ${download_count} artifacts"

    # Show last few lines of Maven output
    echo ""
    echo "Last 5 lines of Maven output:"
    tail -5 "${maven_output}"

    # Clean up full output
    rm -f "${maven_output}"
    return 0
  else
    echo -e "${RED}  ERROR: Maven build failed${NC}"
    echo ""
    echo "Last 20 lines of Maven output:"
    tail -20 "${maven_output}"
    echo ""
    echo "Full Maven output saved to: ${maven_output}"
    return 1
  fi
}

generate_yaml() {
  local temp_dir="$1"
  local temp_log="$2"
  local output="$3"

  echo ""
  echo -e "${BLUE}Generating Flatpak YAML...${NC}"

  if [ ! -f "${PYTHON_GENERATOR}" ]; then
    echo -e "${RED}ERROR: Python generator script not found: ${PYTHON_GENERATOR}${NC}"
    return 1
  fi

  if python3 "${PYTHON_GENERATOR}" "${temp_log}" "${temp_dir}" "${output}"; then
    echo -e "${GREEN}  YAML generated: ${output}${NC}"
  else
    echo -e "${RED}  ERROR: YAML generation failed${NC}"
    echo "  Temporary files preserved for debugging:"
    echo "    - ${temp_dir}"
    echo "    - ${temp_log}"
    return 1
  fi
}

compare_and_backup() {
  local output="$1"
  local backup="$2"

  echo ""
  if [ ! -f "${backup}" ]; then
    echo -e "${YELLOW}No previous YAML found${NC}"
    echo "  This appears to be the first run"
    return 0
  fi

  echo -e "${BLUE}Comparing with previous YAML...${NC}"

  if diff -q "${output}" "${backup}" > /dev/null 2>&1; then
    echo -e "${GREEN}  No changes detected${NC}"
    echo "  Dependencies are up to date"
  else
    echo -e "${YELLOW}  Changes detected!${NC}"
    echo ""
    echo "--- Diff Summary (first 50 lines) ---"
    diff -u "${backup}" "${output}" | head -50 || true
    echo ""

    local added
    local removed
    added=$(diff "${backup}" "${output}" | grep -c "^+" || echo "0")
    removed=$(diff "${backup}" "${output}" | grep -c "^-" || echo "0")
    echo "  Lines added: ${added}"
    echo "  Lines removed: ${removed}"
    echo ""
    echo "  Full diff available: diff ${backup} ${output}"
  fi
}

cleanup_temp_files() {
  local temp_dir="$1"
  local temp_log="$2"

  echo ""
  echo -e "${BLUE}Cleaning up temporary files...${NC}"

  if [ -d "${temp_dir}" ]; then
    rm -rf "${temp_dir}"
    echo "  Removed: ${temp_dir}"
  fi

  if [ -f "${temp_log}" ]; then
    rm -f "${temp_log}"
    echo "  Removed: ${temp_log}"
  fi

  # Also remove .full file if it exists
  if [ -f "${temp_log}.full" ]; then
    rm -f "${temp_log}.full"
    echo "  Removed: ${temp_log}.full"
  fi

  echo -e "${GREEN}  Cleanup complete${NC}"
}

# --- Main Execution ---

main() {
  echo "========================================================================"
  echo "Maven Dependency Fetch for Flatpak"
  echo "========================================================================"
  echo ""
  
  # Generate random ID
  local random_id
  random_id=$(generate_random_id)
  local temp_dir="mvn-dep-fetch-${random_id}"
  local temp_log="mvn-download-${random_id}.log"

  echo "Session ID: ${random_id}"

  # Backup existing YAML if present
  if [ -f "${OUTPUT_YAML}" ]; then
    cp "${OUTPUT_YAML}" "${BACKUP_YAML}"
    echo "Backed up existing YAML: ${BACKUP_YAML}"
  fi

  # Execute workflow
  cleanup_old_fetch_dirs

  if ! run_maven_fetch "${temp_dir}" "${temp_log}"; then
    echo -e "${RED}Maven fetch failed. Temporary files preserved for debugging.${NC}"
    exit 1
  fi

  if ! generate_yaml "${temp_dir}" "${temp_log}" "${OUTPUT_YAML}"; then
    echo -e "${RED}YAML generation failed. Temporary files preserved for debugging.${NC}"
    exit 1
  fi

  compare_and_backup "${OUTPUT_YAML}" "${BACKUP_YAML}"
  cleanup_temp_files "${temp_dir}" "${temp_log}"

  echo ""
  echo "========================================================================"
  echo -e "${GREEN}SUCCESS!${NC}"
  echo "========================================================================"
  echo "Output: ${OUTPUT_YAML}"
  echo ""
  echo "Next steps:"
  echo "  1. Review the generated YAML file"
  echo "  2. Copy to your Flatpak manifest directory"
  echo "  3. Test with: flatpak-builder --sandbox builddir/ manifest.yaml"
  echo ""
}

# Error handling
trap_error() {
  local exit_code=$?
  if [ $exit_code -ne 0 ]; then
    echo ""
    echo -e "${RED}========================================================================"
    echo "ERROR: Script failed with exit code ${exit_code}"
    echo "========================================================================${NC}"
    echo ""
    echo "Temporary files may have been preserved for debugging."
    echo "Check for directories matching: mvn-dep-fetch-*"
    echo ""
  fi
}

trap trap_error EXIT

# Run main function
main "$@"
