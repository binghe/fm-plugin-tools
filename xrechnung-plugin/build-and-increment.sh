#!/bin/bash

# Increment version and build plugin

ASD_FILE="${HOME}/quicklisp/local-projects/fm-plugin-tools/fm-plugin-xrechnung.asd"

# Read current version
current_version=$(grep "defvar \*xrechnung-plugin-version\*" "$ASD_FILE" | sed -E "s/.*'\\(([0-9]+) ([0-9]+) ([0-9]+)\\).*/\\1.\\2.\\3/")

# Extract major, minor, patch
major=$(echo $current_version | cut -d. -f1)
minor=$(echo $current_version | cut -d. -f2)
patch=$(echo $current_version | cut -d. -f3)

# Increment patch version
new_patch=$((patch + 1))
new_version="${major}.${minor}.${new_patch}"

echo "Incrementing version: ${current_version} -> ${new_version}"

# Update the .asd file
sed -i '' "s/(defvar \*xrechnung-plugin-version\* '([0-9]* [0-9]* [0-9]*)/(defvar *xrechnung-plugin-version* '($major $minor $new_patch)/" "$ASD_FILE"

# Now run the normal build script
cd "$(dirname "$0")"
./build-plugin64.command

echo "Built version ${new_version}"
