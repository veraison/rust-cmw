#!/bin/bash

# A script to find the generated cfmap.rs, format it, and move it to the src/ directory.

# Exit immediately if a command exits with a non-zero status.
set -e

# --- Step 1: Build the project to ensure the file is generated ---
echo "Building project to generate cfmap.rs..."
# We build quietly to avoid cluttering the output, but cargo will still show errors.
cargo build --quiet --features rebuild-cfmap

# --- Step 2: Find the generated file in the target directory ---
echo "Searching for cfmap.rs in ./target..."
# The file could be in target/debug/build/... or target/release/build/...
# We find any file with that name and take the first result.
GENERATED_FILE=$(find ./target -name "cfmap.rs" | head -n 1)

# Check if the file was found. If not, exit with an error.
if [ -z "$GENERATED_FILE" ]; then
    echo "Error: Could not find cfmap.rs in the ./target directory." >&2
    echo "Please ensure your build.rs script is set up correctly." >&2
    exit 1
fi

echo "Found generated file at: $GENERATED_FILE"

# --- Step 3: Apply rustfmt to the file ---
echo "Formatting file with rustfmt..."
rustfmt "$GENERATED_FILE"

# --- Step 4: Move the file to the src directory ---
DESTINATION="src/cfmap.rs"
echo "Moving formatted file to $DESTINATION..."
mv "$GENERATED_FILE" "$DESTINATION"

echo "Done. $DESTINATION has been updated."
