#!/bin/bash
# Install fmscriptgen tool

set -e

echo "🔨 Building FMScriptGen..."
swift build -c release

echo "📦 Installing to /usr/local/bin..."
sudo cp .build/release/fmscriptgen /usr/local/bin/
sudo chmod +x /usr/local/bin/fmscriptgen

echo "✅ Installation complete!"
echo ""
echo "Usage:"
echo "  fmscriptgen test1    # Generate Test 1"
echo "  fmscriptgen test2    # Generate Test 2"
echo "  fmscriptgen clear    # Generate Clear Results"
echo "  fmscriptgen runall   # Generate Run All Tests"
echo "  fmscriptgen list     # List all scripts"
echo ""
echo "Then paste in FileMaker (Cmd+V)"
