# FMScriptGen - FileMaker Script Generator

Swift command-line tool that generates FileMaker script steps and copies them to clipboard.

## Quick Start

```bash
# Build the tool
cd tools/FMScriptGen
swift build -c release

# Run it
.build/release/fmscriptgen test1

# Now paste in FileMaker (Cmd+V)
```

## Installation

```bash
# Copy to PATH for easy access
cp .build/release/fmscriptgen /usr/local/bin/

# Now use from anywhere
fmscriptgen test1
```

## Usage

```bash
fmscriptgen [command]

Commands:
  test1    - Generate Test 1: Success Case
  test2    - Generate Test 2: Empty Container
  clear    - Generate Clear Results script
  runall   - Generate Run All Tests script
  list     - List all available scripts
  help     - Show help
```

## How It Works

1. Tool generates proper FileMaker XML format
2. Copies to macOS clipboard (multiple pasteboard types)
3. You paste directly in FileMaker Script Workspace
4. Script steps appear instantly!

## Development

Built with:
- Swift 5.9+
- macOS 12+
- AppKit (for NSPasteboard)

## Next Steps

Once you paste a real FileMaker clipboard sample, I'll update the XML format
to match exactly what FileMaker expects!
