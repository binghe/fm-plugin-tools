# Testing the XRechnung Plugin

This document explains how to test the XRechnung FileMaker plugin using the automated test scripts.

## Quick Start

```bash
# 1. Generate test scripts using FMScriptGen tool
cd tools/FMScriptGen
swift build -c release

# 2. Generate each script
.build/release/fmscriptgen clear     # Paste as "Clear Test Results"
.build/release/fmscriptgen test1     # Paste as "Test 1: Success Case - Valid PDF"
.build/release/fmscriptgen test2     # Paste as "Test 2: Error - Empty Container"
.build/release/fmscriptgen runall    # Paste as "Run All Tests"

# 3. In FileMaker:
#    - Open Script Workspace (Cmd+Shift+S)
#    - Create new script with the name shown above
#    - Paste (Cmd+V)
#    - Save

# 4. Run tests
#    - Load a PDF into container field
#    - Run "Run All Tests" script
#    - Check results in script_result and test_log fields
```

## Installation (Optional)

For easier access, install the tool system-wide:

```bash
cd tools/FMScriptGen
./install.sh

# Then use from anywhere:
fmscriptgen test1
```

## Required Fields

Create these fields in your FileMaker table before running tests:

- **script_result** (Text) - Stores JSON responses
- **test_log** (Text) - Stores test history
- **test_status** (Text) - Current test status
- **plu** (Container) - PDF binary data for testing

## Available Test Scripts

### Core Scripts
- **Clear Test Results** - Reset all test data
- **Run All Tests** - Execute all tests and show report

### Individual Tests
- **Test 1: Success Case - Valid PDF** - Tests ConvertToPDFA with valid PDF
- **Test 2: Error - Empty Container** - Tests error handling with empty input

## What Gets Tested

### ConvertToPDFA Function
- ✓ Valid PDF conversion to PDF/A
- ✓ Empty container error handling
- ✓ JSON response format
- ✓ Error message content

### Expected Results
All tests should return JSON in this format:
```json
{
  "success": true/false,
  "result": "/path/to/output.pdf",
  "error": "error message if any"
}
```

## Test Workflow

1. **Clear Results** - Run before testing
2. **Run All Tests** - Execute full test suite
3. **Review Report** - Check pass/fail counts
4. **Examine Logs** - Review test_log field for details

## Tool Architecture

```
tools/FMScriptGen/          - Swift script generator
├── Sources/main.swift      - Main tool code
├── Package.swift           - Swift package definition
├── install.sh              - Installation script
├── USAGE.md               - Detailed usage guide
└── README.md              - Overview

example_data/scripts/       - Deprecated XML files (reference only)
└── README.md              - Points to FMScriptGen tool
```

## Technical Details

The FMScriptGen tool:
- Generates FileMaker's `fmxmlsnippet` XML format
- Uses pasteboard type `dyn.ah62d4rv4gk8zuxnxnq`
- Creates compact XML without whitespace
- Writes directly to macOS clipboard

This format was reverse-engineered by analyzing FileMaker's actual
clipboard format when copying script steps.

## Troubleshooting

**Problem**: Paste doesn't work in FileMaker
- **Solution**: Make sure you created a new script first, then paste

**Problem**: Script shows <Field Missing>
- **Solution**: Field doesn't exist - create required fields listed above

**Problem**: Tool not found
- **Solution**: Run from tools/FMScriptGen directory or install with ./install.sh

**Problem**: Scripts don't run
- **Solution**: Make sure "Run All Tests" is calling scripts with exact names

## Adding More Tests

To add more test scripts, edit `tools/FMScriptGen/Sources/main.swift`:

1. Add generator function (e.g., `generateTestScript3()`)
2. Add case to switch statement in `main()`
3. Rebuild: `swift build -c release`
4. Generate: `.build/release/fmscriptgen test3`

## Documentation

- **Tool Usage**: `tools/FMScriptGen/USAGE.md`
- **Deprecated XMLs**: `example_data/scripts/_DEPRECATED_README.txt`
- **Plugin Functions**: `functions.lisp`
- **This File**: `TESTING.md`

---

**Always use FMScriptGen tool, not the XML files!**
