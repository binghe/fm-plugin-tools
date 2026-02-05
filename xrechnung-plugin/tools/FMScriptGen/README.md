# FMScriptGen - FileMaker Script Generator

✅ **WORKING!** Generate FileMaker script steps from simple text files!

## Quick Start

```bash
# Build
swift build -c release

# Generate any script
.build/release/fmscriptgen test1    # Predefined test
.build/release/fmscriptgen clear    # Clear results
.build/release/fmscriptgen runall   # Run all tests

# Or use custom file
.build/release/fmscriptgen /path/to/your/script.txt

# Then paste in FileMaker (Cmd+V)
```

## Installation (Optional)

```bash
./install.sh
# Then use from anywhere:
fmscriptgen test1
```

## Available Predefined Scripts

All in `script-definitions/` folder:

- **test1** - Success Case: Valid PDF conversion
- **test2** - Error: Empty Container
- **test3** - Error: Invalid Output Path
- **test4** - Error: File Not Found
- **test5** - Success: File Path conversion
- **test6** - Success: With Metadata
- **clear** - Clear Test Results
- **runall** - Run All Tests

## Creating Your Own Scripts

Create a text file with simple commands:

```
# my-script.txt
SetVariable: $greeting = "Hello!"
ShowDialog: "Test" $greeting

If: IsEmpty($greeting)
  Exit
EndIf

SetField: myTable::result = "Success"
```

Then generate:
```bash
fmscriptgen my-script.txt
```

## Script Format Reference

See [SCRIPT_FORMAT.md](SCRIPT_FORMAT.md) for complete syntax reference.

Quick reference:
- `SetVariable: $name = value`
- `SetField: table::field = calculation`
- `If: condition` / `Else` / `EndIf`
- `Exit`
- `ShowDialog: "title" "message"`
- `PerformScript: "script name"`
- `# Comments`

## How It Works

1. **Parses text file** - Simple, readable format
2. **Generates XML** - Creates FileMaker's `fmxmlsnippet` format
3. **Copies to clipboard** - Uses pasteboard type `dyn.ah62d4rv4gk8zuxnxnq`
4. **Paste in FileMaker** - Works instantly in Script Workspace!

## Technical Details

- **Language**: Swift 5.9+
- **Platform**: macOS 12+
- **Format**: FileMaker XML Snippet (fmxmlsnippet)
- **Pasteboard Type**: `dyn.ah62d4rv4gk8zuxnxnq` (FileMaker's dynamic type)

## Files

```
FMScriptGen/
├── Sources/
│   ├── main.swift           - Main program
│   └── ScriptParser.swift   - Text format parser
├── script-definitions/      - Predefined scripts
│   ├── test1.txt
│   ├── test2.txt
│   ├── ...
│   ├── clear.txt
│   └── runall.txt
├── Package.swift           - Swift package definition
├── install.sh              - Installation script
├── README.md              - This file
├── USAGE.md               - Detailed usage guide
└── SCRIPT_FORMAT.md       - Format reference
```

## Examples

### Example 1: Simple Test
```bash
fmscriptgen test1
# Paste in FileMaker → Test 1 script ready!
```

### Example 2: Custom Script
```bash
echo 'SetVariable: $x = 42
ShowDialog: "Result" $x' > test.txt

fmscriptgen test.txt
# Paste in FileMaker → Custom script ready!
```

### Example 3: Full Test Suite
```bash
# Generate all scripts
for script in test{1..6} clear runall; do
  fmscriptgen $script
  # Paste each one in FileMaker with appropriate name
done
```

## Benefits

✅ **No hardcoded scripts** - Everything in text files
✅ **Easy to modify** - Edit text files, no code changes
✅ **Shareable** - Send .txt files to colleagues
✅ **Version control** - Text files work great in git
✅ **User-friendly** - Non-programmers can create scripts
✅ **No MBS needed** - Free, open source solution

## Troubleshooting

**Problem**: Paste doesn't work
- **Solution**: Make sure you created a new script first in FileMaker

**Problem**: Field shows `<Field Missing>`
- **Solution**: Field doesn't exist or has different name - fix in FileMaker

**Problem**: Script not found
- **Solution**: Check file exists in script-definitions/ or provide full path

**Problem**: Syntax error in script file
- **Solution**: Check SCRIPT_FORMAT.md for correct syntax

## Contributing

To add more script statements:
1. Edit `Sources/ScriptParser.swift`
2. Add parsing for new statement type
3. Add generator in `Sources/main.swift` (FMScriptGenerator)
4. Rebuild: `swift build`

## Credits

Created with assistance from Claude (Anthropic).
Reverse-engineered FileMaker's clipboard format.
Working solution for programmatic FileMaker script generation.

---

**Ready to generate scripts!** 🚀

```bash
fmscriptgen test1
# → Paste in FileMaker → Done! ✨
```
