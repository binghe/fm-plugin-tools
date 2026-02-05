# FMScriptGen - FileMaker Script Generator

✅ **WORKING!** Successfully generates FileMaker script steps!

## Quick Start

```bash
# Install
cd /path/to/FMScriptGen
chmod +x install.sh
./install.sh

# Use
fmscriptgen test1
# Now paste in FileMaker (Cmd+V)
```

## Available Commands

```bash
fmscriptgen test1    # Test 1: Success Case - Valid PDF
fmscriptgen test2    # Test 2: Error - Empty Container
fmscriptgen clear    # Clear Test Results
fmscriptgen runall   # Run All Tests

fmscriptgen list     # Show all available
fmscriptgen help     # Show help
```

## How It Works

1. **Generates proper XML** - Creates FileMaker's `fmxmlsnippet` format
2. **Copies to clipboard** - Uses the exact pasteboard type FileMaker expects:
   - `dyn.ah62d4rv4gk8zuxnxnq` (FileMaker's dynamic type)
3. **Paste directly** - Works instantly in FileMaker Script Workspace

## Example Workflow

```bash
# 1. Generate test script
fmscriptgen test1

# 2. In FileMaker:
#    - Open Script Workspace (Cmd+Shift+S)
#    - Create new script
#    - Name it "Test 1: Success Case - Valid PDF"
#    - Paste (Cmd+V)
#    - Save

# 3. Repeat for other scripts
fmscriptgen test2   # Paste as "Test 2: Error - Empty Container"
fmscriptgen clear   # Paste as "Clear Test Results"
fmscriptgen runall  # Paste as "Run All Tests"
```

## Technical Details

- **Language**: Swift 5.9+
- **Platform**: macOS 12+
- **Dependencies**: AppKit (for NSPasteboard)
- **Format**: FileMaker XML Snippet (fmxmlsnippet)

## Format Discovery

The correct format was reverse-engineered by:
1. Copying a FileMaker script step
2. Inspecting clipboard with custom tool
3. Identifying pasteboard type: `dyn.ah62d4rv4gk8zuxnxnq`
4. Analyzing XML structure

Key findings:
- No `<?xml` declaration
- Compact format (no whitespace)
- `<Calculation>` before `<Field>` in Set Field steps
- Specific pasteboard type required

## Future Enhancements

- [ ] Add Test 3-6 scripts
- [ ] Generate complete scripts (with Script wrapper)
- [ ] Custom field ID mapping
- [ ] Configuration file support
- [ ] Batch generation

## Credits

Created with assistance from Claude (Anthropic).
Tested and working with FileMaker Pro 19+.
