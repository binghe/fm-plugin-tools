# FileMaker Test Scripts

## ⚠️ IMPORTANT: Documentation Update

**The XML files in this directory are DEPRECATED and do not work when pasted directly into FileMaker.**

## ✅ Working Solution: FMScriptGen Tool

Use the Swift command-line tool instead:

```bash
cd ../../tools/FMScriptGen

# Build (first time only)
swift build -c release

# Generate scripts
.build/release/fmscriptgen test1   # Copy to clipboard
.build/release/fmscriptgen test2
.build/release/fmscriptgen clear
.build/release/fmscriptgen runall

# Then paste in FileMaker (Cmd+V)
```

## 📖 Full Documentation

See: `../../tools/FMScriptGen/USAGE.md`

## 🔧 Quick Install

```bash
cd ../../tools/FMScriptGen
./install.sh

# Then use from anywhere:
fmscriptgen test1
```

## 📂 Files in This Directory

- **\*.xml** - Reference files (DO NOT USE for import)
- **HOW_TO_IMPORT.txt** - OUTDATED (kept for reference)
- **CHEAT_SHEET.txt** - OUTDATED (kept for reference)
- **README.md** - This file (current instructions)

## 🎯 Current Workflow

1. Generate script with tool: `fmscriptgen test1`
2. Open FileMaker Script Workspace (Cmd+Shift+S)
3. Create new script with appropriate name
4. Paste (Cmd+V)
5. Save
6. Done!

## ✨ Why This Works

The Swift tool:
- Generates proper fmxmlsnippet format
- Uses correct pasteboard type (`dyn.ah62d4rv4gk8zuxnxnq`)
- Creates compact XML without whitespace
- Matches FileMaker's exact format requirements

The XML files in this directory were an earlier attempt that don't have
the correct format for FileMaker to recognize them.

## 🚀 Available Scripts

Run `fmscriptgen list` to see all available scripts.

Currently includes:
- Test 1: Success Case - Valid PDF
- Test 2: Error - Empty Container
- Clear Test Results
- Run All Tests

---

**For the working solution, always use the FMScriptGen tool!**

Location: `../../tools/FMScriptGen/`
