# XRechnung Plugin - Example Data & Tools

This directory contains test data and tools for the XRechnung FileMaker plugin.

## 📁 Directory Structure

```
example_data/
├── README.md (this file)
├── example-invoice.json          # Sample invoice data
├── in.pdf                         # Test input PDF
├── out.pdf                        # Generated PDF/A output
├── out.xml                        # Generated XRechnung XML
├── plutest.fmp12                  # FileMaker test database
│
├── scripts/                       # ⚠️ OUTDATED - XML files don't work
│   └── README.md                  # Read this first!
│
└── xml_structure_of_plutest/     # FileMaker DDR export
    └── ddr/
        └── plutest_fmp12.xml      # Database structure
```

## 🚀 Getting Started

### For Testing the Plugin

1. **Open the test database:**
   ```bash
   open plutest.fmp12
   ```

2. **Generate test scripts using FMScriptGen:**
   ```bash
   cd ../tools/FMScriptGen
   swift build -c release
   .build/release/fmscriptgen test1
   # Paste in FileMaker Script Workspace
   ```

3. **Run tests:**
   - In FileMaker: Scripts > "Run All Tests"
   - Check results in `script_result` and `test_log` fields

### For Development

- **Plugin source:** `../` (parent directory)
- **Build script:** `../build-plugin64.command`
- **Test tool:** `../tools/FMScriptGen/`

## 🛠️ Tools

### FMScriptGen (✅ WORKING!)

Swift command-line tool that generates FileMaker script steps.

**Location:** `../tools/FMScriptGen/`

**Usage:**
```bash
fmscriptgen test1    # Generate Test 1
fmscriptgen test2    # Generate Test 2
fmscriptgen clear    # Generate Clear Results
fmscriptgen runall   # Generate Run All Tests
```

**Installation:**
```bash
cd ../tools/FMScriptGen
./install.sh
```

See: `../tools/FMScriptGen/USAGE.md` for details.

### Deprecated: XML Script Files (❌ DON'T USE)

The XML files in `scripts/` directory were an early attempt but don't work.
Use FMScriptGen instead!

## 📊 Test Files

- **example-invoice.json** - Sample invoice for XRechnung generation
- **in.pdf** - Input PDF for PDF/A conversion testing
- **out.pdf** - Expected PDF/A output
- **out.xml** - Expected XRechnung XML output
- **plutest.fmp12** - FileMaker database with test layouts

## 🧪 Running Tests

### Quick Test

```bash
# 1. Generate and install test scripts
cd ../tools/FMScriptGen
./install.sh

# 2. Generate all scripts
fmscriptgen test1  # Paste as "Test 1: Success Case - Valid PDF"
fmscriptgen test2  # Paste as "Test 2: Error - Empty Container"
fmscriptgen clear  # Paste as "Clear Test Results"
fmscriptgen runall # Paste as "Run All Tests"

# 3. In FileMaker:
#    - Open plutest.fmp12
#    - Put a PDF in the 'plu' container field
#    - Scripts > "Run All Tests"
#    - Check results
```

### Expected Results

After running tests, you should see:
- **Total Tests:** 2 (or more as you add tests)
- **Passed:** 2
- **Failed:** 0
- **Success Rate:** 100%

Results appear in:
- `script_result` field - Latest JSON response
- `test_log` field - Complete test history

## 📖 Documentation

- **Plugin Functions:** See `../README.md` (parent directory)
- **FMScriptGen Tool:** See `../tools/FMScriptGen/USAGE.md`
- **Test Scripts:** See `scripts/README.md` (⚠️ outdated info)

## 🔧 Troubleshooting

### Scripts don't paste in FileMaker
- Make sure you're using FMScriptGen, not the XML files
- Run: `fmscriptgen test1` and immediately paste

### Plugin functions not found
- Rebuild plugin: `cd .. && ./build-plugin64.command`
- Restart FileMaker
- Check plugin loaded: File > Manage > Plugins

### Tests fail
- Check Ghostscript installed: `which gs`
- Check paths in `path_to_example_data_g` field
- Check PDF in `plu` container field

## 📝 Notes

- The old XML script files don't work - use FMScriptGen!
- FMScriptGen uses the correct FileMaker clipboard format
- Tests require Ghostscript for PDF/A conversion

---

**Ready to test?** Start with FMScriptGen! 🚀
