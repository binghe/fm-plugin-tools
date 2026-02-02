# XRechnungPlugin

A FileMaker plugin for generating X-Rechnung compliant invoices and PDF/A documents.

## Overview

XRechnungPlugin bridges FileMaker's inability to generate PDF/A format documents, which are required for electronic invoicing in Germany and other EU countries. The plugin supports:

- **PDF/A Conversion**: Convert FileMaker-generated PDFs to PDF/A-3b format
- **X-Rechnung XML Generation**: Create valid X-Rechnung/ZUGFeRD XML from FileMaker data
- **ZUGFeRD/Factur-X Hybrid**: Embed invoice XML into PDF/A files for fully compliant hybrid documents
- **Validation**: Validate invoice data and generated documents against X-Rechnung specifications

## Standards Supported

- X-Rechnung 2.3 (EN 16931)
- PDF/A-3b (ISO 19005-3)
- ZUGFeRD 2.3
- Factur-X

## Plugin Functions

### Version Information

- `XRec_Version()` - Returns plugin version
- `XRec_GetXRechnungVersion()` - Returns X-Rechnung standard version
- `XRec_GetPDFALevel()` - Returns PDF/A conformance level
- `XRec_CheckTools()` - Check if Ghostscript and QPDF are available

### PDF/A Functions

- `XRec_ConvertToPDFA(pdfData; outputPath; metadata)` - Convert PDF binary data to PDF/A-3b
- `XRec_ConvertToPDFAFromFile(pdfPath; outputPath; metadata)` - Convert PDF file to PDF/A-3b
- `XRec_ValidatePDFA(pdfPath)` - Validate PDF/A compliance

### X-Rechnung XML Functions

- `XRec_GenerateXRechnungXML(invoiceData; outputPath)` - Generate X-Rechnung XML
- `XRec_ValidateXRechnungXML(xmlPath)` - Validate X-Rechnung XML

### ZUGFeRD/Factur-X Functions

- `XRec_CreateZUGFeRD(pdfPath; xmlPath; outputPath)` - Create hybrid document
- `XRec_ExtractXMLFromZUGFeRD(zugferdPath; outputPath)` - Extract embedded XML

### Validation Functions

- `XRec_ValidateInvoiceData(invoiceData)` - Validate invoice data
- `XRec_ValidateVATID(vatId)` - Validate VAT ID format

### Utility Functions

- `XRec_FormatDateXRechnung(fmDate)` - Format date for X-Rechnung
- `XRec_ClearValidationCache()` - Clear validation cache

## External Tool Requirements

The plugin requires the following external tools for PDF/A conversion and XML embedding:

### Ghostscript (Required for PDF/A conversion)

**macOS:**
```bash
brew install ghostscript
```

**Windows:**
Download and install from https://www.ghostscript.com/download/gsdnld.html

**Linux:**
```bash
sudo apt-get install ghostscript  # Debian/Ubuntu
sudo yum install ghostscript      # RHEL/CentOS
```

### QPDF (Required for XML embedding)

**macOS:**
```bash
brew install qpdf
```

**Windows:**
Download from https://github.com/qpdf/qpdf/releases

**Linux:**
```bash
sudo apt-get install qpdf  # Debian/Ubuntu
sudo yum install qpdf      # RHEL/CentOS
```

### Verification

After installation, verify the tools are available by calling:
```
XRec_CheckTools()
```

This will return a JSON string showing whether Ghostscript and QPDF are found.

## Building the Plugin

### Prerequisites

- LispWorks (Professional or Enterprise Edition)
- FileMaker Pro Advanced
- Common Lisp libraries: CXML, CL-PPCRE
- Ghostscript (runtime dependency)
- QPDF (runtime dependency)

### macOS

1. Edit `build-plugin64.command` to set correct paths
2. Run: `./build-plugin64.command`

### Windows

1. Edit `build-plugin64.cmd` to set correct paths
2. Run: `build-plugin64.cmd`

## Installation

Copy the `XRechnungPlugin.fmplugin` bundle to:

- **macOS**: `/Library/FileMaker Server/Database Server/Extensions/` or `~/Library/Application Support/FileMaker/Extensions/`
- **Windows**: `C:\Program Files\FileMaker\FileMaker Server\Database Server\Extensions\`

Restart FileMaker Pro/Server after installation.

## Configuration

Access plugin configuration through FileMaker Preferences > Plug-Ins > XRechnungPlugin > Configure.

Options:
- Enable/disable validation caching
- Clear validation cache
- View supported standards versions

## Usage Examples

### Example 1: Convert FileMaker PDF to PDF/A

```filemaker
# Store PDF from container field to temporary location
Set Variable [ $pdfPath ; Value: Get(TemporaryPath) & "invoice.pdf" ]
Export Field Contents [ Invoices::PDF_Container ; $pdfPath ]

# Convert to PDF/A-3b
Set Variable [ $outputPath ; Value: Get(TemporaryPath) & "invoice-pdfa.pdf" ]
Set Variable [ $result ; Value: XRec_ConvertToPDFAFromFile( $pdfPath ; $outputPath ) ]

# Check if conversion succeeded
If [ Left($result ; 5) = "Error" ]
    Show Custom Dialog [ "Conversion Failed" ; $result ]
Else
    # Import the PDF/A back to FileMaker
    Insert File [ Invoices::PDFA_Container ; $result ]
End If
```

### Example 2: Create ZUGFeRD Hybrid Document

```filemaker
# Assume you have a PDF/A file and an X-Rechnung XML file
Set Variable [ $pdfPath ; Value: Invoices::PDFA_Path ]
Set Variable [ $xmlPath ; Value: Invoices::XML_Path ]
Set Variable [ $outputPath ; Value: Get(TemporaryPath) & "zugferd-invoice.pdf" ]

# Create hybrid document
Set Variable [ $result ; Value: XRec_CreateZUGFeRD( $pdfPath ; $xmlPath ; $outputPath ) ]

If [ Left($result ; 5) ≠ "Error" ]
    # Success - store the hybrid document
    Insert File [ Invoices::ZUGFeRD_Container ; $result ]
Else
    Show Custom Dialog [ "Error" ; $result ]
End If
```

### Example 3: Validate PDF/A Compliance

```filemaker
Set Variable [ $pdfPath ; Value: Get(TemporaryPath) & "test.pdf" ]
Export Field Contents [ Invoices::PDF_Container ; $pdfPath ]

Set Variable [ $isValid ; Value: XRec_ValidatePDFA( $pdfPath ) ]

If [ Left($isValid ; 1) = "1" ]
    Show Custom Dialog [ "Valid PDF/A-3b" ]
Else
    Show Custom Dialog [ "Not PDF/A Compliant" ; $isValid ]
End If
```

### Example 4: Check Tool Availability

```filemaker
# Check if required tools are installed
Set Variable [ $toolStatus ; Value: XRec_CheckTools() ]

# Parse JSON result (requires JSONGetElement or similar)
Set Variable [ $gsAvailable ; Value: JSONGetElement( $toolStatus ; "ghostscript" ) ]
Set Variable [ $qpdfAvailable ; Value: JSONGetElement( $toolStatus ; "qpdf" ) ]

If [ $gsAvailable = "false" or $qpdfAvailable = "false" ]
    Show Custom Dialog [ "Missing Tools" ; "Please install Ghostscript and QPDF" ]
End If
```

### Example 5: Complete Workflow

```filemaker
# 1. Check tools
Set Variable [ $toolCheck ; Value: XRec_CheckTools() ]
If [ PatternCount($toolCheck ; "false") > 0 ]
    Exit Script [ Text Result: "Missing required tools" ]
End If

# 2. Export FileMaker PDF
Set Variable [ $tempPDF ; Value: Get(TemporaryPath) & "fm-invoice.pdf" ]
Export Field Contents [ Invoices::PDF_Container ; $tempPDF ]

# 3. Convert to PDF/A
Set Variable [ $pdfaPath ; Value: Get(TemporaryPath) & "invoice-pdfa.pdf" ]
Set Variable [ $pdfaResult ; Value: XRec_ConvertToPDFAFromFile( $tempPDF ; $pdfaPath ) ]

If [ Left($pdfaResult ; 5) = "Error" ]
    Exit Script [ Text Result: $pdfaResult ]
End If

# 4. Generate X-Rechnung XML (TODO: implement XML generation)
# Set Variable [ $xmlPath ; Value: ... ]

# 5. Create ZUGFeRD hybrid
Set Variable [ $finalPath ; Value: Get(TemporaryPath) & "zugferd-final.pdf" ]
Set Variable [ $zugferdResult ; Value: XRec_CreateZUGFeRD( $pdfaPath ; $xmlPath ; $finalPath ) ]

If [ Left($zugferdResult ; 5) ≠ "Error" ]
    # Store final document
    Insert File [ Invoices::Final_Container ; $finalPath ]
    Show Custom Dialog [ "Success" ; "ZUGFeRD invoice created successfully" ]
End If
```

## Invoice Data Format

The plugin expects invoice data as JSON strings with the following structure:

```json
{
  "invoiceNumber": "RE-2026-001",
  "invoiceDate": "2026-02-02",
  "seller": {
    "name": "Company Name",
    "address": "Street 1, 12345 City",
    "vatId": "DE123456789",
    "taxNumber": "12/345/67890"
  },
  "buyer": {
    "name": "Customer Name",
    "address": "Street 2, 54321 City"
  },
  "lineItems": [
    {
      "description": "Service/Product",
      "quantity": 1,
      "unitPrice": 100.00,
      "vatRate": 19.0
    }
  ],
  "currency": "EUR"
}
```

## Development Status

**Current Version**: 0.1.0 (Active Development)

### Implemented Features ✅

1. **PDF/A Conversion** ✅
   - Ghostscript integration for PDF/A-3b conversion
   - Binary data and file path input support
   - Basic PDF/A validation
   - Error logging and reporting

2. **PDF Attachment** ✅
   - QPDF integration for XML embedding
   - Factur-X compliant attachment metadata
   - XML extraction from hybrid documents

3. **Tool Management** ✅
   - Automatic tool detection (Ghostscript, QPDF)
   - Cross-platform path resolution
   - Configuration caching

### TODO

1. **XML Generation** (Next Priority)
   - Implement CII (Cross Industry Invoice) XML generation
   - Add EN 16931 compliance
   - Support all X-Rechnung business rules
   - Generate proper namespaces and schema references

2. **Enhanced Validation**
   - Integrate veraPDF for comprehensive PDF/A validation
   - XSD schema validation for X-Rechnung XML
   - Schematron rules checking
   - Business rule validation (BR-DE-XX)

3. **JSON Parsing**
   - Add proper JSON library integration (e.g., com.gigamonkeys.json)
   - Implement robust data parsing for invoice data

4. **Metadata Support**
   - Parse and apply PDF metadata from JSON input
   - Support XMP metadata for PDF/A

5. **ICC Profile Embedding**
   - Bundle or reference standard ICC color profiles
   - Ensure PDF/A-3b color space compliance

## License

BSD License

Copyright (c) 2026, Jens Teich. All rights reserved.

## Contributing

This plugin is part of the fm-plugin-tools project. See the main project README for contribution guidelines.
