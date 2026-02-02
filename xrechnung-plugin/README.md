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

### PDF/A Functions

- `XRec_ConvertToPDFA(pdfData; outputPath; metadata)` - Convert PDF to PDF/A-3b
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

## Building the Plugin

### Prerequisites

- LispWorks (Professional or Enterprise Edition)
- FileMaker Pro Advanced
- Common Lisp libraries: CXML, CL-PPCRE

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

**Current Version**: 0.1.0 (Initial Development)

This is an initial scaffold. The following features need implementation:

### TODO

1. **PDF/A Conversion**
   - Integrate PDF library (e.g., cl-pdf or external tool)
   - Implement PDF/A-3b conversion
   - Add ICC color profile embedding
   - Implement font embedding validation

2. **XML Generation**
   - Implement CII (Cross Industry Invoice) XML generation
   - Add EN 16931 compliance
   - Support all X-Rechnung business rules

3. **Validation**
   - Integrate XSD schema validation
   - Implement Schematron rules checking
   - Add business rule validation (BR-DE-XX)

4. **PDF Attachment**
   - Implement XML embedding in PDF/A
   - Set correct attachment metadata
   - Support both factur-x.xml and zugferd-invoice.xml naming

5. **JSON Parsing**
   - Add proper JSON library integration
   - Implement robust data parsing

## License

BSD License

Copyright (c) 2026, Jens Teich. All rights reserved.

## Contributing

This plugin is part of the fm-plugin-tools project. See the main project README for contribution guidelines.
