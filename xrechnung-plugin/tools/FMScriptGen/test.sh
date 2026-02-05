#!/bin/bash
# Test script generator

cd "$(dirname "$0")"

# Build
swift build -c release

echo "Testing simple step generation..."

# Create a test that generates just one Set Variable step
cat > /tmp/test_single_step.swift << 'EOF'
import AppKit
import Foundation

let xml = "<fmxmlsnippet type=\"FMObjectList\"><Step enable=\"True\" id=\"141\" name=\"Set Variable\"><Value><Calculation><![CDATA[\"Hello FileMaker\"]]></Calculation></Value><Repetition><Calculation><![CDATA[1]]></Calculation></Repetition><Name>\$greeting</Name></Step></fmxmlsnippet>"

let pasteboard = NSPasteboard.general
pasteboard.clearContents()

let data = xml.data(using: .utf8)!
let dynamicType = NSPasteboard.PasteboardType("dyn.ah62d4rv4gk8zuxnxnq")
pasteboard.setData(data, forType: dynamicType)
pasteboard.setData(data, forType: .string)

print("✓ Copied to clipboard!")
print("Now paste in FileMaker Script Workspace (Cmd+V)")
print("\nGenerated XML:")
print(xml)
EOF

swiftc /tmp/test_single_step.swift -o /tmp/test_single_step
/tmp/test_single_step
