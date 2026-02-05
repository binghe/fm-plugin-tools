#!/usr/bin/env swift

import AppKit
import Foundation

print("🔍 Clipboard Inspector\n")
print("======================\n")

let pasteboard = NSPasteboard.general

print("Available types on clipboard:")
print("------------------------------")

if let types = pasteboard.types {
    for (index, type) in types.enumerated() {
        print("\(index + 1). \(type.rawValue)")

        // Try to read data for this type
        if let data = pasteboard.data(forType: type) {
            print("   Size: \(data.count) bytes")

            // If it's a reasonable size, show first 200 bytes as string
            if data.count < 10000 {
                if let string = String(data: data, encoding: .utf8) {
                    let preview = string.prefix(200)
                    print("   Preview: \(preview)...")
                } else if let string = String(data: data, encoding: .utf16) {
                    let preview = string.prefix(200)
                    print("   Preview (UTF-16): \(preview)...")
                } else {
                    // Show hex dump
                    let hexString = data.prefix(50).map { String(format: "%02x", $0) }.joined(separator: " ")
                    print("   Hex: \(hexString)...")
                }

                // Save to file
                let filename = "/tmp/clipboard_type_\(index + 1).dat"
                try? data.write(to: URL(fileURLWithPath: filename))
                print("   Saved to: \(filename)")
            }
        }
        print()
    }
} else {
    print("No types found on clipboard!")
}

print("\n💡 Instructions:")
print("1. Copy a FileMaker script step (just ONE step)")
print("2. Run this tool again")
print("3. Look for FileMaker-specific types")
print("4. Check the saved files in /tmp/")
