#!/usr/bin/env swift

import AppKit
import Foundation

// ============================================================================
// FileMaker Script Generator
// Generates FileMaker script steps and copies them to clipboard
// ============================================================================

struct FMScriptGenerator {

    // MARK: - Script Step Builders

    static func setVariable(name: String, value: String, repetition: String = "1") -> String {
        "<Step enable=\"True\" id=\"141\" name=\"Set Variable\"><Value><Calculation><![CDATA[\(value)]]></Calculation></Value><Repetition><Calculation><![CDATA[\(repetition)]]></Calculation></Repetition><Name>\(name)</Name></Step>"
    }

    static func setField(table: String, fieldName: String, fieldId: String, calculation: String) -> String {
        """
        <Step enable="True" id="76" name="Set Field"><Calculation><![CDATA[\(calculation)]]></Calculation><Field table="\(table)" id="\(fieldId)" name="\(fieldName)"></Field></Step>
        """
    }

    static func ifStatement(condition: String) -> String {
        "<Step enable=\"True\" id=\"68\" name=\"If\"><Calculation><![CDATA[\(condition)]]></Calculation></Step>"
    }

    static func elseStatement() -> String {
        "<Step enable=\"True\" id=\"70\" name=\"Else\"></Step>"
    }

    static func endIfStatement() -> String {
        "<Step enable=\"True\" id=\"69\" name=\"End If\"></Step>"
    }

    static func exitScript() -> String {
        "<Step enable=\"True\" id=\"73\" name=\"Exit Script\"><Result/></Step>"
    }

    static func showCustomDialog(title: String, message: String, buttons: String = "OK") -> String {
        "<Step enable=\"True\" id=\"105\" name=\"Show Custom Dialog\"><Title><Calculation><![CDATA[\(title)]]></Calculation></Title><Message><Calculation><![CDATA[\(message)]]></Calculation></Message><Buttons><Calculation><![CDATA[\"\(buttons)\"]]></Calculation></Buttons></Step>"
    }

    static func performScript(scriptName: String) -> String {
        "<Step enable=\"True\" id=\"1\" name=\"Perform Script\"><Calculation/><Script name=\"\(scriptName)\"/></Step>"
    }

    // MARK: - Complete Script Builders

    static func generateClearResults() -> String {
        let steps = [
            showCustomDialog(title: "Clear Test Results", message: "Clear all test logs and results?", buttons: "Clear; Cancel"),
            ifStatement(condition: "Get(LastMessageChoice) = 1"),
            setField(table: "plutest", fieldName: "script_result", fieldId: "18", calculation: "\"\""),
            setField(table: "plutest", fieldName: "test_log", fieldId: "21", calculation: "\"\""),
            setField(table: "plutest", fieldName: "test_status", fieldId: "22", calculation: "\"\""),
            setVariable(name: "$$testResults", value: "\"\""),
            setVariable(name: "$$testsPassed", value: "0"),
            setVariable(name: "$$testsFailed", value: "0"),
            setVariable(name: "$$testsTotal", value: "0"),
            showCustomDialog(title: "Complete", message: "Test results cleared."),
            endIfStatement()
        ]
        return wrapAsScriptSteps(steps: steps)
    }

    static func generateTestScript1() -> String {
        let steps = [
            setVariable(name: "$testID", value: "\"TEST-001\""),
            setVariable(name: "$testName", value: "\"Success Case - Valid PDF from Container\""),
            setVariable(name: "$$testsTotal", value: "$$testsTotal + 1"),
            setVariable(name: "$outputPath", value: "Get(DesktopPath) & \"test_success_\" & Get(CurrentTimestamp) & \".pdf\""),
            "",
            "<!-- Check if container is empty -->",
            ifStatement(condition: "IsEmpty ( plutest::plu )"),
            setVariable(name: "$result", value: "\"SKIPPED - No PDF in container field\""),
            setVariable(name: "$$testResults", value: "$$testResults & $testID & \" - \" & $testName & \": \" & $result & ¶"),
            exitScript(),
            endIfStatement(),
            "",
            "<!-- Call plugin function -->",
            setField(table: "plutest", fieldName: "script_result", fieldId: "18",
                    calculation: "XRec_ConvertToPDFA ( plutest::plu ; $outputPath )"),
            "",
            "<!-- Parse JSON response -->",
            setVariable(name: "$jsonResponse", value: "plutest::script_result"),
            setVariable(name: "$success", value: "JSONGetElement ( $jsonResponse ; \"success\" )"),
            setVariable(name: "$resultPath", value: "JSONGetElement ( $jsonResponse ; \"result\" )"),
            setVariable(name: "$errorMsg", value: "JSONGetElement ( $jsonResponse ; \"error\" )"),
            "",
            "<!-- Check results -->",
            ifStatement(condition: "$success = \"true\" or $success = true"),
            ifStatement(condition: "not IsEmpty ( $resultPath )"),
            setVariable(name: "$result", value: "\"✓ PASSED\""),
            setVariable(name: "$$testsPassed", value: "$$testsPassed + 1"),
            elseStatement(),
            setVariable(name: "$result", value: "\"✗ FAILED - Empty result path\""),
            setVariable(name: "$$testsFailed", value: "$$testsFailed + 1"),
            endIfStatement(),
            elseStatement(),
            setVariable(name: "$result", value: "\"✗ FAILED - \" & $errorMsg"),
            setVariable(name: "$$testsFailed", value: "$$testsFailed + 1"),
            endIfStatement(),
            "",
            "<!-- Log results -->",
            setVariable(name: "$$testResults", value: "$$testResults & $testID & \" - \" & $testName & \": \" & $result & ¶"),
            setField(table: "plutest", fieldName: "test_log", fieldId: "21",
                    calculation: "plutest::test_log & ¶ & Get(CurrentTimestamp) & \" | \" & $testID & \" | \" & $result & ¶ & \"Response: \" & $jsonResponse & ¶")
        ]

        return wrapAsScriptSteps(steps: steps)
    }

    static func generateTestScript2() -> String {
        let steps = [
            setVariable(name: "$testID", value: "\"TEST-002\""),
            setVariable(name: "$testName", value: "\"Error Handling - Empty Container\""),
            setVariable(name: "$$testsTotal", value: "$$testsTotal + 1"),
            setVariable(name: "$outputPath", value: "Get(DesktopPath) & \"test_empty_\" & Get(CurrentTimestamp) & \".pdf\""),
            setVariable(name: "$emptyData", value: "\"\""),
            setField(table: "plutest", fieldName: "script_result", fieldId: "18",
                    calculation: "XRec_ConvertToPDFA ( $emptyData ; $outputPath )"),
            setVariable(name: "$jsonResponse", value: "plutest::script_result"),
            setVariable(name: "$success", value: "JSONGetElement ( $jsonResponse ; \"success\" )"),
            setVariable(name: "$errorMsg", value: "JSONGetElement ( $jsonResponse ; \"error\" )"),
            ifStatement(condition: "$success = \"false\" or $success = false or $success = 0"),
            ifStatement(condition: "not IsEmpty ( $errorMsg )"),
            setVariable(name: "$result", value: "\"✓ PASSED - Correct error handling\""),
            setVariable(name: "$$testsPassed", value: "$$testsPassed + 1"),
            elseStatement(),
            setVariable(name: "$result", value: "\"✗ FAILED - No error message provided\""),
            setVariable(name: "$$testsFailed", value: "$$testsFailed + 1"),
            endIfStatement(),
            elseStatement(),
            setVariable(name: "$result", value: "\"✗ FAILED - Should have returned error\""),
            setVariable(name: "$$testsFailed", value: "$$testsFailed + 1"),
            endIfStatement(),
            setVariable(name: "$$testResults", value: "$$testResults & $testID & \" - \" & $testName & \": \" & $result & ¶"),
            setField(table: "plutest", fieldName: "test_log", fieldId: "21",
                    calculation: "plutest::test_log & ¶ & Get(CurrentTimestamp) & \" | \" & $testID & \" | \" & $result & ¶ & \"Response: \" & $jsonResponse & ¶")
        ]
        return wrapAsScriptSteps(steps: steps)
    }

    static func generateRunAllTests() -> String {
        let steps = [
            setVariable(name: "$$testResults", value: "\"\""),
            setVariable(name: "$$testsPassed", value: "0"),
            setVariable(name: "$$testsFailed", value: "0"),
            setVariable(name: "$$testsTotal", value: "0"),
            showCustomDialog(title: "PDF/A Test Suite", message: "Running comprehensive tests...¶This may take a moment."),
            performScript(scriptName: "Test 1: Success Case - Valid PDF"),
            performScript(scriptName: "Test 2: Error - Empty Container"),
            setVariable(name: "$report", value: "\"=== PDF/A Test Suite Results ===\" & ¶ & \"Total Tests: \" & $$testsTotal & ¶ & \"Passed: ✓ \" & $$testsPassed & ¶ & \"Failed: ✗ \" & $$testsFailed & ¶ & \"Success Rate: \" & Round($$testsPassed / $$testsTotal * 100; 0) & \"%\" & ¶ & ¶ & \"=== Detailed Results ===\" & ¶ & $$testResults"),
            showCustomDialog(title: "Test Suite Complete", message: "$report")
        ]
        return wrapAsScriptSteps(steps: steps)
    }

    static func wrapInScript(name: String, steps: [String]) -> String {
        let stepsXML = steps
            .filter { !$0.isEmpty && !$0.hasPrefix("<!--") }
            .joined(separator: "")

        // No XML declaration! Just the snippet
        return "<fmxmlsnippet type=\"FMObjectList\">\(stepsXML)</fmxmlsnippet>"
    }

    static func wrapAsScriptSteps(steps: [String]) -> String {
        let stepsXML = steps
            .filter { !$0.isEmpty && !$0.hasPrefix("<!--") }
            .joined(separator: "")

        return "<fmxmlsnippet type=\"FMObjectList\">\(stepsXML)</fmxmlsnippet>"
    }

    // MARK: - Clipboard Functions

    static func copyToClipboard(_ text: String) -> Bool {
        let pasteboard = NSPasteboard.general
        pasteboard.clearContents()

        guard let data = text.data(using: .utf8) else {
            return false
        }

        // Use the exact pasteboard types FileMaker uses
        let dynamicType = NSPasteboard.PasteboardType("dyn.ah62d4rv4gk8zuxnxnq")

        // Set both types (FileMaker uses both)
        pasteboard.setData(data, forType: dynamicType)
        pasteboard.setData(data, forType: .string)  // Also set string type as fallback

        return true
    }
}

// ============================================================================
// MARK: - Main Program
// ============================================================================

func printUsage() {
    print("""

    FileMaker Script Generator
    ==========================

    Usage: fmscriptgen [command]

    Commands:
      test1          Generate Test 1 script and copy to clipboard
      test2          Generate Test 2 script and copy to clipboard
      clear          Generate Clear Results script
      runall         Generate Run All Tests script

      list           List all available scripts
      help           Show this help message

    Example:
      fmscriptgen test1
      # Then paste in FileMaker Script Workspace (Cmd+V)

    """)
}

func main() {
    let args = CommandLine.arguments

    guard args.count > 1 else {
        printUsage()
        return
    }

    let command = args[1].lowercased()

    switch command {
    case "test1":
        print("Generating Test 1 script...")
        let xml = FMScriptGenerator.generateTestScript1()

        if FMScriptGenerator.copyToClipboard(xml) {
            print("✓ Script copied to clipboard!")
            print("Now paste in FileMaker Script Workspace (Cmd+V)")
            print("\nScript size: \(xml.count) bytes")
        } else {
            print("✗ Failed to copy to clipboard")
        }

    case "test2":
        print("Generating Test 2 script...")
        let xml = FMScriptGenerator.generateTestScript2()
        if FMScriptGenerator.copyToClipboard(xml) {
            print("✓ Script copied to clipboard!")
            print("Now paste in FileMaker Script Workspace (Cmd+V)")
        } else {
            print("✗ Failed to copy to clipboard")
        }

    case "clear":
        print("Generating Clear Results script...")
        let xml = FMScriptGenerator.generateClearResults()
        if FMScriptGenerator.copyToClipboard(xml) {
            print("✓ Script copied to clipboard!")
            print("Now paste in FileMaker Script Workspace (Cmd+V)")
        } else {
            print("✗ Failed to copy to clipboard")
        }

    case "runall":
        print("Generating Run All Tests script...")
        let xml = FMScriptGenerator.generateRunAllTests()
        if FMScriptGenerator.copyToClipboard(xml) {
            print("✓ Script copied to clipboard!")
            print("Now paste in FileMaker Script Workspace (Cmd+V)")
        } else {
            print("✗ Failed to copy to clipboard")
        }

    case "list":
        print("""
        Available scripts:
          ✓ test1: Success Case - Valid PDF
          ✓ test2: Error - Empty Container
          ✓ clear: Clear Test Results
          ✓ runall: Run All Tests
        """)

    case "help", "-h", "--help":
        printUsage()

    default:
        print("Unknown command: \(command)")
        print("Run 'fmscriptgen help' for usage")
    }
}

// Run the program
main()
