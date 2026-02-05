import Foundation

struct ScriptParser {

    static func parseScriptFile(_ filepath: String) throws -> [String] {
        let content = try String(contentsOfFile: filepath, encoding: .utf8)
        var steps: [String] = []

        for line in content.components(separatedBy: .newlines) {
            let trimmed = line.trimmingCharacters(in: .whitespaces)

            // Skip empty lines and comments
            if trimmed.isEmpty || trimmed.hasPrefix("#") {
                continue
            }

            // Parse the line and generate FM XML
            if let step = parseStep(trimmed) {
                steps.append(step)
            }
        }

        return steps
    }

    private static func parseStep(_ line: String) -> String? {
        // SetVariable: $name = value
        if line.hasPrefix("SetVariable:") {
            return parseSetVariable(line)
        }

        // SetField: table::field = calculation
        if line.hasPrefix("SetField:") {
            return parseSetField(line)
        }

        // If: condition
        if line.hasPrefix("If:") {
            let condition = line.replacingOccurrences(of: "If:", with: "").trimmingCharacters(in: .whitespaces)
            return FMScriptGenerator.ifStatement(condition: condition)
        }

        // Else
        if line == "Else" {
            return FMScriptGenerator.elseStatement()
        }

        // EndIf
        if line == "EndIf" {
            return FMScriptGenerator.endIfStatement()
        }

        // Exit
        if line == "Exit" {
            return FMScriptGenerator.exitScript()
        }

        // ShowDialog: "title" "message"
        if line.hasPrefix("ShowDialog:") {
            return parseShowDialog(line)
        }

        // PerformScript: "script name"
        if line.hasPrefix("PerformScript:") {
            let scriptName = line.replacingOccurrences(of: "PerformScript:", with: "")
                .trimmingCharacters(in: .whitespaces)
                .trimmingCharacters(in: CharacterSet(charactersIn: "\""))
            return FMScriptGenerator.performScript(scriptName: scriptName)
        }

        return nil
    }

    private static func parseSetVariable(_ line: String) -> String? {
        // SetVariable: $name = value
        let content = line.replacingOccurrences(of: "SetVariable:", with: "").trimmingCharacters(in: .whitespaces)

        guard let equalsIndex = content.firstIndex(of: "=") else {
            return nil
        }

        let name = content[..<equalsIndex].trimmingCharacters(in: .whitespaces)
        let value = content[content.index(after: equalsIndex)...].trimmingCharacters(in: .whitespaces)

        return FMScriptGenerator.setVariable(name: name, value: value)
    }

    private static func parseSetField(_ line: String) -> String? {
        // SetField: table::field = calculation
        let content = line.replacingOccurrences(of: "SetField:", with: "").trimmingCharacters(in: .whitespaces)

        guard let equalsIndex = content.firstIndex(of: "=") else {
            return nil
        }

        let fieldRef = content[..<equalsIndex].trimmingCharacters(in: .whitespaces)
        let calculation = content[content.index(after: equalsIndex)...].trimmingCharacters(in: .whitespaces)

        // Parse table::field
        let parts = fieldRef.components(separatedBy: "::")
        guard parts.count == 2 else {
            return nil
        }

        let table = parts[0]
        let field = parts[1]

        // For now, use a placeholder field ID (user will need to fix in FileMaker)
        let fieldId = "0"

        return FMScriptGenerator.setField(table: table, fieldName: field, fieldId: fieldId, calculation: calculation)
    }

    private static func parseShowDialog(_ line: String) -> String? {
        // ShowDialog: "title" "message"
        let content = line.replacingOccurrences(of: "ShowDialog:", with: "").trimmingCharacters(in: .whitespaces)

        // Simple parsing - look for quoted strings
        let pattern = "\"([^\"]*)\""
        guard let regex = try? NSRegularExpression(pattern: pattern) else {
            return nil
        }

        let matches = regex.matches(in: content, range: NSRange(content.startIndex..., in: content))

        guard matches.count >= 2 else {
            return nil
        }

        let title = String(content[Range(matches[0].range(at: 1), in: content)!])
        let message = String(content[Range(matches[1].range(at: 1), in: content)!])

        return FMScriptGenerator.showCustomDialog(title: title, message: message)
    }
}
