# Script Definition Format

Simple text format for defining FileMaker scripts.

## Basic Syntax

```
# Comments start with #
SetVariable: $name = value
SetField: table::field = calculation
If: condition
Else
EndIf
Exit
ShowDialog: "title" "message"
PerformScript: "script name"
```

## Examples

### Simple Script

```
# Set a variable and show it
SetVariable: $greeting = "Hello World"
ShowDialog: "Greeting" $greeting
```

### Conditional Logic

```
SetVariable: $value = Get(CurrentTime)

If: IsEmpty($value)
  ShowDialog: "Error" "Value is empty"
  Exit
EndIf

SetVariable: $result = "Success"
```

### Complex Script (Test Case)

```
# Test script for PDF conversion
SetVariable: $testID = "TEST-001"
SetVariable: $outputPath = Get(DesktopPath) & "output.pdf"

# Check preconditions
If: IsEmpty ( myTable::pdfField )
  SetVariable: $result = "SKIPPED - No PDF found"
  Exit
EndIf

# Call plugin
SetField: myTable::result = MyPlugin_Function ( myTable::pdfField ; $outputPath )

# Parse response
SetVariable: $response = myTable::result
SetVariable: $success = JSONGetElement ( $response ; "success" )

If: $success = "true"
  ShowDialog: "Success" "Conversion complete"
Else
  SetVariable: $error = JSONGetElement ( $response ; "error" )
  ShowDialog: "Error" $error
EndIf
```

## Statement Reference

### SetVariable
```
SetVariable: $variableName = calculation
```
Creates a Set Variable script step.

### SetField
```
SetField: tableName::fieldName = calculation
```
Creates a Set Field script step.
**Note**: Field ID will be 0 - you may need to fix field reference in FileMaker.

### If / Else / EndIf
```
If: condition
  # steps if true
Else
  # steps if false
EndIf
```
Creates conditional logic.

### Exit
```
Exit
```
Creates an Exit Script step.

### ShowDialog
```
ShowDialog: "Title" "Message"
```
Creates a Show Custom Dialog step with title and message.

### PerformScript
```
PerformScript: "Script Name"
```
Creates a Perform Script step.

## Tips

- Use meaningful variable names with $ prefix
- Add comments with # to document your logic
- Break complex scripts into smaller reusable scripts
- Test simple scripts first before building complex ones

## Usage

```bash
# From any file
fmscriptgen /path/to/myscript.txt

# From script-definitions folder (shortcut)
fmscriptgen test1    # Uses script-definitions/test1.txt

# Then paste in FileMaker (Cmd+V)
```

## Future Enhancements

Potential additions to the format:
- Loop constructs
- Case statements
- Comments in XML output
- Variable type hints
- Field ID mapping file
