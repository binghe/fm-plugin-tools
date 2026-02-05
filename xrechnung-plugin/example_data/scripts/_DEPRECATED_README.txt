═══════════════════════════════════════════════════════════════════
  ⚠️  DEPRECATED: These XML files DO NOT WORK
═══════════════════════════════════════════════════════════════════

The XML files in this directory (Test*.xml) were an early attempt
at creating importable FileMaker scripts. They do NOT work when
pasted into FileMaker Script Workspace.

WHY THEY DON'T WORK:
--------------------
1. Wrong XML format (too verbose, includes <?xml declaration)
2. Field IDs are database-specific and may not match
3. Missing proper pasteboard type registration

═══════════════════════════════════════════════════════════════════
  ✅  USE THE WORKING SOLUTION INSTEAD
═══════════════════════════════════════════════════════════════════

Location: ../../tools/FMScriptGen/

Usage:
  cd ../../tools/FMScriptGen
  swift build -c release
  .build/release/fmscriptgen test1
  # Then paste in FileMaker (Cmd+V)

Or install system-wide:
  cd ../../tools/FMScriptGen
  ./install.sh
  fmscriptgen test1

═══════════════════════════════════════════════════════════════════
  📂 FILES KEPT FOR REFERENCE ONLY
═══════════════════════════════════════════════════════════════════

The following files are kept for reference but should NOT be used:

✗ Test1_Success_Case.xml          - DO NOT USE
✗ Test2_Empty_Container.xml       - DO NOT USE
✗ Test3_Invalid_Path.xml          - DO NOT USE
✗ Test4_File_Not_Found.xml        - DO NOT USE
✗ Test5_File_Path.xml             - DO NOT USE
✗ Test6_With_Metadata.xml         - DO NOT USE
✗ Run_All_Tests.xml               - DO NOT USE
✗ Clear_Test_Results.xml          - DO NOT USE

✗ HOW_TO_IMPORT.txt               - OUTDATED INSTRUCTIONS
✗ CHEAT_SHEET.txt                 - OUTDATED INSTRUCTIONS

═══════════════════════════════════════════════════════════════════

These files represent research into FileMaker's XML format and are
kept for historical/reference purposes. The final working solution
was achieved by reverse-engineering the actual clipboard format
FileMaker uses, which led to the FMScriptGen Swift tool.

═══════════════════════════════════════════════════════════════════
