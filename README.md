# fm-plugin-tools (by Edi Weitz)

A toolset which allows creation of FileMaker plugins based on Common Lisp

## Preparing `fli.lisp`

The needed FileMaker SDK headers (`fm_plugin_sdk_x.zip`) can be
downloaded from
[FileMaker Plug-in Support](https://www.filemaker.com/support/technologies/). (The
latest version also works with old versions of FileMaker Pro and
FileMaker Pro Advanced.

NOTE: I (Chun Tian) fixed several issues in `prepare` code to make sure it translates into buildable `fli.lisp` from recent FM Plugin SDK headers (confirmed working on version 13, 16, 17), but the use of 64-bit integers in recent API may require LispWorks (32-bit) version at least 7.0 [1], unless you're building 64-bit plugins (for FMS).

[1] http://www.lispworks.com/documentation/lw70/RNIG/html/readme-143.htm#pgfId-972771
