# fm-plugin-tools (by Edi Weitz)

A toolset which allows creation of FileMaker plugins based on Common Lisp

## Preparing `fli.lisp`

The needed FileMaker SDK headers (`fm_plugin_sdk_ver.zip`) can be
downloaded from
[FileMaker Plug-in Support](https://www.filemaker.com/support/technologies/). (The
plugin made from latest SDK headers also work with old versions of FileMaker Pro.)

NOTE: When building 32-bit plugins, the use of 64-bit integers in recent FMP API may require LispWorks (32-bit) version at least 7.0 [1].

[1] http://www.lispworks.com/documentation/lw70/RNIG/html/readme-143.htm#pgfId-972771
