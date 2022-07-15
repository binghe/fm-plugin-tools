# fm-plugin-tools (by Edi Weitz)

A toolset which allows creation of FileMaker plugins based on Common Lisp

## Preparing `fli.lisp`

The needed FileMaker SDK headers (`fm_plugin_sdk_ver.zip`) can be
downloaded from
[FileMaker Plug-in Support](https://www.claris.com/resources/downloads/). (The
plugin headers from latest SDK headers should also work for old versions of FileMaker Pro
and FileMaker Pro Advanced.)

## Further processing of FileMaker SDK header files

Please do this on the same platform when you are building your FM plugins:

1. Open a terminal window and go to the directory `PlugInSDK/Headers/FMWrapper`.
2. Execute `for i in *.h; do sed 's/^#include.*//' $i > ${i}h; done`.
3. Execute `for i in *.hh; do gcc -E $i > ${i}h; done`.

At the end, 10 files like `FMXExtern.hhh` are generated and will be used by
the `prepare-fm-plugin-tools` package.
