#!/bin/sh

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + Build script adapted from FM-PLUGIN-TOOLS example           +
# +                                                             +
# + Change the following assignments to adapt them to           +
# + your local settings:                                        +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# The directory where the DLL will be built
build_dir="/tmp"

# The FileMaker extension directory
target_dir="/Applications/FileMaker Pro 18 Advanced/Extensions"

# The delivery script
script="${HOME}/Lisp/fm-plugin-tools/regex-plugin/deliver.lisp"

# The LispWorks executable
lispworks="/Applications/LispWorks 8.0 (64-bit)/LispWorks (64-bit).app/Contents/MacOS/lispworks-8-0-0-macos64-universal"

# The name of the plug-in (the .fmplugin file)
name=RegexPlugIn

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + Don't change anything below this point.                     +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cd "$build_dir"
if [ $? -eq 0 ]
then
  "$lispworks" -build "$script" "$name" "$build_dir"
  if [ $? -eq 0 ]
  then
    echo "Build the loadable bundle successfully."
  else
    echo "Couldn't build the loadable bundle."
  fi
else
  echo "Couldn't change to directory $build_dir."
fi
