#!/bin/sh

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + Example build script for OS X                               +
# +                                                             +
# + Change the following assignments to adapt them to           +
# + your local settings:                                        +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# The directory where the DLL will be built
build_dir="/tmp"

# The FileMaker extension directory
target_dir="/Applications/FileMaker Pro 18 Advanced/Extensions"

# The delivery script
script="${HOME}/Lisp/fm-plugin-tools/deliver.lisp"

# The LispWorks executable
lispworks="/Applications/LispWorks 8.0 (64-bit)/LispWorks (64-bit).app/Contents/MacOS/lispworks-8-0-0-macos64-universal"

# The name of the plug-in (the .fmplugin file)
name=FMPLisp

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + Don't change anything below this point.                     +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cd "$build_dir"
if [ $? -eq 0 ]
then
  "$lispworks" -build "$script" "$name" "$build_dir"
  if [ $? -eq 0 ]
  then
    rm -rf "$target_dir/$name.fmplugin"
    mv "$build_dir/$name.app" "$target_dir/$name.fmplugin"
   if [ $? -ne 0 ]
   then
      echo "Couldn't copy the bundle to $target_dir."
   fi
  else
    echo "Couldn't build the loadable bundle."
  fi
else
  echo "Couldn't change to directory $build_dir."
fi
