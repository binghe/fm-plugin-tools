#!/bin/sh

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# + Build script for XRechnungPlugin                            +
# +                                                             +
# + Change the following assignments to adapt them to           +
# + your local settings:                                        +
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# The directory where the DLL will be built
build_dir="/tmp"

# The FileMaker extension directory
target_dir="${HOME}/Library/Application Support/FileMaker/FileMaker Pro/22.0/Extensions"

# The delivery script
script="${HOME}/quicklisp/local-projects/fm-plugin-tools/xrechnung-plugin/deliver.lisp"

# The LispWorks executable
lispworks="/Applications/LispWorks 8.1 (64-bit)/LispWorks (64-bit).app/Contents/MacOS/lispworks-8-1-0-macos64-universal"

# The name of the plug-in (the .fmplugin file)
name=XRechnungPlugin

# And an identity for codesigning is required since recently
csid="Jens Teich"

# Open FileMaker client as last step with this database file
fmdb="/Users/jens/plutest.fmp12"

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
	mv "$build_dir/$name.fmplugin" "$target_dir/$name.fmplugin"
	if [ $? -eq 0 ]
	then
	    # Sign the internal lwheap file first, then the whole bundle
	    codesign --sign "$csid" --force "$target_dir/$name.fmplugin/Contents/MacOS/Resources/$name.lwheap"
	    if [ $? -eq 0 ]
	    then
		codesign --sign "$csid" --force "$target_dir/$name.fmplugin"
		if [ $? -eq 0 ]
		then
		    echo "Successfully built and codesigned $name.fmplugin"
		    if [ -n "$fmdb" ]
		    then
			open "$fmdb"
		    fi
		else
		    echo "Couldn't codesign bundle $name.fmplugin."
		fi
	    else
		echo "Couldn't codesign $name.lwheap file."
	    fi
	else
	    echo "Couldn't copy the bundle to $target_dir."
	fi
    else
	echo "Couldn't build the loadable bundle."
    fi
else
    echo "Couldn't change to directory $build_dir."
fi
