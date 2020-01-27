#!/bin/bash
function sedstr {
# From stackoverflow.com/a/29626460/633251
	old="$1"
	new="$2"
	file="${3:--}"
	escOld=$(sed 's/[^^]/[&]/g; s/\^/\\^/g' <<< "$old")
	escNew=$(sed 's/[&/\]/\\&/g' <<< "$new")
	#echo $escOld
	#echo $escNew
	sed -i.tmp "s/$escOld/$escNew/g" "$file"
	#echo "sedstr done"
}
