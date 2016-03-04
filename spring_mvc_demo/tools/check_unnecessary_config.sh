#!/bin/bash
##
## check unnecessary config
##

TargetDirPath="$(dirname "$0" | xargs readlink -f | xargs dirname)"
ConfigFilePath="$TargetDirPath"/src/main/webapp/WEB-INF/config.properties
CheckPath="$TargetDirPath"/src

## check
cat "$ConfigFilePath" | while read -r lineStr; do
	ConfigKey=$(expr "$lineStr" : '^\([^=]\+\)=')
	
	if [ -z "$ConfigKey" ]; then
		continue
	fi
	
	## search
	MatchedCount=$(grep -r "$ConfigKey" "$CheckPath" | wc -l)
	if [ $MatchedCount -lt 3 ]; then
		echo "$ConfigKey"
	fi
done
