#!/bin/bash
##
## upgrade
##

NewVersion="$1"
SourceWarPath="$2"
TargetDirPath="$(dirname "$0" | xargs readlink -f | xargs dirname | xargs dirname)"

if [ -z "$JETTY_HOME" ]; then
	echo "JETTY_HOME undefined!"
	exit
elif [ -z "$NewVersion" ] || [ -z "$SourceWarPath" ]; then
	echo "usage: sh upgrade.sh {NewVersion} {SourceWarPath}"
	exit
elif [ ! -e "$SourceWarPath" ]; then
	echo " Source War File do not exist!"
	exit
fi

## create temp dir
Now=$(date +"%Y%m%d_%H%M%S_%N")
WorkTempDir="$TargetDirPath"/temp_"$Now"
mkdir -p "$WorkTempDir"

## copy config files
echo 'copy config files...'
cp "$TargetDirPath"/WEB-INF/*.properties "$WorkTempDir"/
cp "$TargetDirPath"/WEB-INF/classes/log4j2.xml "$WorkTempDir"/

## clean old files
echo 'clean old files...'
rm -rf "$TargetDirPath"/WEB-INF

## copy new files
echo 'copy new files...'
unzip -o "$SourceWarPath" -d "$TargetDirPath"

mv "$WorkTempDir"/*.properties "$TargetDirPath"/WEB-INF/
mv "$WorkTempDir"/log4j2.xml "$TargetDirPath"/WEB-INF/classes/

echo "$NewVersion" > "$TargetDirPath"/VERSION

## clean temp dir
echo 'clean temp dir...'
rm -rf "$WorkTempDir"

## startup
sleep 1
echo 'starting...'
cd "$JETTY_HOME"
"$JETTY_HOME"/bin/jetty.sh stop
sleep 1
"$JETTY_HOME"/bin/jetty.sh start
