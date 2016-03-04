#!/bin/bash
##
## optimize database
## 
## depends: mysql
##

DBHost="$1"
DBPort='3306'
DBUser="$2"
DBName="$3"

if [ -z "$DBHost" ] || [ -z "$DBUser" ] || [ -z "$DBName" ]; then
	echo "usage: ./repaire_database.sh {DBHost} {DBUser} {DBName}"
	exit
fi

## password
read -s -p "DBPassword: " DBPassword
echo ""

mysqlcheck -h "$DBHost" -P "$DBPort" -u "$DBUser" --password="$DBPassword" --check --databases "$DBName"
mysqlcheck -h "$DBHost" -P "$DBPort" -u "$DBUser" --password="$DBPassword" --optimize --databases "$DBName"
mysqlcheck -h "$DBHost" -P "$DBPort" -u "$DBUser" --password="$DBPassword" --analyze --databases "$DBName"
