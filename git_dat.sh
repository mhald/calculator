#!/bin/bash
## Bash script that generates popcorn mapping files to link to module:linenumber
##
## Assumptions
##   - all deps are git projects

## Sample payload:
## {
##   "sender":   {"role": "calculator", "version": "1.0.7"},
##   "vcs":      {"flavor": "git", "base_url": "https://github.com/mhald/calculator/", "checksum": "50f92e0b7eb3a54b556c302ff63793cfb947238e"},
##   "mapping":  {
##      "calculator.erl": "http://github.com/mhald/blob/50f92e0b7eb3a54b556c302ff63793cfb947238e/calculator/apps/calculator/src/calculator.erl",
##      ...
##   }
## }

BUILD=$1
ORG='mhald'
REPO='calculator'
CSUM=`git log | head -1 | sed -e 's/.* //'`

echo '{'
echo '   "sender": {"role": "Calculator", "version": "1.0.7"},' ## make version the variable
echo '   "vcs":    {"flavor": "git", "base_url": "https://github.com/mhald/calculator/", "checksum": "'$CSUM'"}, ' ## make checksum 
echo '   "mapping": {'


find apps -name "*.erl" | sed -e 's/.erl$//' -e "s=\(.*\)/\([^\/]*\)=      \"\2\": \"https://github.com/$ORG/$REPO/blob/$CSUM/\1/\2.erl\",="
echo ''

for DIR in `ls -1 deps`
    do
    if [ -d deps/$DIR ]
    then
      cd deps/$DIR
      echo ''
      CSUM=`git log | head -1 | sed -e 's/.* //'`
      URL=`git remote -v | grep fetch | sed -e 's/.*github.com:/github.com\//' -e 's=.*github.com=github.com=g' -e 's/ .*$//' -e 's/.git$//'`
      find src -name "*.erl" | sed -e 's/.erl$//' -e "s=\(.*\)/\([^\/]*\)=      \"\2\":\"https://$URL/blob/$CSUM/\1/\2.erl\",="
      cd ../..
    fi
done

echo '      "is_done": "1"'
echo '   }'
echo '}'
