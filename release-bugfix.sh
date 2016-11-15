#!/bin/bash
#
# Copyright (c) 2016 org.NLP4L
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#

if [ -z $1 ] || [ -z $2 ] || [ -z $3 ]; then
  echo "Usage: ./release-bugfix.sh <this_branch> <this_release> <next_release>"
  echo "  ex) ./release-bugfix.sh 1.1 1.1.1 1.1.2"
  exit 1
fi

# e.g. 1.1.1, 1.1.2
THIS_BRANCH_NUM=$1
THIS_REL_NUM=$2
NEXT_REL_NUM=$3
#echo "THIS_REL_NUM : $THIS_REL_NUM"
#echo "NEXT_REL_NUM : $NEXT_REL_NUM"

echo -n "Have you executed ant test and has it successfully done? (y/n) "
read ANS
if [ $ANS != "y" ]; then
  exit 1
fi

DATE_Y=$(date +"%Y")
DATE_M=$(date +"%m")
DATE_D=$(date +"%d")
THIS_REL_DATE="${DATE_Y}-${DATE_M}-${DATE_D}"

# make sure on the target release branch
git checkout "rel-${THIS_BRANCH_NUM}"

# set the proper release date
sed -e s/YYYY-MM-DD/$THIS_REL_DATE/ CHANGES.txt > CHANGES.txt.temp
mv CHANGES.txt.temp CHANGES.txt

# commit the modification
git add .
git commit -m "prepare rel-${THIS_REL_NUM}"
git push --set-upstream origin "rel-${THIS_BRANCH_NUM}"

# tag the commit point to rel-${THIS_REL_NUM}
git tag -a "rel-${THIS_REL_NUM}" -m "release tag for ${THIS_REL_NUM}"
git push origin "rel-${THIS_REL_NUM}"

# prepare the next bug fix release on the release branch
git checkout "rel-${THIS_BRANCH_NUM}"
sed -e "1d" CHANGES.txt > CHANGES.txt.temp
cat<<EOF> CHANGES.txt
NLP4L/framework change history

========== ${NEXT_REL_NUM} / YYYY-MM-DD ===================================

Important Notice

New Features & Improvements

Bug Fixes

API Changes

Javadoc Fixes

Project environments

Tests

Deprecated/Deleted Features

Others

EOF
cat CHANGES.txt.temp >> CHANGES.txt
rm CHANGES.txt.temp
sed -e s/$THIS_REL_NUM/$NEXT_REL_NUM/ version.properties > version.properties.temp
mv version.properties.temp version.properties
git add .
git commit -m "prepare the next release ${NEXT_REL_NUM}"
git push

echo -e "\\n\\n\\nThe rel-${THIS_REL_NUM} has been almost prepared. Please execute the following to finalize.\\n"
echo "1. git checkout rel-${THIS_REL_NUM}"
echo "2. build nlp4l-framework-${THIS_REL_NUM}.zip file by executing activator dist."
echo "3. Go to https://github.com/NLP4L/framework/releases/tag/rel-${THIS_REL_NUM}"
echo "4. click [Edit tag] button and drop down the zip file to the drop down box and click [Publish release]"
echo "5. git checkout master"
echo -e "\\nThe download link will be  https://github.com/NLP4L/framework/releases/download/rel-${THIS_REL_NUM}/nlp4l-framework-${THIS_REL_NUM}.zip"
