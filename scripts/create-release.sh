#!/usr/bin/env bash
##
## This file is part of cq2rdf
##
## cq2rdf is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## cq2rdf is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with cq2rdf.  If not, see <https://www.gnu.org/licenses/>.
##

RELEASE_NAME="$1"
TAG_NAME="$2"
PROJECT_ID="$3"
# DESCRIPTION_FILE_PATH="$4"
DESCRIPTION="$4"
PRIVATE_TOKEN="$5"
ASSET_DIR="$6"

if [ "$6" == "" ]; then
    echo "Missing parameter! Parameters are RELEASE_NAME, TAG_NAME, PROJECT_ID, DESCRIPTION, PRIVATE_TOKEN and ASSET_DIR.";
    exit 1;
fi

# DESCRIPTION=''

# # Load data from file
# while read -r line; do
#     DESCRIPTION="${DESCRIPTION}${line}\n";
# done < "${DESCRIPTION_FILE_PATH}"

res=$(curl -s --header "PRIVATE-TOKEN: ${PRIVATE_TOKEN}" "https://gitlab.com/api/v4/projects/${PROJECT_ID}/releases/${TAG_NAME}")

if [[ ${res} == *"404 Project Not Found"* ]]; then
    echo "Project ID ${PROJECT_ID} not found!";
    exit 1;
elif [[ ${res} != *"403 Forbidden"* ]]; then
    echo "Release tag ${TAG_NAME} already exists!";
    exit 1;
fi

# Create a release
res=$(curl -i -s\
     --request POST\
     --header 'Content-Type: application/json'\
     --header "Private-Token: ${PRIVATE_TOKEN}"\
     --data-binary "{\"name\": \"${RELEASE_NAME}\", \"tag_name\":
     \"${TAG_NAME}\", \"description\": \"${DESCRIPTION}\", \"ref\": \"master\"}"\
     "https://gitlab.com/api/v4/projects/${PROJECT_ID}/releases")

if [[ ${res} != *"HTTP/2 201"* ]]; then
    echo "Error: $(grep {*} <<< ${res})";
    exit 1;
fi

# Upload asset
for filename in ${ASSET_DIR}/*; do
  res=$(curl -is \
          --request POST \
          --header "PRIVATE-TOKEN: ${PRIVATE_TOKEN}" \
          --form "file=@${filename}" \
          "https://gitlab.com/api/v4/projects/${PROJECT_ID}/uploads")

  full_path=$(grep {*} <<< ${res} | jq '.full_path' | sed "s/\"\(.*\)\"/\1/")
  alt=$(grep {*} <<< ${res} | jq '.alt' | sed "s/\"\(.*\)\"/\1/")

  if [[ ${res} != *"HTTP/2 201"* ]]; then
      echo "Error: $(grep {*} <<< ${res})";
      exit 1;
  fi

  # Link asset to release
  curl -s \
       -o /dev/null \
       --request POST \
       --header "PRIVATE-TOKEN: ${PRIVATE_TOKEN}" \
       --data name="${alt}" \
       --data url="http://gitlab.com${full_path}" \
       "https://gitlab.com/api/v4/projects/${PROJECT_ID}/releases/${TAG_NAME}/assets/links"

done
echo

