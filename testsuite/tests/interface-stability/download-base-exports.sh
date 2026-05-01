#!/usr/bin/env bash

# See the README file in this folder for usage

jobIDs=("$@")

BASE_DIR_NAME=download-base-exports
DL_DIR_NAME=dl
BASE_DIR="$(dirname "$0")/$BASE_DIR_NAME"
DL_DIR=$BASE_DIR/$DL_DIR_NAME
URL_FILE="$BASE_DIR/url-unexpected-test-output"

DEFAULT_PREFIX="https://gitlab.haskell.org/ghc/ghc/-/jobs/"
DEFAULT_POSTFIX="/artifacts/raw/unexpected-test-output.tar.gz"

mkdir -p "$BASE_DIR"

# URL configuration for finding unexpected-test-output.tar.gz

if [[ ! -f "$URL_FILE" ]]; then
    echo "No URL for unexpected-test-output.tar.gz was found"

    read -p "Enter job URL prefix [${DEFAULT_PREFIX}]: " inputPrefix
    read -p "Enter job URL postfix [${DEFAULT_POSTFIX}]: " inputPostfix

    urlPrefix="${inputPrefix:-$DEFAULT_PREFIX}"
    urlPostfix="${inputPostfix:-$DEFAULT_POSTFIX}"

    {
        echo "urlPrefix=$urlPrefix"
        echo "urlPostfix=$urlPostfix"
    } > "$URL_FILE"
else
    source "$URL_FILE"
fi

mkdir -p $DL_DIR

echo "urlPrefix: $urlPrefix"
echo "jobIDs: $jobIDs"
echo "urlPostfix: $urlPostfix"
echo ""
echo "Downloading unexpected-test-output.tar.gz for each job ..."

# Download and copy base-exports*  files

for jobID  in "${jobIDs[@]}"; do
  unexpectedOutputUrl="$urlPrefix$jobID$urlPostfix"

  wget -O "$DL_DIR/job$jobID.tar.gz" $unexpectedOutputUrl

  mkdir -p "$DL_DIR/job$jobID"
  tar -xzf "$DL_DIR/job$jobID.tar.gz" -C "$DL_DIR/job$jobID"
  cp "$DL_DIR/job$jobID"/unexpected-test-output/testsuite/tests/interface-stability/base-exports* "$BASE_DIR/.."
done
