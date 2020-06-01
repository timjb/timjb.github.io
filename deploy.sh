#!/bin/bash

# configuration
TARGET_BRANCH=master
if which timbaumann; then
  SITE=timbaumann
elif [ -e ./binaries/timbaumann ]; then
  SITE=./binaries/timbaumann
else
  echo "executable 'timbaumann' not found"
  exit 1
fi

cd $(dirname "$0")
DEFAULT_ORIGIN="git@github.com:timjb/timjb.github.io.git"
DEFAULT_TARGET_BRANCH="master"

if [ "$1" == "-h" -o "$1" == "--help" -o $# -gt 1 ]; then
  echo "Usage: ./deploy.sh [REMOTE-REPO-URL] [TARGET-BRANCH]"
  echo ""
  echo "By default, REMOTE-REPO-URL is '$DEFAULT_ORIGIN',"
  echo "and TARGET-BRANCH is '$DEFAULT_TARGET_BRANCH'."
  exit 0
fi

ORIGIN=${1:-$DEFAULT_ORIGIN}
TARGET_BRANCH=${2:-$DEFAULT_TARGET_BRANCH}

BUILD_DIR=$(mktemp -d builddir-XXXX)

function onerr() {
  rm -rf "$BUILD_DIR"
  exit 1
}
trap onerr ERR

git clone -b "$TARGET_BRANCH" --single-branch "$ORIGIN" "$BUILD_DIR"

$SITE build
cp -R ./_site/* "$BUILD_DIR"

cd "$BUILD_DIR"
git add --all
if git commit -m "Updated website output $(date '+%m/%d/%y %H:%M')"; then
  git push "$ORIGIN" "$TARGET_BRANCH"
else
  echo "No changes to generated website."
fi

rm -rf "$BUILD_DIR"