#!/usr/bin/env bash
set -e -x

if [[ -z $1 ]]; then
  echo "You need a version string to build the elm app!"
  exit 1
fi

version=$1

function buildMain() {
  elm make --output "build/$2.$version.js" "src/$1.elm"
}

mkdir -p build

buildMain "UI" "elm"
buildMain "Admin" "elm-admin"
