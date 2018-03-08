#!/usr/bin/env bash
set -e -x

version=$1

if [[ -z $1 ]]; then
  version=$(grep "version.\?=" ../ui/default.nix | sed -e 's/.*"\(.*\)";/\1/')
fi


function buildMain() {
  elm make --output "build/$2.$version.js" "src/$1.elm"
}

mkdir -p build

buildMain "UI" "elm"
buildMain "Admin" "elm-admin"
