#!/usr/bin/env bash
set -e -x

if [[ -z $1 ]]; then
  echo "You need a version string to build the elm app!"
  exit 1
fi

version=$1

function buildMain() {
  cat "src/$1.elm" > "build/Tmp.elm"
  sed -i -e 's/module .* exposing (..)/module Main exposing (..)/' "build/Tmp.elm"
  elm make --output "build/$2.$version.js" "build/Tmp.elm"
}

mkdir -p build

buildMain "UI" "elm"
buildMain "Admin" "elm-admin"
