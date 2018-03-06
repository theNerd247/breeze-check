#!/usr/bin/env bash
set -e -x

if [[ -z $1 ]]; then
  echo "You need a version string to build the elm app!"
  exit 1
fi

version=$1

function buildMain() {
  cat "$1.elm" > Tmp.elm
  sed -i -e 's/module .* exposing (..)/module Main exposing (..)/' Tmp.elm
  elm make --output $2.$version.js Tmp.elm
}

buildMain "UI" "elm"
buildMain "Admin" "elm-admin"
