#!/usr/bin/env bash
set -e -x

function buildMain() {
  cat "$1.elm" > Tmp.elm
  sed -i -e 's/module .* exposing (..)/module Main exposing (..)/' Tmp.elm
  elm make --output $2.js Tmp.elm
}

buildMain "UI" "elm"
buildMain "Admin" "elm-admin"
