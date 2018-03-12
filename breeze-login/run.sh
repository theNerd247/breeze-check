#!/usr/bin/env bash
cabal run makeelm

# fetch the app version from the ui default.nix
v=$(grep "version.\?=" ../ui/default.nix | sed -e 's/.*"\(.*\)";/\1/')

# build the app
cd ../ui/
./build.sh "$v"
cd ../breeze-login

rm -r ./js
ln -s ../ui/build ./js


echo "running ui version: $v"
#ln "../ui/elm.$v.js" "./js/elm.$v.js"
#ln "../ui/elm-admin.$v.js" "./js/elm-admin.$v.js"

# replace the version number in the template file so our web app points to the
# right place
cmd="s/elmVersion\">.*<\\/bind>/elmVersion\">$v<\\/bind>/"
sed -i -e "$cmd" ./snaplets/heist/templates/importElm.tpl
sed -i -e "$cmd" ./snaplets/heist/templates/importAdminElm.tpl

cabal run breeze-login -- -p 8080
